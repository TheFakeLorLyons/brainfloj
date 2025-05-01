(ns floj.api
  (:require [floj.brainflow.boardshim :as shim]
            [floj.brainflow.boardids :as id]
            [floj.io :as fio]
            [floj.state :as state]
            [floj.profiles :as profiles]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [mount.core :as mount])
  (:import [brainflow BrainFlowInputParams 
                      BoardIds DataFilter DataFilter 
                      FilterTypes]
           [org.apache.commons.lang3.tuple Pair]))

 (defn determine-board [args]
   (reset! state/board-id-atom
     (cond
       (and (seq args) (= (first args) "synthetic"))
       BoardIds/SYNTHETIC_BOARD
       (and (seq args) (= (first args) "ganglion"))
       BoardIds/GANGLION_BOARD
       (and (seq args) (= (first args) "cyton"))
       BoardIds/CYTON_BOARD
       :else
       BoardIds/SYNTHETIC_BOARD)))

(defn get-board-info
  "Get detailed information about the currently connected board"
  []
  (let [shim @state/shim]
    (when shim
      (try
        (let [board-id (shim/get-board-id shim)
              board-type (get id/board-types board-id "Unknown Board")
              channels (shim/get-eeg-channels board-id)
              num-channels (count channels)
              accel-channels (shim/get-accel-channels board-id)
              #_#_gyro-channels (get-gyro-channels board-id)
              sampling-rate (shim/get-sampling-rate board-id)
              is-prepared (shim/board-ready? shim)
              is-recording @state/recording?]
          {:board-id board-id
           :board-type board-type
           :eeg-channels channels
           :num-channels num-channels
           :accel-channels accel-channels
           #_#_:gyro-channels gyro-channels
           :sampling-rate sampling-rate
           :is-prepared is-prepared
           :is-recording is-recording})
        (catch Exception e
          {:error (.getMessage e)})))))

(defn switch-board!
  "Switch from current board to a new board with the given ID and parameters"
  [board-id params]
  (try
    (let [current-board @state/shim]
      (if (and current-board (= (shim/get-board-id current-board) board-id))
        (do
          (println "Already connected to the requested board type:" (get id/board-types board-id "Unknown Board"))
          current-board)
        (do
          (println "Switching to" (get id/board-types board-id "Unknown Board") "...")
          (when current-board
            (println "Releasing current board...")
            (try
              (when @state/recording?
                (println "Stopping current recording first...")
                (.stop_stream current-board)
                (reset! state/recording? false))
              (.release_session current-board)
              (catch Exception e
                (println "Warning during board cleanup:" (.getMessage e)))))
          (println "Creating new board connection for board ID:" board-id)
          (let [new-board-shim (shim/naive-board-shim-constructor board-id params)]
            (.prepare_session new-board-shim)
            (reset! state/shim new-board-shim)
            (println "Successfully switched to new board:" (id/board-types board-id))
            new-board-shim))))
    (catch Exception e
      (println "Failed to switch board:" (.getMessage e))
      nil)))

(defn connect!
  [mac-address com-port & {:keys [board-id] :or {board-id BoardIds/GANGLION_BOARD}}]
  (println "Connecting to board with ID:" board-id)
  (try
    (let [params (doto (BrainFlowInputParams.)
                   (.set_mac_address mac-address)
                   (.set_serial_port com-port)
                   (.set_other_info "bled112"))]
      (switch-board! board-id params))
    (catch Exception e
      (println "Failed to create board connection:" (.getMessage e))
      nil)))

(defn update-profile-bci-device!
  "Update the BCI device settings in a profile"
  [profile-name {:keys [device-type board-id mac-address com-port]}]
  (let [profile-path (str (fio/config-base-dir) "/profiles/" profile-name ".edn")]
    (when (.exists (io/file profile-path))
      (let [profile (edn/read-string (slurp profile-path))
            updated-profile (assoc profile :bci-device
                              {:device-type device-type
                               :board-id board-id
                               :mac-address mac-address
                               :com-port com-port})]
        (spit profile-path (pr-str updated-profile))
        updated-profile))))

(defn print-board-type-options
  "Prints available board types in rows for selection"
  []
  (println "\nAvailable BCI Boards:")
  (let [sorted (sort-by key id/board-types)
        rows (partition-all 4 sorted)]
    (doseq [row rows]
      (println (apply str
                 (map (fn [[id name]]
                        (format "%3d: %-30s" id name))
                   row))))))

(defn configure-bci-device!
  "Interactive prompt to configure BCI device settings"
  []
  (println "\n--- BCI Device Configuration ---")
  (print "Enter a name for your profile: ")
  (flush)
    (let [name (read-line)]
      (print-board-type-options)
      (print "Enter the number associated with your board in the list: ")
      (flush)
      (let [board-id (Integer/parseInt (read-line))]
        (print "Enter MAC address (e.g., XX:XX:XX:XX:XX:XX): ")
        (flush)
        (let [mac-address (read-line)]
          (print "Enter COM port (if applicable, e.g., COM3 or /dev/ttyUSB0): ")
          (flush)
          (let [com-port (read-line)
                config {:board-type (id/board-types board-id)
                        :board-id board-id
                        :mac-address mac-address
                        :com-port com-port
                        :connection-method "bled112"}]
            (println "\nSaving BCI device configuration...")
            (profiles/create-profile! name)
            (update-profile-bci-device! name config)
            (println "Configuration saved to profile:" name)
            (println "Would you like to connect to that device now?")
            (flush)
            (let [connect? (read-line)]
              (when connect?
                (connect! mac-address com-port :board-id board-id))))))))

(defn profile-has-bci-device?
  "Check if the profile has configured BCI device settings"
  [profile]
  (and
    (not-empty (:mac-address (:bci-device profile)))
    (not-empty (:com-port (:bci-device profile)))))

(defn connect-from-profile!
  "Connect to the BCI device using the settings from the profile"
  [profile]
  (if (profile-has-bci-device? profile)
    (let [device-config (:bci-device profile)
          board-type (:board-type device-config)
          board-id (:board-id device-config)
          mac-address (:mac-address device-config)
          com-port (:com-port device-config)
          connection-method (:connection-method device-config)]

      (println "Connecting to BCI device from profile settings:")
      (println "  Board Type:" board-type)
      (println "  Board ID:" board-id)
      (println "  MAC address:" mac-address)
      (println "  COM port:" (if (empty? com-port) "[None]" com-port))
      (println "  Connection method:" connection-method)

      (let [result (connect! mac-address com-port :board-id board-id)]
        (if result
          (println "Successfully connected to BCI device from profile settings")
          (println "Failed to connect using profile settings"))))
    (println "No BCI device configured in this profile")))

(defn connect-to-default-device
  [profile]
  (if (profile-has-bci-device? profile)
    (do
      (println "\nYou have a BCI device configured in your profile.")
      (print "Would you like to connect to it? (y/n): ")
      (flush)
      (let [response (read-line)]
        (when (or (= response "y") (= response "Y"))
          (connect-from-profile! profile))))
    (do
      (println "\nYou don't have a BCI device configured in your profile.")
      (print "Would you like to configure one now? (y/n): ")
      (flush)
      (let [response (read-line)]
        (when (or (= response "y") (= response "Y"))
          (configure-bci-device!))))))

(def filter-type-map
  {:butterworth FilterTypes/BUTTERWORTH
   :chebyshev FilterTypes/CHEBYSHEV_TYPE_1})

(defn filter-data!
  "Applies an in-place EEG filter using BrainFlow’s static methods.
   Accepts double[] arrays. Mutates them!
   filter-type: one of :lowpass, :highpass, :bandpass, :bandstop
   filter-shape: one of :butterworth or :chebyshev"
  [data
   sampling-rate
   start-freq
   end-freq
   order
   filter-type
   & {:keys [filter-shape ripple]
      :or {filter-shape :butterworth ripple 0.0}}]
  (let [shape (get filter-type-map filter-shape)]
    (case filter-type
      :lowpass
      (DataFilter/perform_lowpass data sampling-rate end-freq order shape ripple)

      :highpass
      (DataFilter/perform_highpass data sampling-rate start-freq order shape ripple)

      :bandpass
      (DataFilter/perform_bandpass data sampling-rate start-freq end-freq order shape ripple)

      :bandstop
      (DataFilter/perform_bandstop data sampling-rate start-freq end-freq order shape ripple)

      (throw (IllegalArgumentException.
              (str "Unsupported filter type: " filter-type))))))

(defn get-avg-band-powers
  "Compute average band powers and standard deviations from EEG data.
   `data` is a 2D double array (rows = channels).
   `channels` is a vector of channel indices (e.g., [0 1 2 3]).
   `sampling-rate` is Hz.
   Returns a map with :avg and :stddev keys pointing to vectors of doubles."
  [^doubles [] data channels sampling-rate apply-filters?]
  (let [chan-array (int-array channels)
        ^Pair result (DataFilter/get_avg_band_powers data chan-array sampling-rate apply-filters?)]
    {:avg    (vec (.get_left result))
     :stddev (vec (.get_right result))}))

(defn initialize-brainflow! []
  (state/register-fn! :release-board!    shim/release-session!)
  (state/register-fn! :get-sampling-rate shim/get-sampling-rate)
  (state/register-fn! :get-eeg-channels  shim/get-eeg-channels)
  (state/register-fn! :get-board-info    get-board-info))
