(ns floj.api
  (:require [floj.brainflow.board-shim :as brainflow]
            [floj.brainflow.board-ids :as id]
            [floj.brainflow.brainflow-input-params :as params]
            [floj.io :as fio]
            [floj.state :as state]
            [floj.profiles :as profiles]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import  [brainflow
             BrainFlowInputParams BoardIds DataFilter
             WindowOperations FilterTypes]))

(defn get-board-info
  "Get detailed information about the currently connected board"
  []
  (let [shim @state/shim]
    (when shim
      (try
        (let [board-id (brainflow/get-board-id shim)
              board-type (get id/board-types board-id "Unknown Board")
              channels (brainflow/get-channel-data :eeg board-id)
              num-channels (count channels)
              accel-channels (brainflow/get-channel-data :accel board-id)
              gyro-channels (brainflow/get-channel-data :gyro board-id)
              sampling-rate (brainflow/get-sampling-rate board-id)
              is-prepared (brainflow/board-ready? shim)
              is-recording @state/recording?]
          {:board-id board-id
           :board-type board-type
           :eeg-channels channels
           :num-channels num-channels
           :accel-channels accel-channels
           :gyro-channels gyro-channels
           :sampling-rate sampling-rate
           :is-prepared is-prepared
           :is-recording is-recording})
        (catch Exception e
          {:error (.getMessage e)})))))

(defn switch-board!
  "Debug version of switch-board! with more logging"
  [board-id params]
  (try
    (let [current-board @state/shim]
      (if (and current-board (= (brainflow/get-board-id current-board) board-id))
        (do
          (println "Already connected to the requested board type:" (get id/board-types board-id "Unknown Board"))
          current-board)
        (do
          (println "Switching to" (get id/board-types board-id "Unknown Board") "...")
          ;; Release current board if exists
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

          ;; Debug params
          (println "Creating board with parameters:")
          (println "  - Board ID:" board-id)
          (println "  - MAC Address:" (.get_mac_address params))
          (println "  - Serial Port:" (.get_serial_port params))
          (println "  - Other Info:" (.get_other_info params))

          ;; Create new board directly
          (println "Creating new board connection...")
          (try
            ;; Try creating the board directly without using the wrapper
            (let [new-board-shim (brainflow.BoardShim. board-id params)]
              (println "Board created, preparing session...")
              (try
                (.prepare_session new-board-shim)
                (reset! state/shim new-board-shim)
                (println "Successfully switched to new board:" (id/board-types board-id))
                new-board-shim
                (catch Exception e
                  (println "Error in prepare_session:" (.getMessage e))
                  (.printStackTrace e)))
              nil)
            (catch Exception e
              (println "Error creating BoardShim:" (.getMessage e))
              (.printStackTrace e))))))
    (catch Exception e
      (println "Failed to switch board:" (.getMessage e))
      (.printStackTrace e)))
    nil)

(defn connect!
  [mac-address com-port & {:keys [board-id] :or {board-id BoardIds/GANGLION_BOARD}}]
  (println "Connecting to board with ID:" board-id)
  (try
    (let [params (params/create-brainflow-input-params
                  :mac-address mac-address
                  :serial-port com-port
                  :other-info "bled112")]
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

(defn get-psd
  "Calculate Power Spectral Density (PSD) from data"
  [data sampling-rate]
  ;; Use the entire data array (start at 0, end at length)
  (DataFilter/get_psd data 0 (count data) sampling-rate WindowOperations/HANNING))

(defn get-band-power
  "Calculate band power for a specific frequency range using PSD"
  [data sampling-rate start-freq stop-freq]
  (let [psd (get-psd data sampling-rate)]
    (DataFilter/get_band_power psd start-freq stop-freq)))

(defn initialize-brainflow! []
  (state/register-fn! :release-board!    brainflow/release-session!)
  (state/register-fn! :get-sampling-rate brainflow/get-sampling-rate)
  (state/register-fn! :get-eeg-channels  brainflow/get-channel-data)
  (state/register-fn! :get-board-info    get-board-info))
