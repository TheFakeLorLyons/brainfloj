(ns floj.api
  (:require [floj.brainflow.board-shim :as brainflow]
            [floj.brainflow.board-ids :as id]
            [floj.brainflow.brainflow-input-params :as params]
            [floj.state :as state]
            [floj.profiles :as profiles]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str])
  (:import  [brainflow BoardIds]))

(defn get-current-sample-rate
  []
  (brainflow/get-sampling-rate (brainflow/get-board-id @state/shim)))
(defn get-current-channels
  []
  (brainflow/get-channel-data :eeg @state/shim))
#_(defn get-current-board-id
    []
    (brainflow/get-board-id (brainflow/get-board-id @state/shim)))


(def CURRENT_SRATE (get-current-sample-rate))
(def CURRENT_CHANNELS (get-current-channels))
(def CURRENT_CHANNEL_COUNT (count (get-current-channels)))
#_(def CURRENT_BOARD_ID (get-current-board-id))

(defn compare-and-select-profile-name []
  (let [active-profile-name ((:get-active-profile @state/state))]
    (if (= active-profile-name "default")
      active-profile-name
      (do
        (println "You don't have a custom profile configured.")
        (loop []
          (print "Please enter a unique profile name: ")
          (flush)
          (let [input (read-line)]
            (if (str/blank? input)
              (do
                (println "Profile name cannot be blank.")
                (recur))
              (do
                (profiles/create-profile! input true)
                input))))))))

(defn get-board-info
  "Get detailed information about the currently connected board"
  []
  (let [shim @state/shim]
    (when shim
      (try
        (let [board-id (brainflow/get-board-id shim)
              board-type (get id/board-types board-id "Unknown Board")
              channels CURRENT_CHANNELS
              num-channels CURRENT_CHANNEL_COUNT
              accel-channels (brainflow/get-channel-data :accel board-id)
              gyro-channels (brainflow/get-channel-data :gyro board-id)
              sampling-rate CURRENT_SRATE
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
          ; Release current board if exists
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

          (println "Creating board with parameters:")
          (println "  - Board ID:" board-id)
          (println "  - MAC Address:" (.get_mac_address params))
          (println "  - Serial Port:" (.get_serial_port params))
          (println "  - Other Info:" (.get_other_info params))

          ; Create new board directly
          (println "Creating new board connection...")
          (try
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

      (switch-board! board-id params)
      (println "Successfully connected to board!")
      true)
    (catch Exception e
      (println "Failed to create board connection:" (.getMessage e))
      false)))

(defn update-profile-bci-device!
  "Update the BCI device settings in a profile"
  [profile-name {:keys [device-type board-id mac-address com-port connection-method]}]
  (let [profile-path (profiles/get-latest-profile-path profile-name)]
    (when (.exists (io/file profile-path))
      (let [profile (edn/read-string (slurp profile-path))
            updated-profile (assoc profile :bci-device {:device-type device-type
                                                        :board-id board-id
                                                        :mac-address mac-address
                                                        :com-port com-port
                                                        :connection-method connection-method})]
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
  (flush)
  (let [profile-name (compare-and-select-profile-name)]
    (print-board-type-options)
    (print "Enter the number associated with your board in the list: ")
    (flush)
    (let [board-id (Integer/parseInt (read-line))]
      (print "Enter MAC address (e.g., XX:XX:XX:XX:XX:XX): ")
      (flush)
      (let [mac-address (read-line)]
        (print "Enter COM port (if applicable, e.g., COM3 or /dev/ttyUSB0): ")
        (flush)
        (let [name profile-name
              com-port (read-line)
              config {:device-type (id/board-types board-id)
                      :board-id board-id
                      :mac-address mac-address
                      :com-port com-port
                      :connection-method "bled112"}]
          (println "\nSaving BCI device configuration...")

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
  (let [device-config (:bci-device profile)]
    (and device-config
         (map? device-config)
         (:board-id device-config)  ;; Board ID is required for connection
         (not (nil? (:mac-address device-config)))
         (not (nil? (:com-port device-config)))
         (or (seq (:mac-address device-config))
             (seq (:com-port device-config))))))

(defn connect-from-profile!
  "Connect to the BCI device using the settings from the profile"
  [profile]
  (when (profile-has-bci-device? profile)
    (let [device-config (:bci-device profile)
          board-type (:device-type device-config)
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
      (try
        (connect! mac-address com-port :board-id board-id)
        (println "Successfully connected to BCI device from profile settings")
        true
        (catch Exception e
          (println "No BCI device configured in this profile")
          false)))))

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

(defn initialize-brainflow! []
  (state/register-fn! :release-board!    brainflow/release-session!)
  (state/register-fn! :get-sampling-rate brainflow/get-sampling-rate)
  (state/register-fn! :get-eeg-channels  brainflow/get-channel-data)
  (state/register-fn! :get-board-info    get-board-info)
  (state/register-fn! :register-device   configure-bci-device!))