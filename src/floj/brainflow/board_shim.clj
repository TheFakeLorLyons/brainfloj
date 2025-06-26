(ns floj.brainflow.board-shim
  "This does not cover the entire BoardShim.java file, but it covers many of the most
   important functions for basic development. In the future I plan to wrap all of the functions,
   and it is still possible to call the native java wherever you would like in order to specify
   custom params or call functions not covered here (such as temperature channels for example)."
  (:require [floj.brainflow.brainflow-input-params :as params])
  (:import [brainflow BoardShim BrainFlowPresets]))

(def log-levels {:trace 0 :debug 1 :info 2 :warn 3 :error 4 :off 5})
(def brainflow-presets {:default BrainFlowPresets/DEFAULT_PRESET
                         :auxiliary BrainFlowPresets/AUXILIARY_PRESET
                         :ancillary BrainFlowPresets/ANCILLARY_PRESET})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                          ;            Logging functions    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn enable-dev-logger!
  "Enable developer logger for debugging"
  []
  (BoardShim/enable_dev_board_logger))

(defn set-log-level!
  "Set the log level for the BrainFlow logger"
  [level]
  (let [log-level (if (keyword? level) (get log-levels level) level)]
    (BoardShim/set_log_level log-level)))

(defn set-log-file!
  "Set log file for BrainFlow logger"
  [file-path]
  (BoardShim/set_log_file file-path))

(defn log-message!
  "Log a message with the specified log level"
  [level message]
  (let [log-level (if (keyword? level) (get log-levels level) level)]
    (BoardShim/log_message log-level message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                          ;          Recording functions    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Constructor
(defn create-board-shim
  "Create a new BoardShim instance using custom BrainFlowInputParams"
  [board-id & {:keys [serial-port mac-address ip-address ip-port ip-protocol other-info timeout]
               :or {serial-port ""
                    mac-address ""
                    ip-address ""
                    ip-port 0
                    ip-protocol 0
                    other-info ""
                    timeout 0}}]
  (let [input-params (params/create-brainflow-input-params
                      :serial-port serial-port
                      :mac-address mac-address
                      :ip-address ip-address
                      :ip-port ip-port
                      :ip-protocol ip-protocol
                      :other-info other-info
                      :timeout timeout)]
    (brainflow.BoardShim. board-id input-params)))

(defn config-board!
  "Configure the board with string config"
  [board-shim config]
  (.config_board board-shim config))

(defn config-board-with-bytes!
  "Configure the board with bytes"
  [board-shim bytes]
  (.config_board_with_bytes board-shim bytes))

(defn initialize-board
  "Initialize a board with the given options using BrainFlowInputParams."
  [board-id & {:as options}]
  (let [params (apply params/create-brainflow-input-params (flatten (seq options)))]
    (BoardShim. board-id params)))

(defn prepare-session!
  "Prepare and open a board session"
  [board-shim]
  (.prepare_session board-shim))

(defn release-session!
  "Release the board session"
  [board-shim]
  (.release_session board-shim))

(defn start-stream!
  "Start streaming data from the board
   Options:
   - :buffer-size       (default: uses board's default)
   - :streamer-params   (default: empty string)"
  [board-shim & {:keys [buffer-size streamer-params]
                 :or {streamer-params ""}}]
  (cond
    (and buffer-size streamer-params) (.start_stream board-shim buffer-size streamer-params)
    buffer-size (.start_stream board-shim buffer-size)
    :else (.start_stream board-shim)))

(defn stop-stream!
  "Stop streaming data from the board"
  [board-shim]
  (.stop_stream board-shim))

(defn release-all-sessions!
  "Release all board sessions"
  []
  (BoardShim/release_all_sessions))

(defn insert-marker!
  "Insert a marker into the data stream
   Options:
   - :preset - BrainFlow preset to use (default: DEFAULT_PRESET)"
  [board-shim value & {:keys [preset]
                       :or {preset BrainFlowPresets/DEFAULT_PRESET}}]
  (.insert_marker board-shim value preset))

(defn add-streamer!
  "Add a streamer
   Options:
   - :preset - BrainFlow preset to use (default: DEFAULT_PRESET)"
  [board-shim streamer & {:keys [preset]
                          :or {preset BrainFlowPresets/DEFAULT_PRESET}}]
  (.add_streamer board-shim streamer preset))

(defn delete-streamer!
  "Delete a streamer
   Options:
   - :preset - BrainFlow preset to use (default: DEFAULT_PRESET)"
  [board-shim streamer & {:keys [preset]
                          :or {preset BrainFlowPresets/DEFAULT_PRESET}}]
  (.delete_streamer board-shim streamer preset))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                          ;          Board Information      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-version
  "Get BrainFlow BoardController version"
  []
  (BoardShim/get_version))

(defn board-ready?
  "Returns true if the board is prepared (i.e., session is initialized)"
  [board-shim]
  (.is_prepared board-shim))

(defn get-board-id
  [shim]
  (.get_board_id shim))

(defn get-device-name
  "Get device name
   Options:
   - :preset - BrainFlow preset to use (default: DEFAULT_PRESET)"
  [board-id & {:keys [preset]
                 :or {preset BrainFlowPresets/DEFAULT_PRESET}}]
  (BoardShim/get_device_name board-id preset))

(defn get-board-data
  "Get data from the board as a Clojure data structure (Outside of a recording)
   Options:
   - :num-samples (default: 200)
   - :preset      (default: DEFAULT_PRESET)"
  [board-shim & {:keys [num-samples preset]
                 :or {num-samples 200
                      preset BrainFlowPresets/DEFAULT_PRESET}}]
  (let [data (if (pos? num-samples)
               (.get_board_data board-shim num-samples preset)
               (.get_board_data board-shim))]
    (mapv #(vec (seq %)) data)))

(defn get-current-board-data
  "Get current data from the board  (For use during the recording)
   Options:
   - :num-samples - Number of samples to get (default: 200)
   - :preset - BrainFlow preset to use (default: DEFAULT_PRESET)"
  [board-shim & {:keys [num-samples preset]
                 :or {num-samples 200
                      preset BrainFlowPresets/DEFAULT_PRESET}}]
  (let [data (.get_current_board_data board-shim num-samples preset)]
    (mapv #(vec (seq %)) data)))

(defn get-board-data-count
  "Get board data count
   Options:
   - :preset - BrainFlow preset to use (default: DEFAULT_PRESET)"
  [board-shim & {:keys [preset]
                 :or {preset BrainFlowPresets/DEFAULT_PRESET}}]
  (.get_board_data_count board-shim preset))

(defn get-board-description
  "Get board description
   Options:
   - :preset - BrainFlow preset to use (default: DEFAULT_PRESET)"
  [board-shim & {:keys [preset]
                 :or {preset BrainFlowPresets/DEFAULT_PRESET}}]
  (.get_board_descr board-shim preset))

(defn get-board-presets
  "Get board presets"
  [board-shim]
  (vec (.get_board_presets board-shim)))

(defn get-eeg-names
  "Get EEG channel names for the specified board
   Options:
   - :preset - BrainFlow preset to use (default: DEFAULT_PRESET)"
  [board-id & {:keys [preset]
               :or {preset BrainFlowPresets/DEFAULT_PRESET}}]
  (vec (BoardShim/get_eeg_names board-id preset)))

(defn get-num-rows
  "Get number of rows for the specified board
   Options:
   - :preset - BrainFlow preset to use (default: DEFAULT_PRESET)"
  [board-id & {:keys [preset]
               :or {preset BrainFlowPresets/DEFAULT_PRESET}}]
  (BoardShim/get_num_rows board-id preset))

(defn get-sampling-rate
  "Get sampling rate for the specified board
   Options:
   - :preset - BrainFlow preset to use (default: DEFAULT_PRESET)"
  [board-id & {:keys [preset]
               :or {preset BrainFlowPresets/DEFAULT_PRESET}}]
  (BoardShim/get_sampling_rate board-id preset))

(defn get-eeg-channels
  "Get EEG channels for the specified board"
  [board-id]
  (vec (BoardShim/get_eeg_channels board-id)))

(defn get-accel-channels
  "Get accelerometer channels for the specified board"
  [board-id]
  (vec (BoardShim/get_accel_channels board-id)))

(defn get-gyro-channels
  "Get gyroscope channels for the specified board"
  [board-id]
  (vec (BoardShim/get_gyro_channels board-id)))




(defn brainflow-channel-data
  "Call a static BrainFlow method using reflection.
   Supports methods with different argument types (e.g., 1 or 2 args)."
  [method-name args arg-types]
  (try
    (let [method (.getMethod BoardShim method-name (into-array Class arg-types))]
      (.invoke method nil (to-array args)))
    (catch Exception e
      (throw (IllegalArgumentException.
              (str "Failed to call " method-name ": " (.getMessage e)))))))

(def channel-types
  [:eeg :emg :ecg :eog #_:eda #_:ppg :exg
   #_:temperature #_:resistance #_:other
   :accel #_:analog #_:gyro #_:magnetometer
   #_:package-num #_:battery :timestamp :marker])

(def channel-method-overrides
  {:timestamp "get_timestamp_channel"
   :marker "get_marker_channel"})

(defn get-channel-data
   [method-key board-or-shim & {:keys [preset as method-name]
                                :or {preset BrainFlowPresets/DEFAULT_PRESET
                                     as :vec}}]
   (let [board-id (if (instance? BoardShim board-or-shim)
                    (get-board-id board-or-shim)
                    (int board-or-shim))
         [java-method-name arg-types args] (cond
                                             (#{"get_timestamp_channel" "get_marker_channel"} method-name)
                                             [method-name [Integer/TYPE] [board-id]]
 
                                             :else
                                             [(or method-name
                                                  (str "get_" (name method-key) "_channels"))
                                              [Integer/TYPE BrainFlowPresets]
                                              [board-id preset]])
         result (brainflow-channel-data java-method-name args arg-types)]
     (case as
       :array result
       :vec   (if (integer? result) [result] (vec result))
       (throw (ex-info "Invalid :as option"
                       {:valid [:vec :array] :given as})))))

(defmacro defchannel-getters []
  `(do
     ~@(for [k channel-types
             :let [method-name (get channel-method-overrides k
                                    (str "get_" (name k) "_channels"))]]
         `(defn ~(symbol (str "get-" (name k) "-channels"))
            ~(str "Get " (name k) " channels for the specified board or shim")
            [board-or-shim# & opts#]
            (apply get-channel-data ~k board-or-shim# :method-name ~method-name opts#)))))

(defchannel-getters)

;Get patterns
;(get-channel-data :eeg 1)
;(get-channel-data :eeg 1 :as :array)
;(get-channel-data :eeg (create-board-shim 1))
;(get-channel-data :eeg (create-board-shim 1) :as :array)