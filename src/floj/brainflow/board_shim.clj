(ns floj.brainflow.board-shim
  (:require [floj.brainflow.brainflow-input-params :as params])
  (:import [brainflow BoardShim BrainFlowPresets]))

(def log-levels {:trace 0 :debug 1 :info 2 :warn 3 :error 4 :off 5})
(def brain-flow-presets {:default BrainFlowPresets/DEFAULT_PRESET
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

;; Version information
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
  [board-shim & {:keys [preset]
                 :or {preset BrainFlowPresets/DEFAULT_PRESET}}]
  (.get_device_name board-shim preset))

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





(defn brainflow-channel-data
  "Safely get channels using reflection to handle different method signatures"
  [method-name board-id preset]
  (try
    (println "Board ID type: " (class board-id))  ; Print the type of board-id
    (println "Preset type: " (class preset))
    (let [method (.getMethod BoardShim
                             (name method-name)
                             (into-array Class [Integer/TYPE BrainFlowPresets]))]
      (.invoke method nil (to-array [board-id preset])))
    (catch NoSuchMethodException e
      (throw (IllegalArgumentException.
              (str "Failed to call " method-name ": " (.getMessage e)))))))

(def channel-types
  [:eeg :emg :ecg :eog :eda :ppg :exg
   :temperature :resistance :other
   :accel :analog :gyro :magnetometer
   :package-num :battery :timestamp :marker])

(defn get-channel-data
  "Get channels of a specific type from a board or shim
   
   Parameters:
   - method-name: Keyword identifying the channel type (:eeg, :emg, etc.)
   - board-or-shim: BoardShim instance or board ID number
   
   Options:
   - :preset (default: DEFAULT_PRESET)
   - :as (:vec or :array, default: :vec)"
  [method-name board-or-shim & {:keys [preset as]
                                :or {preset BrainFlowPresets/DEFAULT_PRESET
                                     as :vec}}]
  (let [board-id (if (instance? BoardShim board-or-shim)
                   (get-board-id board-or-shim)
                   (int board-or-shim))
        java-method-name (str "get_" (name method-name) "_channels")
        result (brainflow-channel-data java-method-name board-id preset)]
    (case as
      :array result
      :vec   (vec result)
      (throw (ex-info "Invalid :as option"
                      {:valid [:vec :array] :given as})))))

(defmacro defchannel-getters []
  `(do
     ~@(for [k channel-types]
         `(defn ~(symbol (str "get-" (name k) "-channels"))
            ~(str "Get " (name k) " channels for the specified board or shim")
            [board-or-shim# & opts#]
            (apply get-channel-data ~k board-or-shim# opts#)))))

(defchannel-getters)

;Get patterns
;(get-channel-data :eeg 1)
;(get-channel-data :eeg 1 :as :array)
;(get-channel-data :eeg (create-board-shim 1))
;(get-channel-data :eeg (create-board-shim 1) :as :array)