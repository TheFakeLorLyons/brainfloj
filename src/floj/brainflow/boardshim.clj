(ns floj.brainflow.boardshim
  (:import [brainflow BoardShim]))

(def log-levels {:trace 0 :debug 1 :info 2 :warn 3 :error 4 :off 5})

(defn enable-dev-logger!
  "Enable developer logger for debugging"
  []
  (BoardShim/enable_dev_board_logger))

(defn prepare-session!
  "Prepare and open a board session"
  [board-shim]
  (.prepare_session board-shim))

(defn release-session!
  "Release the board session"
  [board-shim]
  (.release_session board-shim))

(defn start-stream!
  "Start streaming data from the board"
  [board-shim & {:keys [buffer-size]}]
  (if buffer-size
    (.start_stream board-shim buffer-size)
    (.start_stream board-shim)))

(defn stop-stream!
  "Stop streaming data from the board"
  [board-shim]
  (.stop_stream board-shim))

(defn board-ready?
  "Returns true if the board is prepared (i.e., session is initialized)"
  [board-shim]
  (.is_prepared board-shim))

(defn get-board-id
  [shim]
  (.get_board_id shim))

(defn get-board-data
  "Get data from the board as a Clojure data structure"
  [board-shim & {:keys [num-samples]
                 :or {num-samples 250}}]
  (let [data (if (pos? num-samples)
               (.get_board_data board-shim num-samples)
               (.get_board_data board-shim))]
    (mapv #(vec (seq %)) data)))

(defn get-sampling-rate
  "Get sampling rate for the specified board"
  [board-id]
  (BoardShim/get_sampling_rate board-id))

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

(defn naive-board-shim-constructor [board-id params]
  (BoardShim. board-id params))