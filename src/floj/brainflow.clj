(ns floj.brainflow
  (:require [floj.io :as fio]
            [floj.state :as state]
            [mount.core :as mount])
  (:import [brainflow BoardShim BrainFlowInputParams 
                      BoardIds DataFilter DataFilter 
                      FilterTypes]
           [org.apache.commons.lang3.tuple Pair]))

(def log-levels {:trace 0 :debug 1 :info 2 :warn 3 :error 4 :off 5})

(def board-types
  {-1 "SYNTHETIC_BOARD"
   -2 "STREAMING_BOARD"
   -3 "PLAYBACK_FILE_BOARD"
    0 "CYTON_BOARD"
    1 "GANGLION_BOARD"
    2 "CYTON_DAISY_BOARD"
    3 "GALEA_BOARD"
    4 "GANGLION_WIFI_BOARD"
    5 "CYTON_DAISY_WIFI"})

(def filter-type-map
  {:butterworth FilterTypes/BUTTERWORTH
   :chebyshev FilterTypes/CHEBYSHEV_TYPE_1})

(defn enable-logger! []
  (let [log-file (str (System/getProperty "user.home")
                   "/.lor/logs/device_logs/brainflow.log")]
    (spit log-file "")
    (.set_log_file_board_controller log-file)
    (.set_log_level_board_controller (log-levels :info))))

(defn init-board-shim
  "Initialize a board with the given parameters"
  [board-id & {:keys [serial-port mac-address ip-address ip-port ip-protocol
                      other-info timeout master-board]
               :or {serial-port "" mac-address "" ip-address ""
                    ip-port 0 ip-protocol 0 other-info ""
                    timeout 0 master-board -1}}]
  (let [params (doto (BrainFlowInputParams.)
                 (.set_serial_port serial-port)
                 (.set_mac_address mac-address)
                 (.set_ip_address ip-address)
                 (.set_ip_port ip-port)
                 (.set_ip_protocol ip-protocol)
                 (.set_other_info other-info)
                 (.set_timeout timeout)
                 (.set_master_board master-board))]
    (BoardShim. board-id params)))

(defn enable-dev-logger!
  "Enable developer logger for debugging"
  []
  (BoardShim/enable_dev_board_logger))

(defn disable-logger!
  "Disable BrainFlow logger"
  []
  (BoardShim/disable_board_logger))

(defn enable-logger!
  "Enable BrainFlow logger with specified level"
  []
  (BoardShim/enable_board_logger ))

(defn set-log-file [path]
  (BoardShim/set_log_file path))

(defn redirect-brainflow-logs! []
  (let [device-log-path (str (fio/config-base-dir) "/logs/device_logs/brainflow.log")]
    (set-log-file device-log-path)
    (enable-logger!)))

(defn prepare-session!
  "Prepare and open a board session"
  [board-shim]
  (.prepare_session board-shim))

(defn initialize-board [board-id]
  (redirect-brainflow-logs!)
  (try
    (let [board-shim (init-board-shim board-id)]
      (prepare-session! board-shim)
      board-shim)
    (catch Exception e
      (println "Failed to initialize board:" (.getMessage e))
      (println "Make sure your board is connected properly")
      nil)))

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

(defn release-session!
  "Release the board session"
  [board-shim]
  (.release_session board-shim))

(defn board-ready?
  "Returns true if the board is prepared (i.e., session is initialized)"
  [board-shim]
  (.is_prepared board-shim))

(defn get-board-data
  "Get data from the board as a Clojure data structure"
  [board-shim & {:keys [num-samples]
                 :or {num-samples 250}}]
  (let [data (if (pos? num-samples)
               (.get_board_data board-shim num-samples)
               (.get_board_data board-shim))]
    (mapv #(vec (seq %)) data)))

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

(defn get-board-id
  [shim]
  (.get_board_id shim))

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

(defn get-board-info
  "Get detailed information about the currently connected board"
  []
  (let [shim @state/shim]
    (when shim
      (try
        (let [board-id (get-board-id shim)
              board-type (get board-types board-id "Unknown Board")
              channels (get-eeg-channels board-id)
              num-channels (count channels)
              accel-channels (get-accel-channels board-id)
              #_#_gyro-channels (get-gyro-channels board-id)
              sampling-rate (get-sampling-rate board-id)
              is-prepared (board-ready? shim)
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
      (if (and current-board (= (get-board-id current-board) board-id))
        (do
          (println "Already connected to the requested board type:" (get board-types board-id "Unknown Board"))
          current-board)
        (do
          (println "Switching to" (get board-types board-id "Unknown Board") "...")
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
          (let [new-board-shim (BoardShim. board-id params)]
            (.prepare_session new-board-shim)
            (reset! state/shim new-board-shim)
            (println "Successfully switched to new board:" (get board-types board-id "Unknown Board"))
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
  (state/register-fn! :init-board! initialize-board)
  (state/register-fn! :release-board!    release-session!)
  (state/register-fn! :get-sampling-rate get-sampling-rate)
  (state/register-fn! :get-eeg-channels  get-eeg-channels)
  (state/register-fn! :get-board-info    get-board-info))
