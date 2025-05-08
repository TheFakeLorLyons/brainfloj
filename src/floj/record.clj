(ns floj.record
  (:require [floj.brainflow.board-shim :as brainflow]
            [floj.api :as api]
            [floj.lor :as lor]
            [floj.keybindings :as kb]
            [floj.state :as state])
  (:import [java.awt.event KeyListener KeyEvent]
           [javax.swing JFrame]))

(def last-calibration-update (atom 0))
(def calibration-update-interval 10000)

(defn tag! [label]
  (when @state/recording?
    (let [timestamp (System/currentTimeMillis)]
      (swap! state/tags conj {:timestamp timestamp :label label})
      (println "Tagged:" label "at" timestamp))))


(defn update-calibration-if-needed!
  "Updates the calibration index periodically based on recent data"
  [recent-data]
  (let [current-time (System/currentTimeMillis)]
    (when (and @state/recording-context
               (> current-time (+ @last-calibration-update calibration-update-interval)))
      (try
        (println "Updating calibration...")
        ; 1. Transform recent-data into channel arrays for calibration
        ; recent-data format: [[timestamp ch1 ch2 ch3...] [...]]
        (let [eeg-channels (brainflow/get-channel-data :eeg (brainflow/get-board-id @state/shim))
                channel-data (reduce (fn [channels data-point]
                                       (reduce-kv (fn [acc i ch-idx]
                                                    (update acc i conj (nth data-point (inc ch-idx)))) ; +1 to skip timestamp
                                                  channels
                                                  (vec eeg-channels))
                                     (vec (repeat (count eeg-channels) [])))
                                     recent-data)

              profile-name (or (:name ((:get-active-profile @state/state))) "default")
              profile-calibration (when (fn? (resolve 'refract/load-calibration-profile))
                                    ((resolve 'refract/load-calibration-profile) profile-name))]

          ; Debug info
          (println "Processing calibration with" (count channel-data) "channels,"
                   (if (seq channel-data) (count (first channel-data)) 0) "samples each")

          ; Create calibration index using transformed channel data
          (let [new-calibration-index (lor/create-local-calibration-index
                                       channel-data
                                       profile-calibration)

                updated-metadata (lor/update-metadata-calibration!
                                  (:lorfile-dir @state/recording-context)
                                  (:metadata @state/recording-context)
                                  new-calibration-index)]

            (when updated-metadata ; Update in memory/recording-context
              (swap! state/recording-context assoc :metadata updated-metadata))
            (reset! last-calibration-update current-time)

            (println "Calibration index updated at:" current-time
                     "with factors:" (:calibration-factors new-calibration-index))))
      (catch Exception e
        (println "Error updating calibration:" (.getMessage e))
        (.printStackTrace e))))))

(defn apply-calibration-to-data
  "Apply calibration to incoming data"
  [data]
  (if-let [ctx @state/recording-context]
    (let [calibration-index (get-in ctx [:metadata :calibration-index])]
      (if calibration-index
        (let [sample-number (first data)
              calibrated-channels (mapv (fn [i]
                                          (let [channel-value (nth data (inc i) 0.0)
                                                ; For real-time use simpler calibration
                                                ; Just multiply by calibration factor
                                                band-key (case (mod i 5)
                                                           0 :delta
                                                           1 :theta
                                                           2 :alpha
                                                           3 :beta
                                                           :gamma)
                                                factor (get-in calibration-index [:calibration-factors band-key] 1.0)]
                                            (* channel-value factor)))
                                    (range (dec (count data))))]
          (into [sample-number] calibrated-channels)); Reconstruct data with sample number
        data)); No calibration, return original data
    data)); No recording context, return original

(defn record-loop!
  [interval-ms]
  (let [fut (future
              (try
                (loop []
                  (when @state/recording?
                    (let [data (brainflow/get-board-data @state/shim)
                          recent-data (or @state/eeg-data [])]
                      (when (and (seq data); Process data with calibration if it exists
                              (every? #(and (vector? %) (seq %)) data))
                        (let [calibrated-data (mapv #(if (seq %); Apply calibration to incoming data
                                                       (apply-calibration-to-data %)
                                                       %)
                                                data)]
                          (swap! state/eeg-data conj calibrated-data)
                          ; Update calibration periodically using recent data window
                          (when (> (count recent-data) 100)
                            (update-calibration-if-needed! (take-last 100 recent-data)))))

                      (Thread/sleep interval-ms)
                      (recur))))
                (catch Exception e
                  (println "Error in recording loop:" (.getMessage e))
                  (.printStackTrace e)))
              (reset! state/recording? false))]
    (swap! state/state assoc :recording-future fut)))

(defn start-recording!
  []
  (if @state/recording?
    (println "Already recording!")
    (do
      (when-not (brainflow/board-ready? @state/shim)
        (println "Board not ready, attempting to reconnect with profile settings...")
        (let [active-profile ((:get-active-profile @state/state))]
          (api/connect-to-default-device active-profile)))
      (println "Starting recording...")
      ; Reset recording state
      (reset! state/recording? true)
      (reset! state/eeg-data [])
      (reset! state/tags [])
      (reset! last-calibration-update 0)

      ; Create metadata file and get recording context
      (let [board-id (brainflow/get-board-id @state/shim)
            current-session-name @state/current-session-name
            context (lor/write-metadata! current-session-name board-id)]

        ; Store context for use during recording
        (reset! state/recording-context context)

        ; Start the stream
        (brainflow/start-stream! @state/shim)

        ; Start recording loop
        (record-loop! 100)))))


(defn stop-recording!
   "Stop the current recording, with option to skip file writing for calibration"
   ([]
    (stop-recording! false))
   ([skip-write?]
    (let [shim @state/shim]
      (if-not @state/recording?
        (println "Not currently recording!")
        (do
          (println "Stopping recording...")
          (reset! state/recording? false)
          (Thread/sleep 200)
          (brainflow/stop-stream! shim)
          ; Only write files if not skipping write (for calibration)
          (when-not skip-write?
            (let [board-id (brainflow/get-board-id shim)
                  eeg-data (or @state/eeg-data [])
                  tags (or @state/tags [])
                  ctx @state/recording-context
                  lorfile-dir (when ctx (:lorfile-dir ctx))
                  write-lor-fn (:write-lor! @state/state)]
              (cond
               ;; Option 1: Use the state-registered function if available
                write-lor-fn
                (let [result-dir (write-lor-fn eeg-data tags board-id)]
                  result-dir)
               ;; Option 2: Use our direct approach with context
                ctx
                (do
                  (println "Using context-based approach to write to" lorfile-dir)
                  (lor/write-recording-data! eeg-data tags ctx)
                  lorfile-dir)
                :else
                (println "Error: No way to write recording data"))))))
        (reset! state/recording-context nil)))) ;Clear recording context

(defn execute-command
  "Execute a command by its key"
  [input board-shim]
  (let [keymap (kb/get-keymap)]
    (if-let [cmd-fn (get keymap (keyword (str input)))]
      (try
        (cmd-fn board-shim)
        (catch Exception e
          (println "Error executing command:" (.getMessage e))))
      (println "Unknown command:" key))))

(defn direct-key-mode [board-shim]
  (println "\nEntering direct key mode (no Enter needed)")
  (println "Press ESC to exit this mode")
  (let [frame (JFrame. "Key Listener (Hidden)")
        key-listener (proxy [KeyListener] []
                       (keyPressed [e]
                         (let [key-char (.getKeyChar e)
                               key-code (.getKeyCode e)]
                           (if (= key-code KeyEvent/VK_ESCAPE)
                             (do
                               (.dispose frame)
                               (println "\nExited direct key mode"))
                             (execute-command key-char board-shim))))
                       (keyReleased [e])
                       (keyTyped [e]))]
    (.setSize frame 0 0)
    (.setVisible frame true)
    (.addKeyListener frame key-listener)
    (.setFocusable frame true)
    (.requestFocus frame)
    (while (.isDisplayable frame)
      (Thread/sleep 100))))

(defn customize-keybinding!
  "Allow user to customize a keybinding"
  []
  (let [active-profile (:name (:get-active-profile @state/state))
        current-bindings (kb/get-key-bindings)]
    (println "\nCurrent keybindings for profile" active-profile ":")
    (doseq [[k cmd] (sort-by first current-bindings)]
      (println (str k " → " cmd " (" (kb/get-command-description cmd) ")")))
    (println "\nEnter command to rebind (e.g., start-recording):")
    (let [cmd-name (keyword (read-line))]
      (if (contains? kb/default-commands cmd-name)
        (do
          (println "Enter key to bind to" cmd-name ":")
          (let [key (first (read-line))
                updated-bindings (assoc current-bindings key cmd-name)]
            (kb/save-profile-keymap! updated-bindings)
            (kb/load-profile-keymap!)
            (println "Keybinding updated:" key "→" cmd-name)))
        (println "Unknown command:" cmd-name)))))

(defn initialize-record! []
  (state/register-fn! :start-recording! start-recording!)
  (state/register-fn! :stop-recording!  stop-recording!)
  (state/register-fn! :tag!             tag!))