(ns floj.record
  "Containing the logic for generic recording functionality. Wave-signature recording is a specialized
   type of recording that uses this same module. The eeg visualizer also uses this same basic recording
   functionality. I'd recommend using this if you plan on making simple eeg applications using this library.
   It uses 'write-metadata' from the lor.clj namespace when the recording begins with 'start-recording'. This
   also uses the stream_manager.clj file and calibration.clj file routinely for formatting the raw data, and
   on the fly calibration. Most of the timings are customizable, but could probably be made more user friendly.
   The recordings by default print the output to the command line but can easily be commented out in the
   'record-loop!' function (lines 179 and 180 as I write this)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [floj.brainflow.board-shim :as brainflow]
            [floj.api :as api]
            [floj.calibration :as calibrate]
            [floj.lor :as lor]
            [floj.keybindings :as kb]
            [floj.state :as state]
            [floj.stream-manager :as stream])
  (:import [java.awt.event KeyListener KeyEvent]
           [javax.swing JFrame]))

(def last-calibration-update (atom 0))
(def initial-stabilization-samples 5)

(defn tag! [label]
  (when @state/recording?
    (let [timestamp (System/currentTimeMillis)]
      (swap! state/tags conj {:timestamp timestamp :label label})
      (println "Tagged:" label "at" timestamp))))

(defn load-historical-calibrations
  "Load the last N calibration files from history"
  [profile-name n]
  (try
    (let [history-dir (str ".home/.lor/profiles/history/" profile-name)
          history-files (when (.exists (io/file history-dir))
                          (->> (.listFiles (io/file history-dir))
                               (filter #(.isFile %))
                               (filter #(.endsWith (.getName %) ".edn"))
                               (sort-by #(.lastModified %))
                               (take-last n))) ; n files up to the maximum number of calibrations permitted
          calibrations (for [file history-files]
                         (try
                           (edn/read-string (slurp file))
                           (catch Exception e
                             (println "Error reading calibration file:" (.getName file))
                             nil)))]
      (println "Loaded" (count calibrations) "historical calibrations")
      (filterv some? calibrations))
    (catch Exception e
      (println "Error loading historical calibrations:" (.getMessage e))
      [])))

(defn process-calibration
  "Complete calibration pipeline with dynamic target distribution"
  [raw-data sampling-rate profile]
  (try
    (println "Starting calibration process with" (count raw-data) "data points")
    (let [profile-name (:name profile)

          recent-calibrations (calibrate/load-recent-calibrations profile-name 5)
          ; Extract dynamic target from profile
          profile-calibration (when profile
                                {:golden-tensor (get profile :golden-tensor)})

          ; Determine the data format and normalize
          data-format (if (and (seq raw-data) (map? (first raw-data)))
                        :map-format
                        :unknown)
          normalized-data (if (= data-format :map-format)
                            (let [channel-keys (remove #{:timestamp} (keys (first raw-data)))]
                              (vec (for [k channel-keys]
                                     (mapv #(get % k 0.0) raw-data))))
                            (let [std-format (stream/normalize-data-format raw-data)]
                              (if (and (map? std-format) (:eeg std-format))
                                ; Ensure data is in [channels][samples] format
                                (stream/transpose-data (:eeg std-format))
                                std-format)))

          ; Extract band powers
          band-powers (calibrate/extract-band-powers normalized-data sampling-rate)

          ; Calculate total power for band distribution
          total-power (reduce + (vals band-powers))

          ; Calculate band distribution (normalized power ratios)
          band-distribution (when (> total-power 0)
                              (into {} (for [[band power] band-powers]
                                         [band (/ power total-power)])))

          ; Calculate calibration factors
          golden-tensor (get profile :golden-tensor)
          target-frequency-domain (get-in golden-tensor [:spectral :frequency-domain])

          calibration-factors (when (and band-powers target-frequency-domain)
                                (into {} (for [[band current-power] band-powers]
                                           (let [target-power (get target-frequency-domain band 1.0)]
                                             [band (if (> current-power 0)
                                                     (/ target-power current-power)
                                                     0.1)]))))

          ; Build the calibration index
          calibration-index (let [aggregate-dist (when (seq recent-calibrations)
                                                   (calibrate/calculate-aggregate-distribution
                                                    (map :calibration-index recent-calibrations)))
                                  base-index (calibrate/create-calibration-index band-powers profile-calibration)]
                              (if aggregate-dist
                                (assoc base-index :band-distribution aggregate-dist
                                       :calibration-factors calibration-factors)
                                (assoc base-index :band-distribution band-distribution
                                       :calibration-factors calibration-factors)))]

      #_(println "Created calibration index with factors:" (:calibration-factors calibration-index))

      ; Save the calibration to history
      (calibrate/save-calibration-to-history! profile-name calibration-index)

      ; Check if we should update the profile's golden tensor
      (let [recording-counter ((:update-recording-counter! @state/state))]
        (when (calibrate/should-update-profile? recording-counter)
          (println "Updating profile golden tensor after" recording-counter "recordings")
          ((:update-golden-tensor @state/state))))

      ; Return the calibration index with band powers, distribution, and factors
      calibration-index)

    (catch Exception e
      (println "Error in calibration process:" (.getMessage e))
      (.printStackTrace e)
      nil)))

(defn update-calibration-if-needed!
  "Updates the calibration index periodically based on recent data"
  []
  (let [current-time (System/currentTimeMillis)
        last-update @last-calibration-update]
    (when (> (- current-time last-update) 5000)
      (reset! last-calibration-update current-time)
      (let [recent-data @state/eeg-data
            profile ((:get-active-profile @state/state))
            sampling-rate (api/get-current-sample-rate)
            eeg-data-only (mapv :eeg recent-data)
            new-calibration-index (process-calibration
                                   eeg-data-only
                                   sampling-rate
                                   profile)]

        (when new-calibration-index
          ; Use the stored recording context instead of creating new timestamp
          (let [recording-ctx @state/recording-context]
            (when recording-ctx
              (lor/update-metadata-calibration!
               (:lorfile-dir recording-ctx)
               (:metadata recording-ctx)
               new-calibration-index))))))))

(defn record-loop!
  [interval-ms]
  (let [stabilization-counter (atom initial-stabilization-samples)
        fut (future
              (try
                (loop []
                  (when @state/recording?
                    (let [board-id (brainflow/get-board-id @state/shim)
                          raw-data (brainflow/get-board-data @state/shim)
                          parsed-data (stream/get-streamed-eeg-data raw-data board-id)
                          calibration-index (get-in @state/recording-context [:metadata :calibration-index])]

                      (when (and parsed-data (seq (:eeg parsed-data)))
                        (let [; Transpose from [samples][channels] to [channels][samples] for DSP processing
                              eeg-channels-samples (stream/transpose-data (:eeg parsed-data))
                              calibrated-eeg (if calibration-index
                                               (calibrate/apply-calibration eeg-channels-samples calibration-index)
                                               eeg-channels-samples)
                              ; Transpose back to [samples][channels] for storage
                              eeg-samples-channels (stream/transpose-data calibrated-eeg)]

                          ; Uncomment print sample data for debugging/to observe the live stream
                          (println "Calibrated EEG data sample:"
                                   (vec (take 5 (mapv vec eeg-samples-channels))))

                          ; Only store data after stabilization period
                          (if (pos? @stabilization-counter)
                            (do
                              (swap! stabilization-counter dec)
                              (println "Discarding sample during stabilization period. Remaining:" @stabilization-counter))

                            ; Normal data storage and processing after stabilization
                            (do
                              (swap! state/eeg-data conj {:eeg eeg-samples-channels
                                                          :timestamp (System/currentTimeMillis)})
                              (update-calibration-if-needed!)))))

                      (Thread/sleep interval-ms)
                      (recur))))
                (catch Exception e
                  (println "Error in recording loop:" (.getMessage e))
                  (.printStackTrace e)))
              (reset! state/recording? false))]
    (swap! state/state assoc :recording-future fut)))

(defn describe-board
  "Get a description of the current board's channels"
  []
  (let [board-id (brainflow/get-board-id @state/shim)]
    (stream/describe-board-channels board-id)))

(defn start-recording!
  "Start recording with optional custom path"
  ([]
   (start-recording! nil))
  ([custom-config]
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
             context (lor/write-metadata! current-session-name board-id custom-config)]

         ; Store context for use during recording
         (reset! state/recording-context context)

         ; Start the stream
         (brainflow/start-stream! @state/shim)

         ; Start recording loop
         (record-loop! 100))))))

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
         (when-not skip-write?
           (let [board-id (brainflow/get-board-id shim)
                 eeg-data (or @state/eeg-data [])
                 tags (or @state/tags [])
                 write-lor-fn (:write-lor! @state/state)]
             (write-lor-fn eeg-data tags board-id))))))
   (reset! state/recording-context nil))) ; Clear recording context

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
  (state/register-fn! :describe-board   describe-board)
  (state/register-fn! :start-recording! start-recording!)
  (state/register-fn! :stop-recording!  stop-recording!)
  (state/register-fn! :tag!             tag!))