(ns visualizer.core
  (:require [clojure.java.io :as io]
            [clojure.java.browse :as browse]
            [floj.api :as api]
            [floj.brainflow.board-shim :as shim]
            [floj.cli :as cli]
            [floj.keybindings :as kb]
            [floj.frequency-analysis :as fft]
            [floj.record :as record]
            [floj.profiles :as profiles]
            [floj.state :as state]
            [mount.core :as mount]
            [visualizer.websocket :as ws]))

(def eeg-buffer (atom []))
(def window-size 256)
(def sample-count (atom 0))
(def min-samples-needed window-size)

(defn update-progress! []
  (let [current-count @sample-count
        percentage (min 100 (int (* 100 (/ current-count min-samples-needed))))
        progress-data {:type "progress"
                       :count current-count
                       :target min-samples-needed
                       :percentage percentage}]
    (ws/feed-eeg-data! progress-data)))

(defn record-loop! [interval-ms]
  (future
    (while @state/recording?
      (try
        (let [data @state/shim
              data-point (shim/get-board-data data)]

          (when (seq data-point)
            (swap! state/eeg-data conj data-point)

            ;; Based on the output, position 1 contains EEG values
            ;; Let's extract data from there if it exists
            (if (and (>= (count data-point) 2)
                     (vector? (nth data-point 1))
                     (seq (nth data-point 1)))
              (let [eeg-data (nth data-point 1)
                    ;; Take first 4 values or pad with zeros if fewer than 4
                    channel-data (vec (take 4 (concat eeg-data (repeat 0.0))))]

                ;; Log some debugging info occasionally
                (when (zero? (mod @sample-count 25))
                  (println "Extracted EEG data:" (pr-str (take 4 eeg-data))))

                (swap! eeg-buffer conj channel-data)
                (swap! sample-count inc)
                (when (zero? (mod @sample-count 10))
                  (update-progress!))

                (when (>= (count @eeg-buffer) window-size)
                  (let [analysis-window (take-last window-size @eeg-buffer)
                        sampling-rate 256
                        transposed-data (apply map vector analysis-window)
                        analysis-result (fft/analyze-eeg-buffer transposed-data window-size sampling-rate)
                        ws-data (assoc analysis-result :timestamp (System/currentTimeMillis))]
                    (println "Processed" @sample-count "samples. Sending analysis:")
                    (println (pr-str (update ws-data :channels #(take 2 %))))
                    (ws/feed-eeg-data! ws-data)
                    (when (> (count @eeg-buffer) (* 2 window-size))
                      (reset! eeg-buffer (take-last window-size @eeg-buffer))))))

              ;; If we don't have data at position 1, log the issue
              (when (zero? (mod @sample-count 25))
                (println "Warning: No EEG data found at expected position in data point")
                (println "Data point structure:" (pr-str data-point))))))
        (catch Exception e
          (println "Error in recording loop:" (.getMessage e))
          (.printStackTrace e)))
      (Thread/sleep interval-ms))
    (println "Recording stopped")))

(defn init-system! [profile-name]
  (ws/init! 3000)
  (cli/initialize-modules!)
  (mount.core/start)
  #_(cli/check-and-load-calibration! profile-name)
  (let [html-file (io/file "resources/public/floj/eeg-visualizer.html")]
    (println "Created visualization interface at:" (.getAbsolutePath html-file))

    (browse/browse-url (.toString (.toURI html-file)))))

(defn -main [& args]
  (println "Starting EEG application...")
  (if-let [profile-name (first args)]
    (do
      (println "Using profile:" profile-name)
      (init-system! profile-name)
      (api/connect-from-profile! (profiles/get-active-profile))
      (kb/create-recording-function :start-recording!)
      (record/start-recording!)
      (record-loop! 200))
    (println "Please provide a profile name (e.g., Lor)")))
#_(defn calibration-exists? [profile]
  (let [calib-file (io/file (profiles/profile-path profile))]
    (.exists calib-file)))
#_(defn run-calibration! [profile]
  (println "Starting calibration for profile:" (:name profile))
  (doseq [{:keys [label duration-ms instruction]} calibration-script]
    (ws/feed-eeg-data! {:type "calibration-stage" :label label :instruction instruction})
    (println "Instruction:" instruction)

    ;; Optional: Play audio or render UI element here

    (reset! state/eeg-data [])
    (reset! state/tags [])
    (record/start-recording!)
    (record-loop! 250)

    ;; Sleep for actual recording
    (Thread/sleep duration-ms)

    (record/stop-recording!)

    ;; Save segment with label
    ;; You could tag it or store it in memory — up to you

    ;; Wait 10 seconds before next stage
    (println "Break before next stage...")
    (Thread/sleep 10000)))
#_(defn -main [& args]
  (println "Starting EEG application...")
  (if-let [profile-name (first args)]
    (do
      (println "Using profile:" profile-name)
      (init-system!)
      (let [profile (profiles/get-active-profile)]
        (api/connect-from-profile! profile)

        (when (needs-calibration? profile) ;; or (not (calibration-exists? profile))
          (run-calibration! profile))

        ;; After calibration done
        (kb/create-recording-function :start-recording!)
        (record/start-recording!)
        (record-loop! 250)))
    (println "Please provide a profile name (e.g., Lor)")))


#_(-main "Lor")