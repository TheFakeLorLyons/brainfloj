(ns calibration.core
  (:require [floj.api :as api]
            [floj.brainflow.boardshim :as shim]
            [floj.keybindings :as kb]
            [floj.record :as record]
            [floj.profiles :as profiles]
            [floj.state :as state]
            [calibration.websocket :as ws]
            [calibration.frequency-analysis :as fft]
            [clojure.java.io :as io]
            [clojure.java.browse :as browse]
            [mount.core :as mount]
            [floj.cli :as cli]))

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

            (let [channels (range 4)
                  channel-data (mapv #(let [val (get data-point (inc %))]
                                        (if (number? val) val 0.0))
                                 channels)]
              (swap! eeg-buffer conj channel-data)
              (swap! sample-count inc)

              (when (zero? (mod @sample-count 10))
                (update-progress!))

              (when (>= (count @eeg-buffer) window-size)
                (let [analysis-window (take-last window-size @eeg-buffer)
                      sampling-rate 200
                      transposed-data (apply map vector analysis-window)
                      analysis-result (fft/analyze-eeg-buffer transposed-data window-size sampling-rate)
                      ws-data (assoc analysis-result :timestamp (System/currentTimeMillis))]
                  (println "Processed" @sample-count "samples. Sending analysis:")
                  (println (pr-str (update ws-data :channels #(take 2 %))))
                  (ws/feed-eeg-data! ws-data)

                  (when (> (count @eeg-buffer) (* 2 window-size))
                    (reset! eeg-buffer (take-last window-size @eeg-buffer))))))))
        (catch Exception e
          (println "Error in recording loop:" (.getMessage e))
          (.printStackTrace e)))
      (Thread/sleep interval-ms))
    (println "Recording stopped")))


(defn init-system! []
  (ws/init! 3000)
  (cli/initialize-modules!)
  (mount.core/start)
  (let [html-file (io/file "resources/public/floj/eeg-visualizer.html")]
    (println "Created visualization interface at:" (.getAbsolutePath html-file))

    (browse/browse-url (.toString (.toURI html-file)))))

(defn -main [& args]
  (println "Starting EEG application...")
  (if-let [profile-name (first args)]
    (do
      (println "Using profile:" profile-name)
      (init-system!)
      (api/connect-from-profile! (profiles/get-active-profile))
      (kb/create-recording-function :start-recording!)
      (record/start-recording!)
      (record-loop! 250))
    (println "Please provide a profile name (e.g., Lor)")))

#_(-main "Lor")