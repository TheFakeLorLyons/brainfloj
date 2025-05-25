(ns brain-pong.bci-integration
  (:require [hyperfiddle.electric3 :as e]
            [brain-pong.game-state :as pong-state]
            [hyperfiddle.electric-dom3 :as dom]
            [mount.core :as mount]
            [brain-pong.signature :as signature]
            #?(:clj [floj.api :as api])
            #?(:clj [floj.state :as state])
            #?(:clj [floj.io :as fio])
            #?(:clj [floj.lor :as lor])
            #?(:clj [floj.profiles :as profiles])
            #?(:clj [floj.wave-lexicon :as lexi])
            #?(:clj [floj.calibration :as calibrate])
            #?(:clj [floj.record :as record])
            #?(:clj [floj.wave-refraction :as refraction])
            #?(:clj [floj.keybindings :as kb])))

#?(:clj
   (defn initialize-modules! []
     (api/initialize-brainflow!)
     (fio/initialize-io!)
     (lor/initialize-lor!)
     (kb/initialize-keybindings!)
     (profiles/initialize-profiles!)
     (lexi/initialize-lexicon!)
     (record/initialize-record!)
     (calibrate/initialize-calibration!)
     (refraction/initialize-baseline!)))

#?(:clj
   (defn connect-device-server []
     (try
       (initialize-modules!)
       (mount/start)
       (println "Server: Connecting to BCI device with active profile")
       (let [active-profile ((:get-active-profile @state/state))
             active-profile-name (:name active-profile)
             connected? (api/connect-from-profile! active-profile)]
         (println "Connection result:" connected?)
         {:connected connected?
          :profile-name active-profile-name})
       (catch Exception e
         (println "Error in connect-device-server:" (.getMessage e))
         {:connected false :error (.getMessage e)}))))


#?(:clj
   (def disconnect-device-server
     (fn []
       (println "Server: Disconnecting from BCI device")
       (try
         (when-let [f (:release-board! @state/state)]
           (f @state/shim))
         {:connected true}
         (catch Exception e
           (println "Error disconnecting:" (.getMessage e))
           {:connected false :error (.getMessage e)})))))

#?(:clj
   (defn get-device-status-server []
     (try
       (let [connected? (boolean @state/shim)
             active-profile ((:get-active-profile @state/state))
             active-profile-name (:name active-profile)]
         {:connected connected?
          :profile-name active-profile-name})
       (catch Exception e
         (println "Error getting device status:" (.getMessage e))
         {:success false
          :connected false
          :error (.getMessage e)}))))

#?(:clj
   (defn start-recording-server [category]
     (println "Server: Starting recording for category:" category)
     (try
       (lexi/start-wave-signature-recording! category "pong")
       (catch Exception e
         (println "Error starting recording:" (.getMessage e))
         nil))))

#?(:clj
   (defn stop-recording-server [category]
     (try
       (lexi/stop-wave-signature-recording! category "pong")
        {:connected true}
       (catch Exception e
         (println "Error stopping recording:" (.getMessage e))
         {:connected false :error (.getMessage e)}))))

(e/defn start-recording! [category]
  (e/client
   (js/console.log "Client: Starting recording for category:" category)
    (let [result (e/server (start-recording-server category))]
      (when (:connected result)
        (swap! pong-state/state assoc-in [:bci :recording?] true)
        (swap! pong-state/state assoc-in [:bci :current-category] category))
      result)))

(e/defn stop-recording! [category]
  (e/client
   (js/console.log "Client: Stopping recording")
   (let [result (e/server (stop-recording-server category))]
     (swap! pong-state/state assoc-in [:bci :recording?] false)
     (swap! pong-state/state assoc-in [:bci :current-category] nil)
     result)))

#_#?(:cljs
   (e/def update-threshold!
     (e/server
      #?(:clj
         (fn [new-threshold]
           (swap! device-state assoc :threshold new-threshold)
           (swap! pong-state/state assoc-in [:bci :threshold] new-threshold)
           new-threshold)))))

#?(:cljs
   (defn calculate-paddle-speed
     "Calculate paddle speed based on confidence score, threshold, and sensitivity"
     [confidence threshold sensitivity]
     (when (>= confidence threshold)
       ; Map confidence from threshold-1.0 range to 0-1.0 range
       (let [normalized-confidence (/ (- confidence threshold) (- 1.0 threshold))
             ; Apply sensitivity factor (higher sensitivity = more movement)
             ; Base speed of 5-15 pixels per frame based on sensitivity and confidence
             base-speed (* 5 (+ 1 (* 2 sensitivity))) ; Apply sensitivity multiplier (higher sensitivity = more responsive)
             speed (* base-speed normalized-confidence)]
         speed))))


(e/defn match-brain-activity-server
  []
  (e/server
   (fn []
     #?(:clj
        (try
       ; First check if we have EEG recording active
          (let [recording? @state/recording?
                eeg-data @state/eeg-data]
            (cond
           ; Recording not started
              (not recording?)
              (do
                (println "EEG recording not started - need to start Pong recording first")
                {:up 0.0 :down 0.0 :error "EEG recording not started"})

           ; No data or not started
              (nil? eeg-data)
              (do
                (println "EEG data not available - recording started but no data stream yet")
                {:up 0.0 :down 0.0 :error "EEG data stream not ready"})

           ; Data available but empty
              (empty? eeg-data)
              (do
                (println "EEG data stream started but no data collected yet")
                {:up 0.0 :down 0.0 :error "No EEG data collected yet"})

           ; We have data - proceed with matching
              :else
              (let [sampling-rate ((:sampling-rate @state/state))
                    last-n-samples (min (count eeg-data) sampling-rate)
                    recent-data (take-last last-n-samples eeg-data)
                    current-features (signature/extract-signature-features recent-data sampling-rate)

                    profile-name (or (:name ((:get-active-profile @state/state))) "default")

                    up-dir (fio/get-wave-lexicon-dir profile-name "pong/up")
                    up-signatures (signature/load-wave-signatures-from-dir up-dir)

                    down-dir (fio/get-wave-lexicon-dir profile-name "pong/down")
                    down-signatures (signature/load-wave-signatures-from-dir down-dir)

                    up-scores (map #(signature/calculate-signature-similarity current-features %) up-signatures)
                    down-scores (map #(signature/calculate-signature-similarity current-features %) down-signatures)

                    best-up-score (if (seq up-scores) (apply max up-scores) 0.0)
                    best-down-score (if (seq down-scores) (apply max down-scores) 0.0)

                    golden-tensor (get-in ((:get-active-profile @state/state)) [:golden-tensor])

                    [calibrated-up calibrated-down]
                    (if golden-tensor
                      (let [spectral-calibration (get-in golden-tensor [:spectral :frequency-domain])
                            calibration-factors (try
                                                  (-> (str (fio/get-wave-lexicon-dir profile-name "pong") "/category.edn")
                                                      (fio/read-edn-file)
                                                      (get-in [:summary :all :calibration-factors :average]))
                                                  (catch Exception _ nil))
                            calibrated-up (if (and spectral-calibration calibration-factors)
                                            (* best-up-score
                                               (/ (+ (:alpha calibration-factors)
                                                     (:beta calibration-factors))
                                                  2.0))
                                            best-up-score)

                            calibrated-down (if (and spectral-calibration calibration-factors)
                                              (* best-down-score
                                                 (/ (+ (:alpha calibration-factors)
                                                       (:beta calibration-factors))
                                                    2.0))
                                              best-down-score)]

                        [(max 0.0 (min 1.0 calibrated-up))
                         (max 0.0 (min 1.0 calibrated-down))])

                      [best-up-score best-down-score])

                    confidence-data {:up calibrated-up
                                     :down calibrated-down}]

                (println "Confidence scores:" confidence-data)
                confidence-data)))
          (catch Exception e
            (println "Error matching brain activity:" (.getMessage e))
            {:up 0.0 :down 0.0 :error (.getMessage e)}))))))

#?(:clj
   (defn start-eeg-streaming-server [category]
     (try
       (println "Server: Starting EEG data streaming for Pong")
       (lexi/start-category-recording! category)
       (println "Started Pong EEG recording stream")
       {:success true :streaming true}
       (catch Exception e
         (println "Error starting EEG streaming:" (.getMessage e))
         {:success false :streaming false :error (.getMessage e)}))))

#?(:clj
   (defn stop-eeg-streaming-server [category]
     (try
       (println "Server: Stopping EEG data streaming")
       (lexi/stop-category-recording! category)
       {:success true :streaming false}
       (catch Exception e
         (println "Error stopping EEG streaming:" (.getMessage e))
         {:success false :streaming true :error (.getMessage e)}))))

#?(:clj
   (defn get-eeg-data-status-server []
     (try
       (let [recording? @state/recording?
             eeg-data @state/eeg-data
             data-available? (and eeg-data
                                  recording?
                                  (seq eeg-data))]
         {:data-available data-available?
          :recording recording?
          :data-type (type eeg-data)
          :data-count (if data-available? (count eeg-data) 0)})
       (catch Exception e
         (println "Error checking EEG data status:" (.getMessage e))
         {:data-available false :recording false :error (.getMessage e)}))))

(e/defn start-eeg-streaming! [category]
  (e/client
   (js/console.log "Client: Starting EEG streaming")
   (let [result (e/server (start-eeg-streaming-server category))]
     (when (:success result)
       (swap! pong-state/state assoc-in [:bci :streaming?] true))
     result)))

(e/defn stop-eeg-streaming! [category]
  (e/client
   (js/console.log "Client: Stopping EEG streaming")
   (let [result (e/server (stop-eeg-streaming-server category))]
     (when (:success result)
       (swap! pong-state/state assoc-in [:bci :streaming?] false))
     result)))

(e/defn check-eeg-data-status []
  (e/server (get-eeg-data-status-server)))

(e/defn start-activity-matching! []
  (e/client
   (println "Client: Starting brain activity matching")
   (swap! pong-state/state assoc-in [:bci :matching?] true)
   true))

(e/defn stop-activity-matching! []
  (e/client
   (println "Client: Stopping brain activity matching")
   (swap! pong-state/state assoc-in [:bci :matching?] false)
   true))

(e/defn PollBrainActivity [category]
  (e/client
   (let [_ (dom/on-animation-frame)
         state (e/watch pong-state/state)
         matching? (get-in state [:bci :matching?])
         streaming? (get-in state [:bci :streaming?])
         connected? (get-in state [:bci :device-connected?])]

     (when (and connected? matching? (not streaming?))
       (js/console.log "Starting EEG streaming for brain activity matching")
       (let [stream-result (e/server (start-eeg-streaming-server category))]
         (js/console.log "Stream start result:" (clj->js stream-result))
         (when (:success stream-result)
           (swap! pong-state/state assoc-in [:bci :streaming?] true))))

     (when (and matching? streaming? connected?)

       (let [confidence-data (match-brain-activity-server)]
         (js/console.log "Raw confidence data received:" (clj->js confidence-data))
         (when confidence-data
           (if (:error confidence-data)
             (js/console.log "BCI Error:" (:error confidence-data))
             (do
               (js/console.log "Processing confidence data:" (clj->js confidence-data))
               (swap! pong-state/state assoc-in [:bci :confidence] confidence-data)
               ))))))))

#?(:cljs
   (defn process-bci-input! []
     (let [state @pong-state/state
           connected? (get-in state [:bci :device-connected?])
           matching? (get-in state [:bci :matching?])
           confidence (get-in state [:bci :confidence] {:up 0.0 :down 0.0})
           threshold (get-in state [:bci :threshold] 0.6)
           sensitivity (get-in state [:bci :sensitivity] 0.5)]

       (js/console.log "Processing BCI input - Connected:" connected? "Matching:" matching? "Confidence:" (clj->js confidence))

       (when (and connected? matching?)
         (let [; Calculate paddle speeds based on confidence values
               up-confidence (:up confidence)
               down-confidence (:down confidence)]

           (js/console.log "Up confidence:" up-confidence "Down confidence:" down-confidence "Threshold:" threshold)

           ; Check if either confidence exceeds threshold
           (cond
             ; Strong up signal and stronger than down
             (and (>= up-confidence threshold)
                  (> up-confidence down-confidence))
             (do
               (js/console.log "BCI: Moving paddle UP - confidence:" up-confidence)
               (pong-state/move-paddle! :up))

             ; Strong down signal and stronger than up  
             (and (>= down-confidence threshold)
                  (> down-confidence up-confidence))
             (do
               (js/console.log "BCI: Moving paddle DOWN - confidence:" down-confidence)
               (pong-state/move-paddle! :down))

             :else
             (js/console.log "BCI: No movement - up:" up-confidence "down:" down-confidence "threshold:" threshold)))))))