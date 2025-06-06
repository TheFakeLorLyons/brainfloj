(ns brain-pong.bci-integration
  (:require [hyperfiddle.electric3 :as e]
            [brain-pong.game-state :as pong-state]
            [hyperfiddle.electric-dom3 :as dom]
            [mount.core :as mount]
            #?(:clj [brain-pong.signature :as signature])
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

#?(:cljs (declare process-bci-input))
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
   (defn disconnect-device-server []
     (println "Server: Disconnecting from BCI device")
     (try
       (when-let [f (:release-board! @state/state)]
         (f @state/shim))
       {:connected true}
       (catch Exception e
         (println "Error disconnecting:" (.getMessage e))
         {:connected false :error (.getMessage e)}))))

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

(e/defn Start-recording! [category]
  (e/client
   (js/console.log "Client: Starting recording for category:" category)
   (let [result (e/server (start-recording-server category))]
     (when (:connected result)
       (swap! pong-state/state assoc-in [:bci :recording?] true)
       (swap! pong-state/state assoc-in [:bci :current-category] category))
     result)))

(e/defn Stop-recording! [category]
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



#?(:cljs
   (defn process-bci-input []

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