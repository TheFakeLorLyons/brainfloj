(ns brain-pong.bci-integration
  (:require [hyperfiddle.electric3 :as e]
            [brain-pong.game-state :as pong-state]
            [hyperfiddle.electric-dom3 :as dom]
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
     (initialize-modules!)
     (println "Server: Connecting to BCI device with active profile")
     (let [profile (profiles/get-active-profile)
           profile-name (:name profile)
           success? (api/connect-from-profile! profile)]
       (println "Connection result:" success? "with profile" profile-name)
       {:success success? :profile-name profile-name})))

; Client functions for BCI integration
(e/defn connect-device! []
  (e/client
   (let [result (e/server (connect-device-server))]
     (when (:success result)
       (js/console.log "Connection successful:" (:profile-name result))
       (swap! pong-state/state assoc-in [:bci :device-connected?] true)
       (swap! pong-state/state assoc-in [:bci :active-profile] (:profile-name result)))
     result)))

; Add a pure client-side function for use in event handlers
#?(:cljs
   (defn handle-connect-click []
     (js/console.log "Client-side connect click handler")
     (e/run
      (let [result (e/server (bci/connect-device-server))]
        (js/console.log "Server connection result:" (clj->js result))
        (when (:success result)
          (swap! pong-state/state assoc-in [:bci :device-connected?] true)
          (swap! pong-state/state assoc-in [:bci :active-profile] (:profile-name result)))))))

#?(:cljs
   (defn handle-disconnect-click []
     (js/console.log "Client-side disconnect click handler")
     (e/run
      (let [result (e/server (bci/disconnect-device-server))]
        (js/console.log "Server disconnect result:" (clj->js result))
        (swap! pong-state/state assoc-in [:bci :device-connected?] false)
        (swap! pong-state/state assoc-in [:bci :active-profile] nil)))))

#?(:clj
   (defn get-device-status-server []
     (try
       (let [connected? (boolean @state/shim)]
         {:connected connected?
          :profile-name (when connected? (:name ((:get-active-profile @state/state))))})
       (catch Exception e
         (println "Error getting device status:" (.getMessage e))
         {:connected false :error (.getMessage e)}))))

(e/defn check-device-status! []
  (let [status (e/server (get-device-status-server))]
    (swap! pong-state/state assoc-in [:bci :device-connected?] (:connected status))
    (when (:connected status)
      (swap! pong-state/state assoc-in [:bci :active-profile] (:profile-name status)))
    status))

#?(:cljs
   (e/defn start-activity-matching! []
     (e/client
      (println "Client: Starting brain activity matching")

   ; First clear any existing interval
      (when-let [existing-interval (get-in @pong-state/state [:bci :match-interval])]
        (js/clearInterval existing-interval))

   ; Create new polling interval
      (let [interval (js/setInterval
                      (e/fn []
                        (e/fn []
                          (e/client
                           (let [confidence-data (e/server (match-brain-activity-server))]
                             (println "Received confidence data:" confidence-data)
                             (swap! pong-state/state assoc-in [:bci :confidence] confidence-data)

                         ; Apply paddle movement based on confidence
                             (let [up-confidence (get confidence-data :up 0.0)
                                   down-confidence (get confidence-data :down 0.0)
                                   threshold (get-in @pong-state/state [:bci :threshold] 0.6)
                                   sensitivity (get-in @pong-state/state [:bci :sensitivity] 0.5)]

                           ; Calculate paddle speeds
                               (let [up-speed (calculate-paddle-speed up-confidence threshold sensitivity)
                                     down-speed (calculate-paddle-speed down-confidence threshold sensitivity)]

                             ; Apply movement if above threshold
                                 (when up-speed
                                   (pong-state/move-paddle! :up))

                                 (when down-speed
                                   (pong-state/move-paddle! :down))))))))
                      100)] ; Poll every 100ms

     ; Store the interval ID for later cleanup
        (swap! pong-state/state assoc-in [:bci :match-interval] interval)
        (swap! pong-state/state assoc-in [:bci :matching?] true))
      true)))

#?(:cljs
   (e/defn stop-activity-matching! []
     (e/client
      (println "Client: Stopping brain activity matching")

      (when-let [interval (get-in @pong-state/state [:bci :match-interval])]
        (js/clearInterval interval)
        (swap! pong-state/state assoc-in [:bci :match-interval] nil)
        (swap! pong-state/state assoc-in [:bci :matching?] false))
      true)))

; BCI recording functions for calibration  
#?(:clj
   (defn start-recording-server [category]
     (println "Server: Starting recording for category:" category)
     (try
       (lexi/start-wave-signature-recording! category "pong")
       (catch Exception e
         (println "Error starting recording:" (.getMessage e))
         nil))))

#?(:cljs
   (e/defn StartRecording [category]
     (e/server
      (start-recording-server category))))

#?(:clj
   (defn stop-recording-server []
     (try
       (lexi/stop-wave-signature-recording!)
       (catch Exception e
         (println "Error stopping recording:" (.getMessage e))
         nil))))

#?(:cljs
   (e/defn start-recording! [category]
     (e/client
      (println "Client: Starting recording for category:" category)
      (let [session-id (e/server (start-recording-server category))]
        (when session-id
          (swap! pong-state/state assoc-in [:bci :recording?] true)
          (swap! pong-state/state assoc-in [:bci :current-category] category))
        session-id))))

#?(:cljs
   (e/defn stop-recording! []
     (e/client
      (println "Client: Stopping recording")
      (let [result (e/server (stop-recording-server))]
        (swap! pong-state/state assoc-in [:bci :recording?] false)
        (swap! pong-state/state assoc-in [:bci :current-category] nil)
        result))))

#?(:cljs
   (e/def update-threshold!
     (e/server
      #?(:clj
         (fn [new-threshold]
           (swap! device-state assoc :threshold new-threshold)
           (swap! pong-state/state assoc-in [:bci :threshold] new-threshold)
           new-threshold)))))

#?(:cljs
   (e/defn process-bci-input! []
     (e/client
      (let [state (e/watch pong-state/state)
            connected? (get-in state [:bci :device-connected?])
            matching? (get-in state [:bci :match-interval])
            confidence (get-in state [:bci :confidence] {:up 0.0 :down 0.0})
            threshold (get-in state [:bci :threshold] 0.6)
            sensitivity (get-in state [:bci :sensitivity] 0.5)]

        (when (and connected? matching?)
          (let [up-speed (calculate-paddle-speed (:up confidence) threshold sensitivity)
                down-speed (calculate-paddle-speed (:down confidence) threshold sensitivity)]

         ; Apply up movement if confidence exceeds threshold
            (when up-speed
              (swap! pong-state/state update-in [:game :player-paddle :y]
                     (fn [y] (max 0 (- y up-speed)))))

         ; Apply down movement if confidence exceeds threshold
            (when down-speed
              (swap! pong-state/state update-in [:game :player-paddle :y]
                     (fn [y] (min (- (get-in @pong-state/state [:game :court :height])
                                     (get-in @pong-state/state [:game :player-paddle :height]))
                                  (+ y down-speed)))))))))))

#?(:cljs
   (e/defn bci-game-loop-tick []
     (e/client
      (process-bci-input!))))