(ns brain-pong.bci-integration
  (:require [hyperfiddle.electric3 :as e]
            [brain-pong.game-state :as pong-state]
            [hyperfiddle.electric-dom3 :as dom]
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
       (println "Server: Connecting to BCI device with active profile")
       (let [profile (profiles/get-active-profile)
             profile-name (:name profile)
             connected? (api/connect-from-profile! profile)]
         (println "Connection result:" connected? "with profile" profile-name)
         {:success connected?
          :connected connected? 
          :profile-name profile-name})
       (catch Exception e
         (println "Error in connect-device-server:" (.getMessage e))
         {:connected false :error (.getMessage e)}))))

(e/defn connect-device! []
  (e/client
   (js/console.log "Client: Starting connection process")
   (let [result (e/server (connect-device-server))]
     (js/console.log "Client: Connection result:" (clj->js result))
     (if (:connected result)
       (do
         (swap! pong-state/state update-in [:bci] merge
                {:device-connected? (:connected result)
                 :active-profile (:profile-name result)
                 :connection-error nil})
         (js/console.log "Client: State updated successfully"))
       (do
         (swap! pong-state/state update-in [:bci] merge
                {:device-connected? false
                 :active-profile nil
                 :connection-error (:error result)})
         (js/console.log "Client: Connection failed:" (:error result))))
     result)))
(e/defn handle-connect-click [_]
  (e/client
   (js/console.log "Client-side connect click handler started")
   (let [result (e/server (connect-device-server))]
     (js/console.log "Server connection result:" (clj->js result))
     (when (:connected result)
       (swap! pong-state/state assoc-in [:bci :device-connected?] true)
       (swap! pong-state/state assoc-in [:bci :active-profile] (:profile-name result)))
     result)))

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


(e/defn disconnect-device! []
  (e/client
   (let [result (e/server (disconnect-device-server))]
     (js/console.log "Disconnect result:" (clj->js result))
     (swap! pong-state/state update-in [:bci] merge
            {:connected false
             :active-profile nil
             :matching? false})
     ; Clear any running intervals
     (when-let [interval (get-in @pong-state/state [:bci :match-interval])]
       (js/clearInterval interval)
       (swap! pong-state/state assoc-in [:bci :match-interval] nil))
     result)))

(e/defn handle-disconnect-click []
  (e/client
   (js/console.log "Client-side disconnect click handler")
    (let [result (e/server (disconnect-device-server))]
      (js/console.log "Server disconnect result:" (clj->js result))
      (swap! pong-state/state assoc-in [:bci :device-connected?] false)
      (swap! pong-state/state assoc-in [:bci :active-profile] nil))))

#?(:clj
   (defn get-device-status-server []
     (try
       (let [connected? (boolean @state/shim)
             profile-name (when connected?
                            (let [active-profile-fn (:get-active-profile @state/state)]
                              (when active-profile-fn
                                (:name (active-profile-fn)))))]
         {:success true
          :connected connected?
          :profile-name profile-name})
       (catch Exception e
         (println "Error getting device status:" (.getMessage e))
         {:success false
          :connected false
          :error (.getMessage e)}))))
(e/defn check-device-status! []
  (e/client
   (js/console.log "Client: Checking device status")
   (let [status (e/server (get-device-status-server))]
     (js/console.log "Client: Status result:" (clj->js status))
     (when (:connected status)
       (let [current-state (get-in @pong-state/state [:bci])
             new-connected? (:connected status)
             new-profile (:profile-name status)]
         ;; Only update if there's actually a change to avoid unnecessary re-renders
         (when (or (not= (:device-connected? current-state) new-connected?)
                   (not= (:active-profile current-state) new-profile))
           (js/console.log "Client: Updating status - Connected:" new-connected? "Profile:" new-profile)
           (swap! pong-state/state update-in [:bci] merge
                  {:device-connected? new-connected?
                   :active-profile new-profile}))))
     status)))


#?(:clj
   (defn start-recording-server [category]
     (println "Server: Starting recording for category:" category)
     (try
       (lexi/start-wave-signature-recording! category "pong")
       (catch Exception e
         (println "Error starting recording:" (.getMessage e))
         nil))))

#?(:clj
   (defn stop-recording-server []
     (try
       (lexi/stop-wave-signature-recording!)
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

(e/defn stop-recording! []
  (e/client
   (js/console.log "Client: Stopping recording")
   (let [result (e/server (stop-recording-server))]
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
   (defn handle-start-recording! [category]
     (js/console.log "Handle start recording for category:" category)
     (swap! pong-state/state assoc-in [:bci :pending-record] category)))

#?(:cljs
   (defn handle-stop-recording! []
     (js/console.log "Handle stop recording")
     (swap! pong-state/state assoc-in [:bci :pending-stop-record] true)))

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

#?(:cljs
   (defn process-bci-input! []
     (let [state @pong-state/state
           connected? (get-in state [:bci :device-connected?])
           matching? (get-in state [:bci :matching?])
           confidence (get-in state [:bci :confidence] {:up 0.0 :down 0.0})
           threshold (get-in state [:bci :threshold] 0.6)
           sensitivity (get-in state [:bci :sensitivity] 0.5)]

       (when (and connected? matching?)
         (let [; Calculate paddle speeds based on confidence values
               up-confidence (:up confidence)
               down-confidence (:down confidence)

               ; Apply non-linear transformation to make movements more responsive
               ; This helps when confidence values are close to threshold
               enhanced-up (js/Math.pow up-confidence 1.5)
               enhanced-down (js/Math.pow down-confidence 1.5)

               ; Calculate movement speeds with the enhanced values
               up-speed (calculate-paddle-speed enhanced-up threshold sensitivity)
               down-speed (calculate-paddle-speed enhanced-down threshold sensitivity)]

           ; If we have both up and down confidence high, use the stronger signal
           (cond
             ; Strong up signal and stronger than down
             (and up-speed (or (not down-speed) (> up-confidence down-confidence)))
             (pong-state/move-paddle! :up)

             ; Strong down signal and stronger than up
             (and down-speed (or (not up-speed) (> down-confidence up-confidence)))
             (pong-state/move-paddle! :down)))))))

(e/defn get-confidence-data []
  (e/server (signature/match-brain-activity-server)))

(e/defn RecordingStatusWatcher []
  (e/client
   (let [state (e/watch pong-state/state)
         pending-record (get-in state [:bci :pending-record])
         pending-stop (get-in state [:bci :pending-stop-record])]

     ;; Handle pending recording start
     (when pending-record
       (swap! pong-state/state assoc-in [:bci :pending-record] nil)
       (start-recording! pending-record))

     ;; Handle pending recording stop
     (when pending-stop
       (swap! pong-state/state assoc-in [:bci :pending-stop-record] nil)
       (stop-recording!))

     nil)))

(e/defn PollBrainActivity []
  (e/client
   ; This triggers the function to run on every animation frame
   (let [_ (dom/on-animation-frame)
         state (e/watch pong-state/state)
         matching? (get-in state [:bci :matching?])]

     ; Only poll for brain activity when matching is active
     (when matching?
       ; Get confidence data from server - this automatically runs when needed
       (let [confidence-data (get-confidence-data)]
         (when confidence-data
           (js/console.log "Received confidence data:" (clj->js confidence-data))
           (swap! pong-state/state assoc-in [:bci :confidence] confidence-data)

           ; Apply paddle movement based on confidence
           (process-bci-input!)))))))

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

(e/defn bci-game-loop-tick []
  (e/client
   (process-bci-input!)))