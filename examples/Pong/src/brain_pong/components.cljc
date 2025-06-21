(ns brain-pong.components
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [brain-pong.game-state :as pong-state]
            [brain-pong.bci-integration :as bci]
            [brain-pong.training-wheels :as wheels]
            #?(:clj [floj.profiles :as profiles])
            #?(:clj [brain-pong.signature :as signature])))

(e/defn BrainConnectionStatus []
  (e/client
   (let [state (e/watch pong-state/state)
         connected? (get-in state [:bci :device-connected?])
         active-profile (get-in state [:bci :active-profile])
         recording? (get-in state [:bci :recording?])
         current-category (get-in state [:bci :current-category])
         streaming? (get-in state [:bci :streaming?])
         matching? (get-in state [:bci :matching?])]

     (dom/div
      (dom/props {:class "bci-status"})

      ; Connection status
      (dom/div
       (dom/props {:class (str "status-indicator " (if connected? "connected" "disconnected"))})
       (dom/span (dom/props {:class "status-dot"}) (dom/text ""))
       (dom/span (dom/props {:class "status-text"})
                 (dom/text (if connected?
                             (str "Connected" (when active-profile (str " (" active-profile ")")))
                             "Disconnected"))))

      ; Recording status
      (when recording?
        (dom/div
         (dom/props {:class "recording-status"})
         (dom/text (str "Recording " current-category))))

      ; Streaming status
      (when streaming?
        (dom/div
         (dom/props {:class "streaming-status"})
         (dom/text "EEG Streaming Active")))

      ; Matching status
      (when matching?
        (dom/div
         (dom/props {:class "matching-status"})
         (dom/text "Brain Control Enabled")))))))

(e/defn ConnectWhenPending []
  (e/client
   (let [state (e/watch pong-state/state)]
     (when (get-in state [:bci :pending-connect])
       (js/console.log "Initiating BCI connection!")

       ; Clear the pending state first
       (swap! pong-state/state assoc-in [:bci :pending-connect] false)

       (let [result (e/server (bci/connect-device-server))]
         (js/console.log "Connection result:" (clj->js result))
         (if (:connected result)
           (do
             (swap! pong-state/state update-in [:bci] merge
                    {:device-connected? (:connected result)
                     :active-profile (:profile-name result)
                     :connection-error nil})
             (js/console.log "Connection successful"))
           (do
             (swap! pong-state/state update-in [:bci] merge
                    {:device-connected? false
                     :active-profile nil
                     :connection-error (:error result)})
             (js/console.log "Connection failed:" (:error result)))))))))

(e/defn DisconnectWhenPending []
  (e/client
   (let [state (e/watch pong-state/state)]
     (when (get-in state [:bci :pending-disconnect])
       (js/console.log "Initiating BCI disconnection")

       ; Clear the pending state first
       (swap! pong-state/state assoc-in [:bci :pending-disconnect] false)

       ; Call the disconnection function
       (let [result (e/server (bci/disconnect-device-server))]
         (js/console.log "Disconnect result:" (clj->js result))
         (swap! pong-state/state update-in [:bci] merge
                {:device-connected? false
                 :active-profile nil
                 :matching? false
                 :connection-error nil})

         ; Clear any running intervals
         (when-let [interval (get-in @pong-state/state [:bci :match-interval])]
           (js/clearInterval interval)
           (swap! pong-state/state assoc-in [:bci :match-interval] nil)))))))

(e/defn AsyncBCIStateManager []
  (e/client
   (let [state (e/watch pong-state/state)
         playing? (get-in state [:game :playing?])
         connected? (get-in state [:bci :device-connected?])
         streaming? (get-in state [:bci :streaming?])
         pipeline-running? (get-in state [:bci :pipeline-running?])
         matching? (get-in state [:bci :matching?])
         profile-name (or (get-in state [:bci :user-profile :name]) "default")]

     ; Game started with BCI connected - start EEG streaming first
     (when (and playing? connected? (not streaming?) (not matching?))
       (js/console.log "🎯 Game started with BCI connected - starting EEG streaming server")
       (let [stream-result (e/server (signature/start-eeg-streaming-server))]
         (js/console.log "📡 EEG Stream start result:" (clj->js stream-result))
         (when (:success stream-result)
           (js/console.log "✅ EEG streaming server started successfully")
           (swap! pong-state/state assoc-in [:bci :streaming?] true)

           ; Small delay to ensure streaming is established
           (js/setTimeout #(js/console.log "📊 EEG streaming should now be active") 500))))

     ; EEG streaming active - now start the optimized pipeline
     (when (and playing? connected? streaming? (not matching?) (not pipeline-running?))
       (js/console.log "🚀 EEG streaming active - starting optimized BCI pipeline")
       (let [start-result (e/server (bci/start-optimized-bci-pipeline! profile-name "pong"))]
         (js/console.log "🏭 Pipeline start result:" (clj->js start-result))
         (when (:started start-result)
           (js/console.log "✅ Optimized BCI pipeline started - enabling brain control")

           ; Set flags atomically on the client side
           (swap! pong-state/state
                  (fn [current-state]
                    (-> current-state
                        (assoc-in [:bci :pipeline-running?] true)
                        (assoc-in [:bci :matching?] true))))
           (js/console.log "🔧 Client state updated - pipeline-running? should now be true")

           ; Verify the update worked
           (let [updated-state @pong-state/state]
             (js/console.log "🔍 Verification - pipeline-running?:" (get-in updated-state [:bci :pipeline-running?]))
             (js/console.log "🔍 Verification - matching?:" (get-in updated-state [:bci :matching?]))))))

     ; Game stopped - stop everything in reverse order
     (when (and (not playing?) (or streaming? matching?))
       (js/console.log "⏹️ Game stopped - stopping BCI pipeline and streaming")

       ; Stop the pipeline
       (when matching?
         (js/console.log "🛑 Stopping optimized BCI pipeline")
         (e/server (bci/stop-optimized-bci-pipeline!))
         (swap! pong-state/state assoc-in [:bci :matching?] false))

       ; Stop the EEG streaming
       (when streaming?
         (js/console.log "Stopping EEG streaming server")
         (let [stop-result (e/server (signature/stop-eeg-streaming-server))]
           (js/console.log "EEG Stream stop result:" (clj->js stop-result))
           (swap! pong-state/state assoc-in [:bci :streaming?] false))))

     ; Device disconnected - emergency stop everything
     (when (and (not connected?) (or streaming? matching?))
       (js/console.log "Device disconnected - emergency stop of all BCI activity")
       (e/server (bci/stop-optimized-bci-pipeline!))
       (e/server (signature/stop-eeg-streaming-server))
       (swap! pong-state/state update-in [:bci] merge
              {:streaming? false
               :matching? false})))))

(e/defn ReactiveServerBCIPoll []
  (e/server
   (let [state (e/watch pong-state/state)
         bci-state (:bci state)
         latest-output (:latest-output bci-state)]
     (when (and (:running? bci-state) latest-output)

       {:up (:up latest-output)
        :down (:down latest-output)
        :confidence (:confidence latest-output)
        :timestamp (:timestamp latest-output)
        :processing-mode (:processing-mode latest-output)
        :fresh? (< (- (System/currentTimeMillis) (:timestamp latest-output 0)) 1000)}))))

(e/defn AsyncBCIDataPoller []
  (e/client
   (let [state (e/watch pong-state/state)
         connected? (get-in state [:bci :device-connected?])
         streaming? (get-in state [:bci :streaming?])
         matching? (get-in state [:bci :matching?])
         pipeline-running? (get-in state [:bci :pipeline-running?])
         should-poll? (and connected? streaming? matching? pipeline-running?)
         poll-trigger (get-in state [:bci :poll-trigger] 0)
         server-response (when should-poll?
                           (ReactiveServerBCIPoll))
         poll-interval-atom (atom nil)
         current-interval (e/watch poll-interval-atom)]

     (when (and server-response should-poll?)
       (when (and (:up server-response) (:down server-response))
         (let [current-time (js/Date.now)
               enhanced-response (merge server-response
                                        {:client-received-time current-time
                                         :age (- current-time (:timestamp server-response 0))
                                         :fresh? (< (- current-time (:timestamp server-response 0)) 1000)
                                         :client-processed true})]
           (js/console.log "Client updating state with enhanced response:" (clj->js enhanced-response))

           (swap! pong-state/state
                  (fn [current-state]
                    (-> current-state
                        (assoc-in [:bci :confidence] enhanced-response)
                        (assoc-in [:bci :latest-output] enhanced-response)
                        (update-in [:bci :action-count] (fnil inc 0)))))

           (let [updated-confidence (get-in @pong-state/state [:bci :confidence])]))))

     ; Start polling when conditions are met
     (when (and should-poll? (not current-interval))
       (js/console.log "Starting BCI polling...")
       (let [base-interval 150
             interval-id (js/setInterval
                          (fn []
                            (let [new-trigger (inc (get-in @pong-state/state [:bci :poll-trigger] 0))]
                              (js/console.log "🔄 Poll trigger #" new-trigger)
                              (swap! pong-state/state assoc-in [:bci :poll-trigger] new-trigger)))
                          base-interval)]
         (reset! poll-interval-atom interval-id)))

     ; Stop polling when conditions change
     (when (and (not should-poll?) current-interval)
       (js/console.log "Stopping BCI polling...")
       (js/clearInterval current-interval)
       (reset! poll-interval-atom nil)))))

   (e/defn BrainActivityPoller []
     (e/client
      (let [state (e/watch pong-state/state)
            connected? (get-in state [:bci :device-connected?])
            matching? (get-in state [:bci :matching?])
            streaming? (get-in state [:bci :streaming?])
            should-poll? (and connected? streaming? matching?)
            profile-name (or (get-in state [:bci :user-profile :name]) "default")
            poll-interval-atom (atom nil)
            current-interval (e/watch poll-interval-atom)]

        ; Cleanup on unmount
        (e/on-unmount
         #(when current-interval
            (js/clearInterval current-interval)
            (reset! poll-interval-atom nil)))

        ; Start polling when conditions are met
        (when (and should-poll? (not current-interval))
          (js/console.log "Starting BCI polling...")
          (let [base-interval 200
                interval-id (js/setInterval
                             (fn []
                               (let [current-time (js/Date.now)]
                                 (swap! pong-state/state update-in [:bci :poll-trigger] (fnil inc 0))
                                 (swap! pong-state/state assoc-in [:bci :last-poll-time] current-time)))
                             base-interval)]
            (reset! poll-interval-atom interval-id)))

        ; Stop polling when conditions are no longer met
        (when (and (not should-poll?) current-interval)
          (js/console.log "Stopping BCI polling...")
          (js/clearInterval current-interval)
          (reset! poll-interval-atom nil))

        ; Polling logic that handles both basic matching AND training
        (when should-poll?
          (let [poll-trigger (get-in state [:bci :poll-trigger])]
            (when poll-trigger
              (let [start-time (js/performance.now)
                    recent-performance (bci/calculate-recent-performance state)
                    calc-time (- (js/performance.now) start-time)
                    game-context {:ball-position-validates-intent
                                  (let [ball (get-in state [:game :ball])
                                        paddle (get-in state [:game :player-paddle])]
                                    (and ball paddle
                                         (< (Math/abs (- (:x ball) (:x paddle))) 200)))
                                  :game-active (get-in state [:game :playing?])
                                  :timestamp (js/Date.now)}

                    server-start (js/performance.now)
                    unified-response (e/server
                                      (e/Offload
                                       (fn []
                                      ;(println "Server: Processing brain activity #" poll-trigger)
                                         (bci/match-brain-activity-server
                                          profile-name "pong" game-context recent-performance))))
                    server-time (- (js/performance.now) server-start)]

                (when unified-response
                  ; Update confidence structure to include all server data
                  (when (and (:up unified-response) (:down unified-response))
                    (let [current-threshold (get-in @pong-state/state [:bci :confidence :dynamic-threshold] 0.05)]
                      (swap! pong-state/state assoc-in [:bci :confidence]
                             {:up (:up unified-response)
                              :down (:down unified-response)
                              :confidence (:confidence unified-response) ; Use :confidence not :overall
                              :dynamic-threshold (or (:dynamic-threshold unified-response) current-threshold)
                              :confidence-gap (:confidence-gap unified-response)
                              :intelligence-grade (:intelligence-grade unified-response)
                              :separation-score (:separation-score unified-response)
                              :triangulation-quality (:triangulation-quality unified-response)})))

                  ; Store training assistance for game loop
                  (when (:training-assistance unified-response)
                    (swap! pong-state/state assoc-in [:bci :training-assistance]
                           (:training-assistance unified-response)))

                  ; Track enhancements
                  (when (:signature-enhanced unified-response)
                    (swap! pong-state/state update-in [:bci :enhancement-count] (fnil inc 0)))

                  ; Update base threshold periodically (every 10 cycles)
                  (when (= 0 (mod poll-trigger 10))
                    (js/console.log "Updating base threshold from server...")
                    (let [server-threshold (e/server (wheels/Get-adaptive-threshold "pong"))]
                      (js/console.log "New server base threshold:" server-threshold)
                      (swap! pong-state/state assoc-in [:bci :confidence :dynamic-threshold] server-threshold)
                      (swap! pong-state/state assoc-in [:bci :server-base-threshold] server-threshold)))))))
          nil))))

   (e/defn BrainControls []
     (e/client
      (let [state (e/watch pong-state/state)
            connected? (get-in state [:bci :device-connected?])
            playing? (get-in state [:game :playing?])
            pending-connect? (get-in state [:bci :pending-connect])
            pending-disconnect? (get-in state [:bci :pending-disconnect])
            active-profile (get-in state [:bci :active-profile])
            connection-error (get-in state [:bci :connection-error])
            matching? (get-in state [:bci :matching?])
            streaming? (get-in state [:bci :streaming?])
            confidence (get-in state [:bci :confidence])]

        (ConnectWhenPending)
        (DisconnectWhenPending)
        (AsyncBCIStateManager)
        (AsyncBCIDataPoller)

        (dom/div
         (dom/props {:class "bci-controls"})

         ; Connection button
         (dom/button
          (dom/props {:class (str "bci-button " (if connected? "disconnect" "connect"))
                      :disabled (or pending-connect? pending-disconnect?)})
          (dom/text (cond pending-connect? "Connecting..."
                          pending-disconnect? "Disconnecting..."
                          connected? "Disconnect Device"
                          :else "Connect Device"))
          (dom/On "click"
                  (fn []
                    (if connected?
                      (swap! pong-state/state assoc-in [:bci :pending-disconnect] true)
                      (swap! pong-state/state assoc-in [:bci :pending-connect] true)))
                  nil))

         ; Show active profile info and BCI status
         (when (and connected? active-profile)
           (dom/div
            (dom/props {:class "profile-info"})
            (dom/text (str "Profile: " active-profile))
            (when streaming?
              (dom/span
               (dom/props {:class "streaming-indicator"})
               (dom/text " • Streaming")))
            (when (and matching? playing?)
              (dom/span
               (dom/props {:class "matching-indicator"})
               (dom/text " • Brain Control Active")))))

         ; Show BCI status message when connected but not playing
         (when (and connected? (not playing?))
           (dom/div
            (dom/props {:class "bci-status-message"})
            (dom/text "BCI ready - Start the game to enable brain control")))

         ; Show brain activity confidence when matching
         (when (and connected? matching? confidence)
           (dom/div
            (dom/props {:class "confidence-display"})
            (dom/div (dom/text (str "Up: " (.toFixed (or (:up confidence) 0.0) 3))))
            (dom/div (dom/text (str "Down: " (.toFixed (or (:down confidence) 0.0) 3))))
            (dom/div (dom/text (str "Overall: " (.toFixed (or (:confidence confidence) 0.0) 3))))
            (dom/div (dom/text (str "Data age: " (or (:age confidence) 0) "ms")))
            (when (:fresh? confidence)
              (dom/div
               (dom/props {:class "fresh-indicator"})
               (dom/text "🟢 Fresh data")))
            (when-let [error (:error confidence)]
              (dom/div
               (dom/props {:class "confidence-error"})
               (dom/text (str "Error: " error))))))

         ; Show connection error if any
         (when connection-error
           (dom/div
            (dom/props {:class "connection-error"})
            (dom/text (str "Error: " connection-error))))))))

   ; Simplified status checker
   (e/defn CheckConnectionStatus []
     (e/client
      (let [status (e/server (bci/get-device-status-server))]
        (js/console.log "Status result:" (clj->js status))
        (when (:connected status)
          (let [current-state (get-in @pong-state/state [:bci])
                new-connected? (:connected status)
                new-profile (:profile-name status)]
            ; Only update if there's actually a change
            (when (or (not= (:device-connected? current-state) new-connected?)
                      (not= (:active-profile current-state) new-profile))
              (js/console.log "Updating status - Connected:" new-connected? "Profile:" new-profile)
              (swap! pong-state/state update-in [:bci] merge
                     {:device-connected? new-connected?
                      :active-profile new-profile}))))
        status)))

   (e/defn ThresholdAdjustment []
     (e/client
      (let [state (e/watch pong-state/state)
            threshold (get-in state [:bci :threshold] 0.05)
            sensitivity (get-in state [:bci :sensitivity] 0.5)]
        (dom/div
         (dom/props {:class "threshold-adjustment"})
         (dom/h3 (dom/text "Fine Tuning"))

         ; Threshold slider
         (dom/div
          (dom/props {:class "slider-group"})
          (dom/label (dom/text (str "Threshold: " (.toFixed threshold 2))))
          (dom/input
           (dom/props {:type "range"
                       :min 0.1
                       :max 0.95
                       :step 0.05
                       :value threshold})
           (dom/On "input" (fn [e]
                             (let [value (js/parseFloat (.. e -target -value))]
                               (swap! pong-state/state assoc-in [:bci :threshold] value)))
                   nil)))

         ; Sensitivity slider
         (dom/div
          (dom/props {:class "slider-group"})
          (dom/label (dom/text (str "Sensitivity: " (.toFixed sensitivity 2))))
          (dom/input
           (dom/props {:type "range"
                       :min 0.1
                       :max 1.0
                       :step 0.05
                       :value sensitivity})
           (dom/On "input" (fn [e]
                             (let [value (js/parseFloat (.. e -target -value))]
                               (swap! pong-state/state assoc-in [:bci :sensitivity] value)))
                   nil)))))))

   (e/defn BCIPanel []
     (e/client
      (dom/div
       (dom/props {:class "bci-panel"})
       (dom/h2 (dom/text "Brain-Computer Interface"))
       (BrainConnectionStatus)
       (BrainControls)
       #_(ThresholdAdjustment))))

   (e/defn BrainSignalVisualization []
     (e/client
      (let [state (e/watch pong-state/state)
            connected? (get-in state [:bci :device-connected?])
            confidence (get-in state [:bci :confidence] {:up 0.0 :down 0.0})
            threshold (get-in state [:bci :threshold] 0.6)]

        (when connected?
          (dom/div
           (dom/props {:class "brain-signal-viz"})
           (dom/h3 (dom/text "Brain Signal"))

           ; Signal bars visualization
           (dom/div
            (dom/props {:class "signal-bars"})

            ; Up signal
            (dom/div
             (dom/props {:class "signal-bar"})
             (dom/div
              (dom/props {:class "bar-label"})
              (dom/text "Up"))
             (dom/div
              (dom/props {:class "bar-container"})
              (dom/div
               (dom/props {:class (str "bar-fill" (when (>= (:up confidence) threshold) " active"))
                           :style (str "width: " (* 100 (:up confidence)) "%")}))
              (dom/div
               (dom/props {:class "threshold-marker"
                           :style (str "left: " (* 100 threshold) "%")}))))

            ; Down signal
            (dom/div
             (dom/props {:class "signal-bar"})
             (dom/div
              (dom/props {:class "bar-label"})
              (dom/text "Down"))
             (dom/div
              (dom/props {:class "bar-container"})
              (dom/div
               (dom/props {:class (str "bar-fill" (when (>= (:down confidence) threshold) " active"))
                           :style (str "width: " (* 100 (:down confidence)) "%")}))
              (dom/div
               (dom/props {:class "threshold-marker"
                           :style (str "left: " (* 100 threshold) "%")}))))))))))

   (e/defn SimpleStartButton []
     (e/client
      (dom/button
       (dom/props {:class "control-button start-button"})
       (dom/text "Start Game")
       (dom/On "click" (fn [_]
                         (pong-state/reset-game!)
                         (pong-state/init-keyboard-controls!)
                         (pong-state/reset-ball!)
                         (swap! pong-state/state assoc-in [:game :playing?] true)
                         (js/console.log "Game start, state:"
                                         (pr-str (get-in @pong-state/state [:game :playing?]))))
               nil))))

   (e/defn SimpleStopButton []
     (e/client
      (dom/button
       (dom/props {:class "control-button stop-button"})
       (dom/text "Stop Game")
       (dom/On "click" (fn [_]
                         (pong-state/reset-game!)
                         (pong-state/init-keyboard-controls!)
                         (swap! pong-state/state assoc-in [:game :playing?] false)
                         (js/console.log "Game stop, state:"
                                         (pr-str (get-in @pong-state/state [:game :playing?]))))
               nil))))

   (e/defn SimplePongControls []
     (e/client
      (let [state (e/watch pong-state/state)
            playing? (get-in state [:game :playing?])]
        (dom/div
         (dom/props {:class "game-controls simple-controls"})
         (if playing?
           (SimpleStopButton)
           (SimpleStartButton))))))

   (e/defn ScoreDisplay [ai-score player-score]
     (dom/div
      (dom/props {:class "score"})
      (dom/div (dom/props {:class "ai-score"})
               (dom/text (str "AI: " ai-score)))
      (dom/div (dom/props {:class "player-score"})
               (dom/text (str "Player: " player-score)))))

   (e/defn PongCourt []
     (e/client
      (let [state (e/watch pong-state/state)
            game (:game state)
            {:keys [court ball player-paddle ai-paddle score playing?]} game
            {:keys [width height]} court
            {:keys [x y radius]} ball
            player-score (:player score)
            ai-score (:ai score)]

        (dom/div
         (dom/props {:class "court-container"})

         (ScoreDisplay ai-score player-score)

         (SimplePongControls)

         (dom/div
          (dom/props {:class "court"
                      :style {:width (str width "px")
                              :height (str height "px")}})

          ; Center line
          (dom/div (dom/props {:class "center-line"}))

          ; Ball (dynamic position)
          (dom/div
           (dom/props {:class "ball"
                       :style {:left (str (- x radius) "px")
                               :top (str (- y radius) "px")
                               :width (str (* 2 radius) "px")
                               :height (str (* 2 radius) "px")}}))

          ; AI paddle
          (dom/div
           (dom/props {:class "paddle ai-paddle"
                       :style {:left (str (:x ai-paddle) "px")
                               :top (str (:y ai-paddle) "px")
                               :height (str (:height ai-paddle) "px")
                               :width (str (:width ai-paddle) "px")}}))

          ; Player paddle
          (dom/div
           (dom/props {:class "paddle player-paddle"
                       :style {:left (str (:x player-paddle) "px")
                               :top (str (:y player-paddle) "px")
                               :height (str (:height player-paddle) "px")
                               :width (str (:width player-paddle) "px")}})))))))

   (e/defn Instructions []
     (e/client
      (let [state (e/watch pong-state/state)
            connected? (get-in state [:bci :device-connected?])
            active-profile (get-in state [:bci :active-profile])
            matching? (get-in state [:bci :matching?])]
        (dom/div
         (dom/props {:class "instructions"})
         (dom/h2 (dom/text "Brain-Controlled Pong"))

         ; Game Instructions
         (dom/div
          (dom/props {:class "game-instructions"})
          (dom/h3 (dom/text "How to Play"))
          (dom/p (dom/text "Control the paddle on the right side to hit the ball. Score by making the ball pass the AI paddle on the left."))

          (if connected?
            ; BCI control instructions when connected
            (dom/div
             (dom/props {:class "bci-instructions"})
             (dom/p (dom/text (str "Connected to BCI device with profile: " active-profile)))
             (if matching?
               (dom/div
                (dom/p (dom/text "Brain control is ACTIVE!"))
                (dom/p (dom/text "Think about moving UP to move the paddle up"))
                (dom/p (dom/text "Think about moving DOWN to move the paddle down"))
                (dom/p (dom/text "You can adjust sensitivity and threshold in the controls panel")))
               (dom/div
                (dom/p (dom/text "Click 'Start Brain Control' to control with your mind"))
                (dom/p (dom/text "First, use the calibration tools to record your brain patterns")))))

            ; Keyboard fallback when not connected
            (dom/div
             (dom/props {:class "keyboard-instructions"})
             (dom/p (dom/text "Currently using keyboard controls:"))
             (dom/ul
              (dom/li (dom/text "↑ Up Arrow: Move paddle up"))
              (dom/li (dom/text "↓ Down Arrow: Move paddle down")))
             (dom/p (dom/text "Connect your BCI device to control with your mind!")))))))))

   (e/defn DebugPanel []
     (e/client
      (let [state (e/watch pong-state/state)
            debug-visible? (or (get-in state [:app :debug-visible?]) false)]

        (dom/div
         (dom/props {:class "debug-panel-toggle"})
         (dom/button
          (dom/text (if debug-visible? "Hide Debug" "Show Debug"))
          (dom/On "click" (fn [_]
                            (swap! pong-state/state update-in [:app :debug-visible?] not))
                  nil)))

        (when debug-visible?
          (dom/div
           (dom/props {:class "debug-panel"})
           (dom/h3 (dom/text "Debug Information"))

           ; BCI Information
           (dom/div
            (dom/props {:class "debug-section"})
            (dom/h4 (dom/text "BCI State"))
            (dom/pre
             (dom/text (str "Device Connected: " (get-in state [:bci :device-connected?])
                            "\nActive Profile: " (get-in state [:bci :active-profile])
                            "\nRecording: " (get-in state [:bci :recording?])
                            "\nCurrent Category: " (get-in state [:bci :current-category])
                            "\nMatching Active: " (get-in state [:bci :matching?])
                            "\nThreshold: " (get-in state [:bci :threshold])
                            "\nSensitivity: " (get-in state [:bci :sensitivity])))))

           ; Game Information
           (dom/div
            (dom/props {:class "debug-section"})
            (dom/h4 (dom/text "Game State"))
            (dom/pre
             (dom/text (str "Playing: " (get-in state [:game :playing?])
                            #_#_"\nScore - Player: " (get-in state [:game :score :player])
                            #_#_"\nScore - AI: " (get-in state [:game :score :ai]))))))))))