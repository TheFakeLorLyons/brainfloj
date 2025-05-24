(ns brain-pong.components
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [floj.profiles :as profiles])
   [brain-pong.game-state :as pong-state]
   [brain-pong.bci-integration :as bci]))

(e/defn BrainConnectionStatus []
  (e/client
   (let [state (e/watch pong-state/state)
         connected? (get-in state [:bci :device-connected?])
         active-profile (get-in state [:bci :active-profile])
         recording? (get-in state [:bci :recording?])
         current-category (get-in state [:bci :current-category])
         confidence (get-in state [:bci :confidence] {:up 0.0 :down 0.0})]

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

      ; Confidence meters for up/down movements
      (when connected?
        (dom/div
         (dom/props {:class "confidence-meters"})

         ; Up confidence
         (dom/div
          (dom/props {:class "confidence-meter"})
          (dom/label (dom/text "Up: "))
          (dom/div
           (dom/props {:class "meter-bar"})
           (dom/div
            (dom/props {:class "meter-fill"
                        :style (str "width: " (* 100 (:up confidence)) "%;")}))
           (dom/span (dom/props {:class "meter-value"})
                     (dom/text (.toFixed (* 100 (:up confidence)) 1) "%")))

         ; Down confidence
          (dom/div
           (dom/props {:class "confidence-meter"})
           (dom/label (dom/text "Down: "))
           (dom/div
            (dom/props {:class "meter-bar"})
            (dom/div
             (dom/props {:class "meter-fill"
                         :style (str "width: " (* 100 (:down confidence)) "%;")}))
            (dom/span (dom/props {:class "meter-value"})
                      (dom/text (.toFixed (* 100 (:down confidence)) 1) "%")))))))))))

;; Simplified connection handler that uses the new bci functions directly
(e/defn ConnectWhenPending []
  (e/client
   (let [state (e/watch pong-state/state)]
     (when (get-in state [:bci :pending-connect])
       (js/console.log "Initiating BCI connection!")
       ; Clear the pending state first
       (swap! pong-state/state assoc-in [:bci :pending-connect] false)
       ; Call the connection function - fixed the server call

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
             (swap! pong-state/state assoc-in [:bci :match-interval] nil)))
   ))))

  (e/defn BrainControls []
    (e/client
     (let [state (e/watch pong-state/state)
           connected? (get-in state [:bci :device-connected?])
           pending-connect? (get-in state [:bci :pending-connect])
           pending-disconnect? (get-in state [:bci :pending-disconnect])
           active-profile (get-in state [:bci :active-profile])
           connection-error (get-in state [:bci :connection-error])]

       (ConnectWhenPending)
       (DisconnectWhenPending)

       (dom/div
        (dom/props {:class "bci-controls"})

      ; Connection button
        (dom/button
         (dom/props {:class (str "bci-button " (if connected? "disconnect" "connect"))
                     :disabled (or pending-connect? pending-disconnect?)})
         (dom/text (cond
                     pending-connect? "Connecting..."
                     pending-disconnect? "Disconnecting..."
                     connected? "Disconnect Device"
                     :else "Connect Device"))
         (dom/On "click"
                 (fn [_]
                   (if connected?
                     (swap! pong-state/state assoc-in [:bci :pending-disconnect] true)
                     (swap! pong-state/state assoc-in [:bci :pending-connect] true)))
                 nil))

      ; Show active profile info
        (when (and connected? active-profile)
          (dom/div
           (dom/props {:class "profile-info"})
           (dom/text (str "Profile: " active-profile))))

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
       (when (:success status)
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
           threshold (get-in state [:bci :threshold] 0.6)
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

        ;; Sensitivity slider
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

  (e/defn CalibrationControl []
    (e/client
     (let [state (e/watch pong-state/state)
           connected? (get-in state [:bci :device-connected?])
           recording? (get-in state [:bci :recording?])
           current-category (get-in state [:bci :current-category])
           matching? (get-in state [:bci :matching?])]
       (dom/div
        (dom/props {:class "calibration-controls"})
        (dom/h3 (dom/text "Brain Calibration"))

      ; Calibration instructions
        (dom/div
         (dom/props {:class "calibration-instructions"})
         (dom/text "Record your brain patterns while thinking about moving the paddle up or down."))

      ; "Up" recording button
        (dom/div
         (dom/props {:class "recording-button-group"})
         (dom/button
          (dom/props {:class (str "recording-button up-button"
                                  (when (and recording? (= current-category "up")) " active"))
                      :disabled (or (not connected?) (and recording? (not= current-category "up")))})
          (dom/text (if (and recording? (= current-category "up")) "Stop Recording Up" "Record Up"))
          (dom/On "click" (fn [_]
                            (if (and recording? (= current-category "up"))
                              (bci/stop-recording!)
                              (bci/start-recording! "up")))
                  nil))

       ; "Down" recording button  
         (dom/button
          (dom/props {:class (str "recording-button down-button"
                                  (when (and recording? (= current-category "down")) " active"))
                      :disabled (or (not connected?) (and recording? (not= current-category "down")))})
          (dom/text (if (and recording? (= current-category "down")) "Stop Recording Down" "Record Down"))
          (dom/On "click" (fn [_]
                            (if (and recording? (= current-category "down"))
                              (bci/stop-recording!)
                              (bci/start-recording! "down")))
                  nil)))

      ; Start/Stop brain activity matching
        (dom/div
         (dom/props {:class "matching-controls"})
         (dom/button
          (dom/props {:class (str "matching-button" (when matching? " active"))
                      :disabled (not connected?)})
          (dom/text (if matching? "Stop Brain Control" "Start Brain Control"))
          (dom/On "click" (fn [_]
                            (if matching?
                              (bci/stop-activity-matching!)
                              (bci/start-activity-matching!)))
                  nil)))))))

  (e/defn BCIPanel []
    (e/client
     (dom/div
      (dom/props {:class "bci-panel"})
      (dom/h2 (dom/text "Brain-Computer Interface"))
      (BrainConnectionStatus)
      (BrainControls)
      #_(CalibrationControl)
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
                        (js/console.log "Force starting game...")
                        (pong-state/reset-game!)
                        (pong-state/init-keyboard-controls!)
                        (swap! pong-state/state assoc-in [:game :playing?] true)
                        (js/console.log "Forced game start, state:"
                                        (pr-str (get-in @pong-state/state [:game :playing?]))))
              nil))))

  (e/defn SimpleStopButton []
    (e/client
     (dom/button
      (dom/props {:class "control-button stop-button"})
      (dom/text "Stop Game")
      (dom/On "click" (fn [_]
                        (js/console.log "Force starting game...")
                        (pong-state/reset-game!)
                        (pong-state/init-keyboard-controls!)
                        (swap! pong-state/state assoc-in [:game :playing?] false)
                        (js/console.log "Forced game stop, state:"
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
                           "\nSensitivity: " (get-in state [:bci :sensitivity])
                           "\nUp Confidence: " (get-in state [:bci :confidence :up])
                           "\nDown Confidence: " (get-in state [:bci :confidence :down])))))

        ; Game Information
          (dom/div
           (dom/props {:class "debug-section"})
           (dom/h4 (dom/text "Game State"))
           (dom/pre
            (dom/text (str "Playing: " (get-in state [:game :playing?])
                           "\nScore - Player: " (get-in state [:game :score :player])
                           "\nScore - AI: " (get-in state [:game :score :ai])))))

        ; Brain Signal Visualization
          (BrainSignalVisualization))))))

  #_(e/defn StateDebugging []
      (e/client
       (let [state (e/watch pong-state/state)
             playing? (get-in state [:game :playing?])]
         (dom/div
          (dom/props {:class "state-debugging"
                      :style {:position "fixed"
                              :top "10px"
                              :right "10px"
                              :background "rgba(255,0,0,0.7)"
                              :color "white"
                              :padding "10px"
                              :border-radius "5px"
                              :font-family "monospace"
                              :z-index "2000"}})
          (dom/h4 (dom/text "State Debugging"))
          (dom/div (dom/text (str "Playing? " playing?)))
          (dom/div (dom/text (str "Keys: " (pr-str (:keys-pressed state)))))
          (dom/button
           (dom/props {:style {:margin-top "10px"
                               :padding "5px"
                               :background "#4CAF50"
                               :color "white"
                               :border "none"
                               :cursor "pointer"}})
           (dom/text "Force Start")
           (dom/On "click" (fn [_]
                             (js/console.log "Force starting game...")
                             (pong-state/reset-game!)
                             (pong-state/init-keyboard-controls!)
                             (swap! pong-state/state assoc-in [:game :playing?] true)
                             (js/console.log "Forced game start, state:"
                                             (pr-str (get-in @pong-state/state [:game :playing?]))))
                   nil))))))