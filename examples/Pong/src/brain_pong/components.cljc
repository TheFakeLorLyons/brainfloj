(ns brain-pong.components
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [floj.profiles :as profiles])
   [brain-pong.game-state :as pong-state]
   [brain-pong.bci-integration :as bci]))

#_(e/defn ConnectionIndicator []
    (e/client
     (let [state (e/watch pong-state/state)
           connected? (get-in state [:bci :device-connected?])]
       (dom/div
        (dom/props {:class (str "connection-indicator " (if connected? "connected" "disconnected"))})
        (dom/span
         (dom/props {:class "indicator-dot"})
         (dom/text (if connected? "Connected" "Disconnected")))))))

#_(e/defn ConfidenceMeter [props]
    (e/client
     (let [label (:label props)
           value (:value props)
           bg-color (cljs.core/cond
                      (> value 0.7) "#4CAF50"
                      (> value 0.5) "#FFC107"
                      :else "#F44336")
           style {:width (str (* 100 value) "%")
                  :background-color bg-color}]
       (dom/div
        (dom/props {:class "confidence-meter"})
        (dom/div (dom/props {:class "label"}) (dom/text label))
        (dom/div
         (dom/props {:class "meter"})
         (dom/div
          (dom/props {:class "fill"
                      :style style})))
        (dom/div (dom/props {:class "value"})
                 (dom/text (str (int (* 100 value)) "%")))))))

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
                             (str "Connected (" active-profile ")")
                             "Disconnected")))
       
       (dom/text "Profile Connected?" (:active-profile @pong-state/state)))

      ; Recording status (if relevant)
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

(e/defn ConnectWhenPending []
  (e/client
   (let [state (e/watch pong-state/state)]
     (when (get-in state [:bci :pending-connect])
       (js/console.log "pending connect!")
       (swap! pong-state/state assoc-in [:bci :pending-connect] false)
       (e/server
        (bci/connect-device-server))))))

(e/defn DisconnectWhenPending []
  (e/client
   (let [state (e/watch pong-state/state)]
     (when (get-in state [:bci :pending-disconnect])
       (js/console.log "DisconnectWhenPending running")
       (swap! pong-state/state assoc-in [:bci :pending-disconnect] false)
       (bci/disconnect-device!)))))

(e/defn BrainControls []
  (e/client
   (let [state (e/watch pong-state/state)
         connected? (get-in state [:bci :device-connected?])
         matching? (get-in state [:bci :match-interval])]

     (ConnectWhenPending)
     (DisconnectWhenPending)

     (dom/div
      (dom/props {:class "bci-controls"})
      (dom/button
       (dom/props {:class (str "bci-button " (if connected? "disconnect" "connect"))})
       (dom/text (if connected? "Disconnect Device" "Connect Device"))
       (dom/On "click" (fn [_]
                         (js/console.log "BCI button clicked, connected?" connected?)
                         (if connected?
                           #?(:cljs (bci/handle-disconnect-click))
                           #?(:cljs (bci/handle-connect-click))))
               nil)
       (dom/text (str " " (get-in state [:bci :active-profile]))))))))

#_(e/defn ThresholdAdjustment []
  (e/client
   (let [state (e/watch pong-state/state)
         threshold (get-in state [:bci :threshold] 0.6)
         sensitivity (get-in state [:bci :sensitivity] 0.5)]

     (dom/div
      (dom/props {:class "threshold-adjustment"})
      (dom/h3 (dom/text "BCI Settings"))

       ;; Confidence threshold slider
      (dom/div
       (dom/props {:class "slider-control"})
       (dom/label
        (dom/props {:for "threshold-slider"})
        (dom/text "Confidence Threshold: " (.toFixed threshold 2)))
       (dom/input
        (dom/props {:type "range"
                    :id "threshold-slider"
                    :min "0.1"
                    :max "0.9"
                    :step "0.05"
                    :value threshold})
        (dom/On "input" (e/fn [e]
                          (let [new-val (js/parseFloat (.. e -target -value))]
                            (bci/update-threshold! new-val)))
                nil)))

       ;; Movement sensitivity slider
      (dom/div
       (dom/props {:class "slider-control"})
       (dom/label (dom/props {:for "sensitivity-slider"})
                  (dom/text "Movement Sensitivity: " (.toFixed sensitivity 2)))
       (dom/input
        (dom/props {:type "range"
                    :id "sensitivity-slider"
                    :min "0.1"
                    :max "1.0"
                    :step "0.05"
                    :value sensitivity})
        (dom/On "input" (e/fn [e]
                          (let [new-val (js/parseFloat (.. e -target -value))]
                            (bci/update-sensitivity! new-val)))
                nil)))))))


#_(e/defn CalibrationButton []
  (e/client
   (dom/div
    (dom/props {:class "calibration-control"})
    (dom/button
     (dom/props {:class "calibration-button"})
     (dom/text "Calibrate BCI Controls")
     (dom/On "click" (e/fn [_]
                       (dom/div
                        (dom/props {:id "calibration-modal" :class "modal"})
                        (dom/div
                         (dom/props {:class "modal-content"})
                         (dom/h2 (dom/text "BCI Calibration"))
                         (dom/p (dom/text "First, we'll calibrate the UP movement:"))
                         (dom/button
                          (dom/props {:class "record-button"})
                          (dom/text "Record UP Movement")
                          (dom/On "click" (e/fn [_]
                                            (bci/start-recording! "pong-up")
                                            (js/setTimeout #(bci/stop-recording!) 5000))
                                  nil))
                         (dom/p (dom/text "Next, we'll calibrate the DOWN movement:"))
                         (dom/button
                          (dom/props {:class "record-button"})
                          (dom/text "Record DOWN Movement")
                          (dom/On "click" (e/fn [_]
                                            (bci/start-recording! "pong-down")
                                            (js/setTimeout #(bci/stop-recording!) 5000))
                                  nil))
                         (dom/button
                          (dom/props {:class "close-button"})
                          (dom/text "Close")
                          (dom/On "click" (e/fn [_]
                                            (let [modal (js/document.getElementById "calibration-modal")]
                                              (set! (.. modal -style -display) "none")))
                                  nil)))))
             nil)))))

;; Brain Calibration Component
#_ (e/defn CalibrationControl []
   (e/client
    (let [state (e/watch pong-state/state)
          connected? (get-in state [:bci :device-connected?])
          recording? (get-in state [:bci :recording?])
          current-category (get-in state [:bci :current-category])]

      (dom/div
       (dom/props {:class "calibration-control"})
       (dom/h3 (dom/text "Calibration"))

      ; Only show calibration controls if connected
       (if connected?
         (dom/div
         ; If not recording, show start recording buttons
          (if-not recording?
            (dom/div
             (dom/props {:class "recording-buttons"})
             (dom/button
              (dom/props {:class "calibration-button up-calib"})
              (dom/text "Record UP Pattern")
              (dom/On "click" (e/fn [_]
                                (bci/start-recording! "pong-up"))
                      nil))

             (dom/button
              (dom/props {:class "calibration-button down-calib"})
              (dom/text "Record DOWN Pattern")
              (dom/On "click" (e/fn [_]
                                (bci/start-recording! "pong-down"))
                      nil)))

           ; If recording, show stop button
            (dom/div
             (dom/props {:class "recording-status-control"})
             (dom/p
              (dom/props {:class "recording-text"})
              (dom/text "Recording " current-category " pattern..."))
             (dom/p
              (dom/props {:class "recording-instruction"})
              (dom/text "Think about moving the paddle "
                        (if (= current-category "pong-up") "UP" "DOWN")
                        " while recording"))
             (dom/button
              (dom/props {:class "stop-recording-button"})
              (dom/text "Stop Recording")
              (dom/On "click" (e/fn [_]
                                (bci/stop-recording!))
                      nil)))))

        ; Not connected - show message
         (dom/p
          (dom/props {:class "calibration-message"})
          (dom/text "Connect a BCI device to calibrate")))))))

; Combined BCI Panel
(e/defn BCIPanel []
  (e/client
   ; Initialize BCI on component mount
   #_(e/server (bci/initialize-bci!))

   (dom/div
    (dom/props {:class "bci-panel"})
    (dom/h2 (dom/text "Brain-Computer Interface"))
    #_(dom/text (e/server (:name (profiles/get-active-profile))));example
    (BrainConnectionStatus)
    (BrainControls)
    #_(CalibrationControl)
    #_(ThresholdAdjustment))))

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
   (dom/div
    (dom/props {:class "instructions"})
    (dom/h3 (dom/text "How to Play"))
    (dom/ul
     (dom/li (dom/text "Use ↑ and ↓ arrow keys to move your paddle"))
     (dom/li (dom/text "First to 5 points wins"))
     (dom/li (dom/text "Click 'Start Game' to begin"))))))

(e/defn DebugPanel []
  (e/client
   (let [state (e/watch pong-state/state)
         playing? (get-in state [:game :playing?])
         keys-pressed (:keys-pressed state)]
     (dom/div
      (dom/props {:class "debug-panel"
                  :style {:position "fixed"
                          :bottom "10px"
                          :left "10px"
                          :background "rgba(0,0,0,0.7)"
                          :color "white"
                          :padding "10px"
                          :border-radius "5px"
                          :font-family "monospace"
                          :z-index "1000"}})
      (dom/h4 (dom/text "Debug Info"))
      (dom/div (dom/text (str "Game Playing: " playing?)))
      (dom/div (dom/text (str "Keys Pressed: " (pr-str keys-pressed))))
      (dom/div (dom/text (str "Ball Position: ("
                              (get-in state [:game :ball :x])
                              ", "
                              (get-in state [:game :ball :y])
                              ")")))))))

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

#_(e/defn Pong []
    (e/client
     (let [state (e/watch pong-state/state)
           !current-screen (:current-screen (:app state))
           current-screen (e/watch !current-screen)

           menu-actions {:start-game (e/fn []
                                       (reset! !current-screen :game)
                                       (reset-game!)
                                       (start-game!))
                         :show-bci-setup #(reset! !current-screen :bci-setup)
                         :show-instructions #(reset! !current-screen :instructions)
                         :exit-game #(js/console.log "Exit game - not applicable in browser")}]
       (dom/div
        (dom/props {:class "bci-pong"})
        (dom/h1 (dom/text "BCI Pong Game"))

        (case current-screen
          :main-menu (MainScreen menu-actions)
          :game (GameScreen #(reset! !current-screen :main-menu))
           ; :bci-setup (DeviceConnectionPanel #(reset! !current-screen :main-menu))
          :instructions (InstructionsScreen #(reset! !current-screen :main-menu))

           ; Default fallback
          (MainScreen menu-actions))))))