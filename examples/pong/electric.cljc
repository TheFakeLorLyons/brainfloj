(ns pong.electric
  (:require
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   #?(:cljs [pong.game-state :as pong-state])
   #?(:cljs [pong.bci-integration :as bci])))

#?(:cljs (js/console.log "Loading pong.electric namespace"))


(e/def !game-state
  (e/client
   #?(:cljs
      (do
        (js/console.log "Initializing reactive game state")
        ;; Initialize game here to ensure it's ready
        (when-not @pong-state/state
          (pong-state/init-game!))
        (e/watch pong-state/state)))))


(e/defn reset-game! []
  (e/client
   #?(:cljs (do
              (js/console.log "Resetting game from Electric")
              (pong-state/reset-game!)))))

(e/defn stop-game! []
  (e/client
   #?(:cljs (do
              (js/console.log "Stopping game from Electric")
              (pong-state/stop-game!)))))

(e/defn start-game! []
  (e/client
   #?(:cljs (js/console.log "Starting game..."))
   #?(:cljs (pong-state/start-game!))
   #?(:cljs (let [game-loop (fn game-loop []
                              (when (get-in @pong-state/state [:game :playing?])
                                ;; Execute one tick of the game loop
                                (pong-state/game-loop-tick!)
                                ;; Schedule next frame
                                (js/requestAnimationFrame game-loop)))]
              (game-loop)))))

(e/defn ConnectionIndicator []
  (e/client
   (let [connected? (get-in !game-state [:bci :device-connected?])]
     (dom/div
      (dom/props {:class (str "connection-indicator " (if connected? "connected" "disconnected"))})
      (dom/span
       (dom/props {:class "indicator-dot"})
       (dom/text (if connected? "Connected" "Disconnected")))))))

(e/defn GameControls []
  (e/client
   (let [playing? (get-in !game-state [:game :playing?])]
     (dom/div
      (dom/props {:class "game-controls"})
      (dom/button
       (dom/props {:class "game-button"})
       (dom/on "click"
               (e/fn [_]
                 (if playing?
                   (stop-game!)
                   (start-game!))))
       (dom/text (if playing? "Pause" "Start")))

      (dom/button
       (dom/props {:class "game-button reset"})
       (dom/on "click" (e/fn [_] (reset-game!)))
       (dom/text "Reset Game"))))))

(e/defn ConfidenceMeter [props]
  (e/client
   (let [label (:label props)
         value (:value props)]
     (dom/div
      (dom/props {:class "confidence-meter"})
      (dom/div (dom/props {:class "label"}) (dom/text label))
      (dom/div
       (dom/props {:class "meter"})
       (dom/div
        (dom/props {:class "fill"
                    :style {:width (str (* 100 value) "%")
                            :background-color (cond
                                                (> value 0.7) "#4CAF50"
                                                (> value 0.5) "#FFC107"
                                                :else "#F44336")}})))
      (dom/div (dom/props {:class "value"}) (dom/text (str (int (* 100 value)) "%")))))))

(e/defn Instructions []
  (e/client
   (dom/div
    (dom/props {:class "instructions"})
    (dom/h3 (dom/text "Instructions"))
    (dom/ul
     (dom/li (dom/text "Connect your BCI device first"))
     (dom/li (dom/text "Ensure you have enough recordings in your wave lexicon (recommended: at least 10)"))
     (dom/li (dom/text "Press start and the game will begin after 3 seconds, first to 3 wins, wins!"))))))

(e/defn DeviceConnectionPanel [on-back]
  (e/client
   (let [!connection-status (atom "disconnected")
         connection-status (e/watch !connection-status)]
     (dom/div
      (dom/props {:class "bci-panel"})
      (dom/h2 (dom/text "BCI Device Setup"))

      (dom/div
       (dom/props {:class "connection-status"})
       (dom/span (dom/text "Status: "))
       (dom/span
        (dom/props {:class (str "status-" connection-status)})
        (dom/text (case connection-status
                    "connected" "Connected"
                    "connecting" "Connecting..."
                    "failed" "Connection Failed"
                    "disconnected" "Disconnected"))))

      (dom/div
       (dom/props {:class "action-buttons"})
       (dom/button
        (dom/props {:class "bci-btn"})
        #?(:cljs (dom/on "click"
                         (e/fn [_]
                           (reset! !connection-status "connecting")
                           (bci/connect-device!)
                           #?(:cljs (js/setTimeout
                                     (fn []
                                       (if (get-in @pong-state/state [:bci :device-connected?])
                                         (reset! !connection-status "connected")
                                         (reset! !connection-status "failed")))
                                     2000)))))
        (dom/text "Connect Device"))

       (dom/button
        (dom/props {:class "bci-btn back-btn"})
        (dom/on "click" (e/fn [_] (on-back)))
        (dom/text "Back to Menu")))

      ;; Training section (only visible when connected)
      #?(:cljs (when (= connection-status "connected")
                 (dom/div
                  (dom/props {:class "training-section"})
                  (dom/h3 (dom/text "Brain Signal Training"))

                  (dom/div
                   (dom/props {:class "training-options"})
                   (dom/div
                    (dom/props {:class "training-option"})
                    (dom/h4 (dom/text "Up Movement"))
                    (dom/button
                     (dom/props {:class "training-btn"})
                     (dom/on "click" (e/fn [_] (bci/start-recording! "up")))
                     (dom/text "Record Up Signal")))

                   (dom/div
                    (dom/props {:class "training-option"})
                    (dom/h4 (dom/text "Down Movement"))
                    (dom/button
                     (dom/props {:class "training-btn"})
                     (dom/on "click" (e/fn [_] (bci/start-recording! "down")))
                     (dom/text "Record Down Signal"))))

                  (dom/button
                   (dom/props {:class "stop-btn"})
                   (dom/on "click" (e/fn [_] (bci/stop-recording!)))
                   (dom/text "Stop Recording")))))))))


(e/defn Court []
  (e/client
   #?(:cljs (let [state @pong-state/state
                  {:keys [ball player-paddle ai-paddle]} (:game state)
                  {:keys [court score bci-confidence]} (:game state)]
              (dom/div
               (dom/props {:class "court-container"})

      ;; Score display
               (dom/div
                (dom/props {:class "score"})
                (dom/div (dom/props {:class "player-score"}) (dom/text (str "Player: " (:player score))))
                (dom/div (dom/props {:class "ai-score"}) (dom/text (str "AI: " (:ai score)))))

      ;; Court
               (dom/div
                (dom/props {:class "court"
                            :style {:width (str (:width court) "px")
                                    :height (str (:height court) "px")}})

       ;; Center line
                (dom/div (dom/props {:class "center-line"}))

       ;; Ball
                (dom/div
                 (dom/props {:class "ball"
                             :style {:left (str (- (:x ball) (:radius ball)) "px")
                                     :top (str (- (:y ball) (:radius ball)) "px")
                                     :width (str (* 2 (:radius ball)) "px")
                                     :height (str (* 2 (:radius ball)) "px")}}))

       ;; Player paddle
                (dom/div
                 (dom/props {:class "paddle player-paddle"
                             :style {:left (str (:x player-paddle) "px")
                                     :top (str (:y player-paddle) "px")
                                     :height (str (:height player-paddle) "px")
                                     :width (str (:width player-paddle) "px")}}))

       ;; AI paddle
                (dom/div
                 (dom/props {:class "paddle ai-paddle"
                             :style {:left (str (:x ai-paddle) "px")
                                     :top (str (:y ai-paddle) "px")
                                     :height (str (:height ai-paddle) "px")
                                     :width (str (:width ai-paddle) "px")}}))

       ;; Training indicator
                (when (get-in state [:game :training-mode?])
                  (dom/div
                   (dom/props {:class "training-indicator"})
                   (dom/div
                    (dom/props {:class "training-text"})
                    (dom/text (str "Training: " (when-let [ct (get-in state [:game :current-training])]
                                                  (name ct))))))))

      ;; BCI confidence meters
               (when (get-in state [:bci :device-connected?])
                 (dom/div
                  (dom/props {:class "confidence-meters"})
                  (ConfidenceMeter {:label "Up Signal" :value (get bci-confidence :up 0.0)})
                  (ConfidenceMeter {:label "Down Signal" :value (get bci-confidence :down 0.0)}))))))))

(e/defn InstructionsScreen [on-back]
  (e/client
   (dom/div
    (dom/props {:class "instructions-panel"})
    (dom/h2 (dom/text "How to Play BCI Pong"))

    (dom/div
     (dom/props {:class "instructions-content"})
     (dom/p (dom/text "BCI Pong is a game that lets you play Pong using your brain signals!"))

     (dom/h3 (dom/text "Setup Steps:"))
     (dom/ol
      (dom/li (dom/text "Connect your BCI device through the BCI Setup screen"))
      (dom/li (dom/text "Train the system by recording 'up' and 'down' thought patterns"))
      (dom/li (dom/text "Start the game and focus on moving the paddle up or down"))
      (dom/li (dom/text "The system will interpret your brain signals to move the paddle")))

     (dom/h3 (dom/text "Game Controls:"))
     (dom/ul
      (dom/li (dom/text "Think 'up' to move the paddle upward"))
      (dom/li (dom/text "Think 'down' to move the paddle downward"))
      (dom/li (dom/text "First player to score 5 points wins!"))))

    (dom/button
     (dom/props {:class "back-btn"})
     (dom/on "click" (e/fn [_] (on-back)))
     (dom/text "Back to Menu")))))

(e/defn MainScreen [actions]
  (e/client
   (dom/div
    (dom/props {:class "main-menu"})
    (dom/h1 (dom/text "BCI Pong"))

    (dom/div
     (dom/props {:class "menu-options"})
     (dom/button
      (dom/props {:class "menu-btn"})
      (dom/on "click" (e/fn [_] ((:start-game actions))))
      (dom/text "Start Game"))

     (dom/button
      (dom/props {:class "menu-btn"})
      (dom/on "click" (e/fn [_] ((:show-bci-setup actions))))
      (dom/text "BCI Setup"))

     (dom/button
      (dom/props {:class "menu-btn"})
      (dom/on "click" (e/fn [_] ((:show-instructions actions))))
      (dom/text "How to Play"))))))

(e/defn GameScreen [on-exit]
  (e/client
   (dom/div
    (dom/props {:class "game-screen"})
    (Court)
    (dom/props {:class "control-panels"})
    (GameControls)
    (Instructions)

    (dom/button
     (dom/props {:class "exit-button"})
     (dom/on "click" (e/fn [_]
                       (stop-game!)
                       (on-exit)))
     (dom/text "Exit Game")))))

(e/defn !Pong []
  #?(:cljs (js/console.log "Rendering !Pong component"))
  (e/client
   #?(:cljs (js/console.log "Inside !Pong client"))
   (let [current-screen (get-in !game-state [:app :current-screen])]
     #?(:cljs (js/console.log "Current screen:" (str current-screen)))
     (dom/div
      (dom/props {:class "bci-pong" 
                  :style {:background "#222"
                          :color "#fff"
                          :padding "20px"
                          :height "100vh"
                          :width "100%"
                          :fontFamily "Arial, sans-serif"}})
      (dom/h1 
       (dom/props {:style {:color "#4ef" :textAlign "center"}})
       (dom/text "BCI Pong Game"))
      
      ;; Simplified rendering - just show a basic UI to debug
      (dom/div
       (dom/props {:style {:textAlign "center" :marginTop "40px"}})
       (dom/button
        (dom/props {:style {:padding "10px 20px"
                            :background "#4ef"
                            :color "#000"
                            :border "none"
                            :borderRadius "5px"
                            :cursor "pointer"
                            :fontSize "16px"
                            :margin "10px"}})
        (dom/on "click" (e/fn [_] 
                          #?(:cljs (js/console.log "Start button clicked"))
                          (reset-game!)
                          (start-game!)))
        (dom/text "Start Game"))
       
       (dom/button
        (dom/props {:style {:padding "10px 20px"
                            :background "#f44"
                            :color "#fff"
                            :border "none"
                            :borderRadius "5px"
                            :cursor "pointer"
                            :fontSize "16px"
                            :margin "10px"}})
        (dom/on "click" (e/fn [_] 
                          #?(:cljs (js/console.log "Stop button clicked"))
                          (stop-game!)))
        (dom/text "Stop Game")))
      
      ;; Debug info
      (dom/div
       (dom/props {:style {:marginTop "20px" 
                           :padding "10px" 
                           :background "#333" 
                           :borderRadius "5px"}})
       (dom/p (dom/text (str "Current screen: " current-screen)))
       (dom/p (dom/text (str "Game initialized: " (boolean !game-state))))
       (dom/p (dom/text (str "Game active: " (get-in !game-state [:game :playing?])))))))))