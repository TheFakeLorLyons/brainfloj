(ns brain-pong.main
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [brain-pong.bci-integration :as bci]
            [brain-pong.game-state :as pong-state]
            [brain-pong.components :as component]
            [brain-pong.signature :as sig]
            #?(:clj [floj.brainflow.board-ids :as bids])))

(def client-state (atom nil))

(e/defn GameLoop []
  (e/client
   (let [state (e/watch pong-state/state)
         playing? (get-in state [:game :playing?])
         matching? (get-in state [:bci :matching?])
         frame-id-atom (atom nil)]

     ; Cleanup any existing animation frame on each re-render
     (when @frame-id-atom
       (js/cancelAnimationFrame @frame-id-atom)
       (reset! frame-id-atom nil))

     ; Start a new game loop if we're playing
     (when playing?
       (js/console.log "Starting new game loop")
       (letfn [(game-loop []
                 ; Process keyboard input from keys-pressed set
                 (let [keys-pressed (get @pong-state/state :keys-pressed #{})]
                   (when (contains? keys-pressed "ArrowUp")
                     (pong-state/move-paddle! :up))
                   (when (contains? keys-pressed "ArrowDown")
                     (pong-state/move-paddle! :down)))

                 ; Process BCI input if it's connected and matching
                 (when matching?
                   (try
                     (bci/process-bci-input!)
                     (catch :default e
                       (js/console.error "Error processing BCI input:" e))))

                 ; Run the game logic tick
                 (pong-state/game-loop-tick!)

                 ; Continue the loop if still playing
                 (when (get-in @pong-state/state [:game :playing?])
                   (reset! frame-id-atom (js/requestAnimationFrame game-loop))))]

         ; Start the game loop immediately
         (reset! frame-id-atom (js/requestAnimationFrame game-loop))))

     ; Cleanup on unmount
     (e/on-unmount #(when @frame-id-atom
                      (js/cancelAnimationFrame @frame-id-atom)
                      (reset! frame-id-atom nil)))

     ; Return nil for the component (it doesn't render anything)
     nil)))

; Safe initial check
(e/defn InitialBCICheck []
  (e/client

     ; Simple one-time check with delay
   (js/setTimeout
    (fn [_]

      (js/console.log "Running initial BCI check...")
      (let [result (bci/check-device-status!)]
        (js/console.log "Initial check result:" (clj->js result))
        (when result
          (swap! pong-state/state update-in [:bci] merge
                 {:device-connected? (boolean (:connected result))
                  :active-profile (:profile-name result)})))))
   1000) ; 1 second delay
  nil)

(e/defn Main [ring-request]
  (e/client
   (when-not @client-state  ; Only initialize once
     (reset! pong-state/state pong-state/default-state)
     (pong-state/init-keyboard-controls!)
     (pong-state/init-bci-state!) ; Make sure BCI state is initialized
     (reset! client-state true)
     (js/console.log "Game state initialized successfully"))
   
   (binding [dom/node js/document.body] ; Don't forget to bind and use a wrapper div
     (dom/div ; Mandatory wrapper div https://github.com/hyperfiddle/electric/issues/74
      (let [server? (dom/label (dom/text "Toggle client/server:")
                               (dom/input (dom/props {:type "checkbox", :checked true})
                                          (dom/On "change" (fn [js-event] (-> js-event .-target .-checked)) true)))]
        (dom/pre (dom/text (if server?
                             (e/server (str "`1` on server is `" (class 1) "`"))
                             (e/client (str "`1` on client is `" (goog/typeOf 1) "`"))))))
      (dom/hr)
      (dom/p (dom/text "Source code is in ") (dom/code (dom/text "src/brain_pong/main.cljc")))
      (dom/p (dom/text "Check out ") (dom/a (dom/text "Electric examples")
                                            (dom/props {:href "https://electric.hyperfiddle.net" :target "_blank"})))

      (dom/div
       (dom/props {:class "main-container"})

                ; Page header
       (dom/div
        (dom/props {:class "header"})
        (dom/h1 (dom/text "Electric Pong")))

                ; Game container
       (dom/div
        (dom/props {:class "pong-container"})
        (component/Instructions)
        (component/PongCourt)
        (GameLoop)
                   ; BCI components
        (dom/div
         (dom/props {:class "bci-area"})
         (component/BCIPanel)
                    ; BCI status checker
         (InitialBCICheck))
        #_(component/StateDebugging)
                   ; Debug panel
        (component/DebugPanel)))))))

#_(e/defn MainMenu []
  (e/client
   (let [state (e/watch pong-state/state)]

      ;; Initialize BCI state handlers (detect connection/disconnection requests)
     (ConnectWhenPending)
     (DisconnectWhenPending)

     (dom/div
      (dom/props {:class "main-menu"})

        ;; Title and tagline
      (dom/h1 (dom/text "Electric Brain Pong"))
      (dom/p
       (dom/props {:class "tagline"})
       (dom/text "Control the paddle with your mind!"))

        ;; Main menu sections
      (dom/div
       (dom/props {:class "menu-container"})

          ;; Left column - Game modes
       (dom/div
        (dom/props {:class "menu-column game-modes"})
        (GameModeSelection))

          ;; Right column - Device status and training
       (dom/div
        (dom/props {:class "menu-column device-training"})
        (DeviceStatus)

            ;; Training section
        (dom/div
         (dom/props {:class "training-section"})
         (dom/h3 (dom/text "Training Mode"))
         (dom/p (dom/text "Calibrate your brain patterns for optimal control"))

         (dom/button
          (dom/props {:class "menu-button training-mode"})
          (dom/text "Enter Training Mode")
          (dom/On "click"
                  (fn [_]
                    (js/console.log "Entering BCI training mode...")
                    (training/reset-training-state!)
                    (swap! pong-state/state assoc-in [:app :current-screen] :training))
                  nil)))))

        ;; Instructions panel
      (dom/div
       (dom/props {:class "instructions-panel"})
       (dom/h3 (dom/text "How to Play"))
       (dom/div
        (dom/props {:class "instruction-columns"})

            ;; Keyboard controls
        (dom/div
         (dom/props {:class "instruction-column"})
         (dom/h4 (dom/text "Keyboard Controls"))
         (dom/ul
          (dom/li (dom/text "Use ↑ and ↓ arrow keys to move the paddle"))
          (dom/li (dom/text "First to 5 points wins"))))

            ;; BCI controls
        (dom/div
         (dom/props {:class "instruction-column"})
         (dom/h4 (dom/text "Brain Controls"))
         (dom/ol
          (dom/li (dom/text "Connect your BCI device"))
          (dom/li (dom/text "Train UP and DOWN brain patterns"))
          (dom/li (dom/text "Start the game with Brain Controls"))))))))))

#_(e/defn TrainingFeedback []
   (e/client
    (let [state (e/watch pong-state/state)
          training-state (e/watch training/training-state)
          confidence (get-in state [:bci :confidence] {:up 0.0 :down 0.0})
          current-direction (:current-direction training-state)]

      (dom/div
       (dom/props {:class "training-feedback"})

        ;; Only show when confidence data is available
       (when (or (> (:up confidence) 0) (> (:down confidence) 0))
         (dom/div
          (dom/props {:class "confidence-visualization"})
          (dom/h4 (dom/text "Real-time Signal Confidence"))

            ;; UP confidence bar
          (dom/div
           (dom/props {:class (str "confidence-bar "
                                   (when (= current-direction :up) "active"))})
           (dom/label (dom/text "UP: "))
           (dom/div
            (dom/props {:class "meter"})
            (dom/div
             (dom/props {:class "meter-fill"
                         :style (str "width: " (* 100 (:up confidence)) "%")})
             (dom/text (str (.toFixed (* 100 (:up confidence)) 1) "%")))))

            ;; DOWN confidence bar
          (dom/div
           (dom/props {:class (str "confidence-bar "
                                   (when (= current-direction :down) "active"))})
           (dom/label (dom/text "DOWN: "))
           (dom/div
            (dom/props {:class "meter"})
            (dom/div
             (dom/props {:class "meter-fill"
                         :style (str "width: " (* 100 (:down confidence)) "%")})
             (dom/text (str (.toFixed (* 100 (:down confidence)) 1) "%")))))))))))


#_(e/defn TrainingModePanel []
   (e/client
    (let [state (e/watch pong-state/state)
          training-mode? (get-in state [:game :training-mode?])
          current-training (get-in state [:game :current-training])
          connected? (get-in state [:bci :device-connected?])]

      (dom/div
       (dom/props {:class "training-panel"})
       (dom/h2 (dom/text "BCI Training Mode"))

      ;; Training instructions
       (dom/div
        (dom/props {:class "training-instructions"})
        (dom/p (dom/text "In this mode, you'll train your BCI device to recognize your brain patterns for paddle movement."))
        (dom/p (dom/text "Follow these steps:"))
        (dom/ol
         (dom/li (dom/text "Connect your BCI device"))
         (dom/li (dom/text "Record brain patterns for UP and DOWN movements"))
         (dom/li (dom/text "Test your control with the paddle"))))

      ;; Training controls
       (dom/div
        (dom/props {:class (str "training-controls" (when-not connected? " disabled"))})

       ;; Status display
        (dom/div
         (dom/props {:class "training-status"})
         (if training-mode?
           (dom/div
            (dom/props {:class "active-training"})
            (dom/text "Currently training: ")
            (dom/strong (dom/text (or current-training "None"))))
           (dom/div
            (dom/props {:class "inactive-training"})
            (dom/text "Training mode inactive"))))

       ;; Training buttons
        (dom/div
         (dom/props {:class "training-buttons"})

        ;; Record UP pattern
         (dom/button
          (dom/props {:class "training-button up-training"
                      :disabled (or (not connected?)
                                    (get-in state [:bci :recording?]))})
          (dom/text "Record UP Pattern")
          (dom/On "click" (fn [_]
                            (js/console.log "Starting UP pattern recording")
                            (swap! pong-state/state assoc-in [:game :training-mode?] true)
                            (swap! pong-state/state assoc-in [:game :current-training] "UP")
                            #?(:cljs (bci/start-recording! "pong-up")))
                  nil))

        ;; Record DOWN pattern
         (dom/button
          (dom/props {:class "training-button down-training"
                      :disabled (or (not connected?)
                                    (get-in state [:bci :recording?]))})
          (dom/text "Record DOWN Pattern")
          (dom/On "click" (fn [_]
                            (js/console.log "Starting DOWN pattern recording")
                            (swap! pong-state/state assoc-in [:game :training-mode?] true)
                            (swap! pong-state/state assoc-in [:game :current-training] "DOWN")
                            #?(:cljs (bci/start-recording! "pong-down")))
                  nil))

        ;; Stop recording
         (dom/button
          (dom/props {:class "training-button stop-recording"
                      :disabled (or (not connected?)
                                    (not (get-in state [:bci :recording?])))})
          (dom/text "Stop Recording")
          (dom/On "click" (fn [_]
                            (js/console.log "Stopping pattern recording")
                            (swap! pong-state/state assoc-in [:game :current-training] nil)
                            #?(:cljs (bci/stop-recording!)))
                  nil))

        ;; Test controls
         (dom/button
          (dom/props {:class "training-button test-controls"
                      :disabled (or (not connected?)
                                    (get-in state [:bci :recording?]))})
          (dom/text (if (get-in state [:bci :match-interval])
                      "Stop Testing Controls"
                      "Test BCI Controls"))
          (dom/On "click" (fn [_]
                            (if (get-in @pong-state/state [:bci :match-interval])
                              #?(:cljs (bci/stop-activity-matching!))
                              #?(:cljs (bci/start-activity-matching!))))
                  nil))

        ;; Return to main menu
         (dom/button
          (dom/props {:class "training-button return-menu"})
          (dom/text "Return to Main Menu")
          (dom/On "click" (fn [_]
                            (js/console.log "Returning to main menu")
                            (when (get-in @pong-state/state [:bci :recording?])
                              #?(:cljs (bci/stop-recording!)))
                            (when (get-in @pong-state/state [:bci :match-interval])
                              #?(:cljs (bci/stop-activity-matching!)))
                            (swap! pong-state/state assoc-in [:game :training-mode?] false)
                            (swap! pong-state/state assoc-in [:game :current-training] nil)
                            (swap! pong-state/state assoc-in [:app :current-screen] :main-menu))
                  nil))))

      ;; Feedback area
       (when (get-in state [:bci :confidence])
         (dom/div
          (dom/props {:class "training-feedback"})
          (dom/h3 (dom/text "Real-time Brain Signal Confidence"))
          (dom/div
           (dom/props {:class "confidence-data"})
           (dom/div
            (dom/props {:class "confidence-bar up"})
            (dom/label (dom/text "UP: "))
            (let [up-confidence (get-in state [:bci :confidence :up] 0)]
              (dom/div
               (dom/props {:class "meter"})
               (dom/div
                (dom/props {:class "meter-fill"
                            :style (str "width: " (* 100 up-confidence) "%")})
                (dom/text (str (.toFixed (* 100 up-confidence) 1) "%"))))))

           (dom/div
            (dom/props {:class "confidence-bar down"})
            (dom/label (dom/text "DOWN: "))
            (let [down-confidence (get-in state [:bci :confidence :down] 0)]
              (dom/div
               (dom/props {:class "meter"})
               (dom/div
                (dom/props {:class "meter-fill"
                            :style (str "width: " (* 100 down-confidence) "%")})
                (dom/text (str (.toFixed (* 100 down-confidence) 1) "%")))))))))))))

#_ (e/defn GameModeSelection []
   (e/client
    (let [state (e/watch pong-state/state)
          bci-connected? (get-in state [:bci :device-connected?])
          has-calibrations? (or (seq (get-in state [:bci :up-patterns]))
                                (seq (get-in state [:bci :down-patterns])))]
 
      (dom/div
       (dom/props {:class "game-mode-selection"})
       (dom/h2 (dom/text "Play Pong"))
 
         ;; Standard keyboard controls
       (dom/button
        (dom/props {:class "mode-button keyboard-mode"})
        (dom/text "Keyboard Controls")
        (dom/On "click"
                (fn [_]
                  (js/console.log "Starting game with keyboard controls...")
                  (pong-state/reset-game!)
                  (pong-state/init-keyboard-controls!)
                  (swap! pong-state/state assoc-in [:game :playing?] true)
                  (swap! pong-state/state assoc-in [:app :current-screen] :game))
                nil))
 
         ;; Brain controls - only enabled if connected and calibrated
       (dom/button
        (dom/props {:class (str "mode-button brain-mode"
                                (when-not (and bci-connected? has-calibrations?) " disabled"))})
        (dom/text "Brain Controls")
        (dom/On "click"
                (fn [_]
                  (when (and bci-connected? has-calibrations?)
                    (js/console.log "Starting game with brain controls...")
                    (pong-state/reset-game!)
                    (swap! pong-state/state assoc-in [:game :control-mode] :brain)
                    (swap! pong-state/state assoc-in [:game :playing?] true)
                 ;; Start brain activity matching
                    (bci/start-activity-matching!)
                    (swap! pong-state/state assoc-in [:app :current-screen] :game)))
                nil))
 
         ;; Show a message if brain controls are disabled
       (when-not (and bci-connected? has-calibrations?)
         (dom/div
          (dom/props {:class "mode-requirements"})
          (cond
            (not bci-connected?)
            (dom/p (dom/text "Connect your BCI device to use brain controls"))
 
            (not has-calibrations?)
            (dom/p (dom/text "Calibrate your brain patterns in Training Mode first")))))))))

#_ (e/defn GameModeSelection []
   (e/client
    (let [state (e/watch pong-state/state)
          bci-connected? (get-in state [:bci :device-connected?])
          has-calibrations? (or (seq (get-in state [:bci :up-patterns]))
                                (seq (get-in state [:bci :down-patterns])))]
 
      (dom/div
       (dom/props {:class "game-mode-selection"})
       (dom/h3 (dom/text "Play Mode"))
 
       ;; Standard keyboard controls
       (dom/button
        (dom/props {:class "mode-button keyboard-mode"})
        (dom/text "Play with Keyboard")
        (dom/On "click"
                (fn [_]
                  (js/console.log "Starting game with keyboard controls...")
                  (pong-state/reset-game!)
                  (pong-state/init-keyboard-controls!)
                  (swap! pong-state/state assoc-in [:game :playing?] true)
                  (swap! pong-state/state assoc-in [:app :current-screen] :game))
                nil))
 
       ;; Brain controls - only enabled if connected and calibrated
       (dom/button
        (dom/props {:class (str "mode-button brain-mode"
                                (when-not (and bci-connected? has-calibrations?) " disabled"))})
        (dom/text "Play with Brain Controls")
        (dom/On "click"
                (fn [_]
                  (when (and bci-connected? has-calibrations?)
                    (js/console.log "Starting game with brain controls...")
                    (pong-state/reset-game!)
                    (swap! pong-state/state assoc-in [:game :control-mode] :brain)
                    (swap! pong-state/state assoc-in [:game :playing?] true)
                    ;; Start brain activity matching
                    (bci/start-activity-matching!)
                    (swap! pong-state/state assoc-in [:app :current-screen] :game)))
                nil))
 
       ;; Show a message if brain controls are disabled
       (when-not (and bci-connected? has-calibrations?)
         (dom/div
          (dom/props {:class "mode-requirements"})
          (cond
            (not bci-connected?)
            (dom/p (dom/text "Connect your BCI device to use brain controls"))
 
            (not has-calibrations?)
            (dom/p (dom/text "Calibrate your brain patterns in Training Mode first")))))))))


#_(e/defn ContentRouter []
  (e/client
   (let [state (e/watch pong-state/state)
         current-screen (get-in state [:app :current-screen])]

     (case current-screen
       ;; Main Menu Screen
       :main-menu
       (dom/div
        (dom/props {:class "screen main-menu-screen"})
        (dom/h1 (dom/text "Electric Brain Pong"))
        (dom/p (dom/props {:class "tagline"})
               (dom/text "Control the paddle with your mind!"))

        (dom/div
         (dom/props {:class "menu-content"})
         ;; Left column - Game modes
         (GameModeSelection)

         ;; Right column - Device status
         (DeviceStatus)

         ;; Training mode button
         (dom/div
          (dom/props {:class "training-section"})
          (dom/h3 (dom/text "Training Mode"))
          (dom/p (dom/text "Calibrate your brain patterns for optimal control"))
          (dom/button
           (dom/props {:class "menu-button training-mode"})
           (dom/text "Enter Training Mode")
           (dom/On "click"
                   (fn [_]
                     (js/console.log "Entering BCI training mode...")
                     (training/reset-training-state!)
                     (swap! pong-state/state assoc-in [:app :current-screen] :training))
                   nil)))))

       ;; Game Screen
       :game
       (dom/div
        (dom/props {:class "screen game-screen"})
        (component/PongCourt)
        (GameLoop)
        (when (get-in state [:bci :device-connected?])
          (component/BrainConnectionStatus))
        (dom/button
         (dom/props {:class "menu-button return-button"})
         (dom/text "Return to Menu")
         (dom/On "click"
                 (fn [_]
                   (js/console.log "Returning to main menu...")
                   (swap! pong-state/state assoc-in [:game :playing?] false)
                   (when-let [interval (get-in @pong-state/state [:bci :match-interval])]
                     (bci/stop-activity-matching!))
                   (swap! pong-state/state assoc-in [:app :current-screen] :main-menu))
                 nil)))

       ;; Training Screen
       :training
       (dom/div
        (dom/props {:class "screen training-screen"})
        (training/TrainingMode))

       ;; Default/Error case
       (dom/div
        (dom/props {:class "screen error-screen"})
        (dom/h2 (dom/text "Unknown Screen"))
        (dom/p (dom/text "Something went wrong with navigation"))
        (dom/button
         (dom/props {:class "menu-button return-button"})
         (dom/text "Return to Main Menu")
         (dom/On "click"
                 (fn [_]
                   (swap! pong-state/state assoc-in [:app :current-screen] :main-menu))
                 nil)))))))

#_ (e/defn MainMenu []
   (e/client
    (let [state (e/watch pong-state/state)
          current-screen (get-in state [:app :current-screen])
          bci-connected? (get-in state [:bci :device-connected?])]

      (case current-screen
        :main-menu
        (dom/div
         (dom/props {:class "main-menu"})
         (dom/h1 (dom/text "Electric Pong"))
         (dom/p (dom/text "A Brain-Computer Interface powered game"))

         (dom/div
          (dom/props {:class "menu-options"})

         ;; Start Game Button
          (dom/button
           (dom/props {:class "menu-button start-game"})
           (dom/text "Start Game")
           (dom/On "click" (fn [_]
                             (js/console.log "Starting game...")
                             (pong-state/reset-game!)
                             (pong-state/init-keyboard-controls!)
                             (swap! pong-state/state assoc-in [:game :playing?] true)
                             (swap! pong-state/state assoc-in [:app :current-screen] :game))
                   nil))

         ;; BCI Training Mode Button
          (dom/button
           (dom/props {:class "menu-button training-mode"})
           (dom/text "BCI Training Mode")
           (dom/On "click" (fn [_]
                             (js/console.log "Entering BCI training mode...")
                             (swap! pong-state/state assoc-in [:app :current-screen] :training))
                   nil))

         ;; Connect BCI Device Button
          (dom/button
           (dom/props {:class (str "menu-button bci-connect "
                                   (if bci-connected? "connected" "disconnected"))})
           (dom/text (if bci-connected?
                       "Disconnect BCI Device"
                       "Connect BCI Device"))
           (dom/On "click" (fn [_]
                             (if bci-connected?
                               (do
                                 (js/console.log "Disconnecting BCI device...")
                                 (swap! pong-state/state assoc-in [:bci :pending-disconnect] true))
                               (do
                                 (js/console.log "Connecting BCI device...")
                                 (swap! pong-state/state assoc-in [:bci :pending-connect] true))))
                   nil))

         ;; Instructions and Controls
          (dom/div
           (dom/props {:class "instructions-panel"})
           (dom/h3 (dom/text "How to Play"))
           (dom/ul
            (dom/li (dom/text "Use ↑ and ↓ arrow keys to move your paddle"))
            (dom/li (dom/text "OR connect your BCI device and control with your mind!"))
            (dom/li (dom/text "First player to reach 5 points wins")))

           (dom/h3 (dom/text "BCI Controls"))
           (dom/p (dom/text "To use brain control:"))
           (dom/ol
            (dom/li (dom/text "Connect your BCI device"))
            (dom/li (dom/text "Train UP and DOWN brain patterns in Training Mode"))
            (dom/li (dom/text "Start the game and control with your mind!")))))))

      :game
      (dom/div
       (dom/props {:class "game-screen"})
       (PongCourt)
       (when bci-connected?
         (BrainConnectionStatus))
       (dom/button
        (dom/props {:class "return-button"})
        (dom/text "Return to Menu")
        (dom/On "click" (fn [_]
                          (js/console.log "Returning to main menu...")
                          (swap! pong-state/state assoc-in [:game :playing?] false)
                          (swap! pong-state/state assoc-in [:app :current-screen] :main-menu))
                nil)))

      :training
      (TrainingModePanel)

       ;; Default fallback
      (dom/div
       (dom/props {:class "error-screen"})
       (dom/h2 (dom/text "Unknown Screen"))
       (dom/p (dom/text "Something went wrong"))
       (dom/button
        (dom/props {:class "return-button"})
        (dom/text "Return to Main Menu")
        (dom/On "click" (fn [_]
                          (swap! pong-state/state assoc-in [:app :current-screen] :main-menu))
                nil))))))

#_(e/defn BciConnectionHandlers []
  (e/client
   (let [state (e/watch pong-state/state)]
     ;; Check for connection requests
     (when (get-in state [:bci :pending-connect])
       (js/console.log "Processing pending BCI connection")
       (swap! pong-state/state assoc-in [:bci :pending-connect] false)
       (e/server
        (let [result (bci/connect-device-server)]
          (when (:success result)
            (e/client
             (swap! pong-state/state assoc-in [:bci :device-connected?] true)
             (swap! pong-state/state assoc-in [:bci :active-profile] (:profile-name result)))))))

     ;; Check for disconnection requests
     (when (get-in state [:bci :pending-disconnect])
       (js/console.log "Processing pending BCI disconnection")
       (swap! pong-state/state assoc-in [:bci :pending-disconnect] false)
       (e/server
        (bci/disconnect-device-server))
       (e/client
        (swap! pong-state/state assoc-in [:bci :device-connected?] false)
        (swap! pong-state/state assoc-in [:bci :active-profile] nil)
        (when-let [interval (get-in @pong-state/state [:bci :match-interval])]
          (bci/stop-activity-matching!)))))))

;; -------------------------
;; Main App Component
;; -------------------------
#_(e/defn AppRoot []
  (e/client
   ;; Always initialize the BCI connection handlers
   (BciConnectionHandlers)

   (dom/div
    (dom/props {:class "brain-pong-app"})
    (ContentRouter))))

#_(e/defn AppRoot []
  (e/client
   (let [state (e/watch pong-state/state)]
     ;; Initialize BCI state handlers
     (ConnectWhenPending)
     (DisconnectWhenPending)

     (dom/div
      (dom/props {:class "brain-pong-app"})
      (MainMenu)))))


(defn electric-boot [ring-request]
  #?(:clj  (e/boot-server {} Main (e/server ring-request))  ; inject server-only ring-request ;copy this verbatim nil on client not on server
     :cljs (e/boot-client {} Main (e/server (e/amb)))))     ; symmetric – same arity – no-value hole in place of server-only ring-request