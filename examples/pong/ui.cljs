(ns pong.ui
  (:require
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]))

(defn draw-rectangle! [ctx x y width height color]
  (set! (.-fillStyle ctx) color)
  (.fillRect ctx x y width height))

(defn draw-circle! [ctx x y radius color]
  (set! (.-fillStyle ctx) color)
  (.beginPath ctx)
  (.arc ctx x y radius 0 (* 2 js/Math.PI))
  (.fill ctx))

(defn draw-text! [ctx text x y size color]
  (set! (.-fillStyle ctx) color)
  (set! (.-font ctx) (str size "px Arial"))
  (set! (.-textAlign ctx) "center")
  (.fillText ctx text x y))

(defn draw-dashed-line! [ctx x1 y1 x2 y2]
  (set! (.-strokeStyle ctx) "#444")
  (set! (.-lineWidth ctx) 2)
  (set! (.-setLineDash ctx) #js[5, 10])
  (.beginPath ctx)
  (.moveTo ctx x1 y1)
  (.lineTo ctx x2 y2)
  (.stroke ctx)
  (set! (.-setLineDash ctx) #js[]))


(e/defn GameCanvas [game-state]
  (let [canvas-ref (e/client (atom nil))]
    ;; Render canvas element
    (e/client
     (dom/div (dom/props {:class "game-container"})
              (dom/canvas
               (dom/props {:id "pong-canvas"
                           :width 640
                           :height 480})
               (dom/on! "mount" (e/fn [el] (reset! canvas-ref el))))))
    (e/client
     (when-let [canvas @canvas-ref]
       (let [ctx (.getContext canvas "2d")]
         (js/setInterval
          (fn []
            (let [pong-state (:game game-state)]
              (set! (.-fillStyle ctx) "black")
              (.fillRect ctx 0 0 (.-width canvas) (.-height canvas))

              (set! (.-fillStyle ctx) "white")
              (set! (.-strokeStyle ctx) "white")
              (set! (.-lineWidth ctx) 5)
              (.setLineDash ctx #js[15 15])
              (.beginPath ctx)
              (.moveTo ctx (/ (.-width canvas) 2) 0)
              (.lineTo ctx (/ (.-width canvas) 2) (.-height canvas))
              (.stroke ctx)

              (let [left-paddle (:player-paddle pong-state)
                    right-paddle (:ai-paddle pong-state)]
                (.fillRect ctx
                           (:x left-paddle)
                           (:y left-paddle)
                           (:width left-paddle)
                           (:height left-paddle))
                (.fillRect ctx
                           (:x right-paddle)
                           (:y right-paddle)
                           (:width right-paddle)
                           (:height right-paddle)))

              (let [ball (:ball pong-state)]
                (.beginPath ctx)
                (.arc ctx (:x ball) (:y ball) (:radius ball) 0 (* 2 js/Math.PI))
                (.fill ctx))

              (set! (.-font ctx) "40px Arial")
              (set! (.-textAlign ctx) "center")
              (.fillText ctx
                         (str (get-in pong-state [:score :player]))
                         (- (/ (.-width canvas) 2) 50)
                         50)
              (.fillText ctx
                         (str (get-in pong-state [:score :right]))
                         (+ (/ (.-width canvas) 2) 50)
                         50))))
         16)))))

(e/defn WaveSignaturePanel [profile-name actions]
  (e/client
   (let [!recording (atom {:recording? false
                           :category ""})]
     (dom/div (dom/props {:class "wave-signatures"})
              (dom/h3 (dom/text "Brain Wave Signatures"))

              (dom/div (dom/props {:class "record-signature"})
                       (dom/h4 (dom/text "Record New Signature"))

                       (if (:recording? @!recording)
                         (dom/div
                          (dom/p (dom/text "Recording category: " (:category @!recording)))
                          (dom/button
                           (dom/props {:class "btn danger"})
                           (dom/on "click" (e/fn [_]
                                             ((:stop-recording actions))
                                             (swap! !recording assoc :recording? false)))
                           (dom/text "Stop Recording")))

                         (dom/div
                          (dom/input
                           (dom/props {:type "text"
                                       :placeholder "Enter category (e.g., pong-up)"
                                       :value (:category @!recording)})
                           (dom/on "input" (e/fn [e]
                                             (swap! !recording assoc
                                                    :category (.. e -target -value)))))

                          (dom/button
                           (dom/props {:class "btn primary"
                                       :disabled (empty? (:category @!recording))})
                           (dom/on "click" (e/fn [_]
                                             (when-not (empty? (:category @!recording))
                                               ((:start-recording actions) (:category @!recording))
                                               (swap! !recording assoc :recording? true))))
                           (dom/text "Start Recording")))))))))

(e/defn SignatureDebugOverlay [similarities]
  (dom/div (dom/props {:class "signature-debug-overlay"})
           (dom/h3 (dom/text "📊 Live Signal Match"))
           (if (seq similarities)
             (dom/ul
              (e/for [{:keys [category similarity]} similarities]
                (dom/li
                 (dom/span (dom/text (str "🧠 " category ": " (.toFixed (* 100 similarity) 1) "%"))))))
             (dom/p (dom/text "No match data yet")))))

(e/defn GameControls [game-state actions]
  (e/client
   (let [game-running? (boolean (:playing? (:game game-state)))]
     (dom/div (dom/props {:class "game-controls"})
              (if game-running?
                (dom/button
                 (dom/props {:class "btn danger"})
                 (dom/on "click" (e/fn [_] ((:stop-game actions))))
                 (dom/text "Stop Game"))

                (dom/button
                 (dom/props {:class "btn success"})
                 (dom/on "click" (e/fn [_] ((:start-game actions))))
                 (dom/text "Start Game")))))))

(e/defn DeviceConnectionPanel [bci-state actions on-back]
  (e/client
   (let [!connection-status (atom (if (:device-connected? (:bci bci-state))
                                    "connected"
                                    "disconnected"))]
     (dom/div (dom/props {:class "bci-panel"})
              (dom/h2 (dom/text "BCI Device Setup"))

              (dom/div (dom/props {:class "connection-status"})
                       (dom/span (dom/text "Status: "))
                       (dom/span
                        (dom/props {:class (str "status-" @!connection-status)})
                        (dom/text (case @!connection-status
                                    "connected" "Connected"
                                    "connecting" "Connecting..."
                                    "failed" "Connection Failed"
                                    "disconnected" "Disconnected"))))

              (dom/div (dom/props {:class "profile-section"})
                       (dom/h3 (dom/text "Device Profile"))
                       (dom/div (dom/props {:class "profile-display"})
                                (if-let [profile (:active-profile (:bci bci-state))]
                                  (dom/div
                                   (dom/span (dom/props {:class "profile-name"})
                                             (dom/text profile))
                                   (dom/span (dom/props {:class "profile-info"})
                                             (dom/text (str "Profile: " profile))))
                                  (dom/span (dom/props {:class "no-profile"})
                                            (dom/text "No profile loaded")))))

              (dom/div (dom/props {:class "action-buttons"})
                       (dom/button (dom/props {:class "bci-btn"})
                                   (dom/on "click" (e/fn [_]
                                                     (reset! !connection-status "connecting")
                                                     ((:connect-device actions))
                                                     (js/setTimeout
                                                      (fn []
                                                        (if (:device-connected? (:bci bci-state))
                                                          (reset! !connection-status "connected")
                                                          (reset! !connection-status "failed")))
                                                      2000)))
                                   (dom/text "Connect Device"))

                       (dom/button (dom/props {:class "bci-btn profile-btn"})
                                   (dom/on "click" (e/fn [_] ((:load-profiles actions))))
                                   (dom/text "Load Profiles"))

                       (dom/button (dom/props {:class "bci-btn back-btn"})
                                   (dom/on "click" (e/fn [_] (on-back)))
                                   (dom/text "Back to Menu")))

              (when (= @!connection-status "connected")
                (dom/div (dom/props {:class "training-section"})
                         (dom/h3 (dom/text "Brain Signal Training"))

                         (dom/div (dom/props {:class "training-options"})
                                  (dom/div (dom/props {:class "training-option"})
                                           (dom/h4 (dom/text "Up Movement"))
                                           (dom/button (dom/props {:class "training-btn"})
                                                       (dom/on "click" (e/fn [_]
                                                                         ((:start-recording actions) "up")))
                                                       (dom/text "Record Up Signal"))
                                           (dom/div (dom/props {:class "sample-count"})
                                                    (dom/text "10 samples")))

                                  (dom/div (dom/props {:class "training-option"})
                                           (dom/h4 (dom/text "Down Movement"))
                                           (dom/button (dom/props {:class "training-btn"})
                                                       (dom/on "click" (e/fn [_]
                                                                         ((:start-recording actions) "down")))
                                                       (dom/text "Record Down Signal"))
                                           (dom/div (dom/props {:class "sample-count"})
                                                    (dom/text "10 samples"))))

                         (dom/button (dom/props {:class "stop-btn"})
                                     (dom/on "click" (e/fn [_] ((:stop-recording actions))))
                                     (dom/text "Stop Recording"))))))))

(e/defn MainMenu [actions]
  (dom/div (dom/props {:class "main-menu"})
           (dom/h1 (dom/text "BCI Pong"))

           (dom/div (dom/props {:class "menu-options"})
                    (dom/button (dom/props {:class "menu-btn"})
                                (dom/on "click" (e/fn [_] ((:start-game actions))))
                                (dom/text "Start Game"))

                    (dom/button (dom/props {:class "menu-btn"})
                                (dom/on "click" (e/fn [_] ((:show-bci-setup actions))))
                                (dom/text "BCI Setup"))

                    (dom/button (dom/props {:class "menu-btn"})
                                (dom/on "click" (e/fn [_] ((:show-instructions actions))))
                                (dom/text "How to Play")))))