(ns pong.game-state
  (:require
   [hyperfiddle.electric :as e]))

(def default-state
  {:app {:current-screen :main-menu
         :selection-mode :selective}
   :game {:court {:width 800 :height 500}
          :ball {:x 400 :y 250 :dx 5 :dy 3 :radius 10}
          :player-paddle {:x 50 :y 200 :width 15 :height 100}
          :ai-paddle {:x 735 :y 200 :width 15 :height 100}
          :score {:player 0 :ai 0}
          :playing? false
          :training-mode? false
          :current-training nil
          :bci-confidence {:up 0.0 :down 0.0}}
   :bci {:device-connected? false
         :up-patterns []
         :down-patterns []}})

(def state (atom default-state))

(defn start-game! []
  (swap! state assoc-in [:game :playing?] true))

(defn stop-game! []
  (swap! state assoc-in [:game :playing?] false))

(defn reset-game! []
  (reset! state default-state))

(defn reset-ball! []
  (let [{:keys [court]} (:game @state)
        direction (if (> (js/Math.random) 0.5) 1 -1)]
    (swap! state assoc-in [:game :ball]
           {:x (/ (:width court) 2)
            :y (/ (:height court) 2)
            :dx (* 2 direction)
            :dy (-> (js/Math.random) (* 4) (- 2))
            :radius 5})))

(defn move-paddle! [direction]
  (let [{:keys [player-paddle]} (:game @state)
        {:keys [court]} (:game @state)
        speed (or (:speed player-paddle) 5)
        new-y (case direction
                :up (max 0 (- (:y player-paddle) speed))
                :down (min (- (:height court) (:height player-paddle))
                           (+ (:y player-paddle) speed))
                (:y player-paddle))]
    (swap! state assoc-in [:game :player-paddle :y] new-y)))

(defn move-ai-paddle! []
  (let [{:keys [ai-paddle ball court]} (:game @state)
        paddle-center (+ (:y ai-paddle) (/ (:height ai-paddle) 2))
        ball-y (:y ball)
        speed (or (:speed ai-paddle) 3)
        direction (cond
                    (< paddle-center ball-y) :down
                    (> paddle-center ball-y) :up
                    :else :none)
        new-y (case direction
                :up (max 0 (- (:y ai-paddle) speed))
                :down (min (- (:height court) (:height ai-paddle))
                           (+ (:y ai-paddle) speed))
                (:y ai-paddle))]
    (swap! state assoc-in [:game :ai-paddle :y] new-y)))

(defn update-ball! []
  (let [game-state (:game @state)
        {:keys [ball player-paddle ai-paddle court score]} game-state
        {:keys [x y dx dy radius]} ball
        new-x (+ x dx)
        new-y (+ y dy)

        ; Check for collisions with top/bottom walls
        new-dy (cond
                 (<= new-y radius) (js/Math.abs dy)
                 (>= new-y (- (:height court) radius)) (- (js/Math.abs dy))
                 :else dy)

        ; Check for collisions with paddles
        [new-dx new-x] (cond
                         ; Player paddle collision
                         (and (<= new-x (+ (:x player-paddle) (:width player-paddle) radius))
                              (>= new-x (- (:x player-paddle) radius))
                              (>= (+ new-y radius) (:y player-paddle))
                              (<= (- new-y radius) (+ (:y player-paddle) (:height player-paddle))))
                         [(js/Math.abs dx) (+ (:x player-paddle) (:width player-paddle) radius)]

                         ; AI paddle collision
                         (and (>= new-x (- (:x ai-paddle) radius))
                              (<= new-x (+ (:x ai-paddle) (:width ai-paddle) radius))
                              (>= (+ new-y radius) (:y ai-paddle))
                              (<= (- new-y radius) (+ (:y ai-paddle) (:height ai-paddle))))
                         [(- (js/Math.abs dx)) (- (:x ai-paddle) radius)]

                         :else [dx new-x])

        ; Check for scoring
        [new-score reset?] (cond
                             ; AI scores
                             (<= new-x radius)
                             [(update score :ai inc) true]

                             ; Player scores
                             (>= new-x (- (:width court) radius))
                             [(update score :player inc) true]

                             :else [score false])]

    (if reset?
      (do
        (swap! state assoc-in [:game :score] new-score)
        (reset-ball!))
      (swap! state assoc-in [:game :ball]
             {:x new-x :y new-y :dx new-dx :dy new-dy :radius radius}))))

(defn check-wall-collision! [ball court]
  (let [{:keys [x y radius]} ball
        {:keys [width height]} court]
    ; Top/bottom collision
    (when (or (<= y radius) (>= y (- height radius)))
      (swap! state update-in [:game :ball :speed-y] -))
    ; Left/right collision - for scoring
    (cond
      ; Right wall - player scores
      (>= x (- width radius))
      (do
        (swap! state update-in [:game :score :player] inc)
        (swap! state update-in [:game :ball :x] #(- width radius))
        (swap! state update-in [:game :ball :speed-x] -)
        (when (>= (get-in @state [:game :score :player]) 5)
          (js/alert "You win!")
          (stop-game!)
          (reset-game!)))

      ; Left wall - AI scores
      (<= x radius)
      (do
        (swap! state update-in [:game :score :ai] inc)
        (swap! state update-in [:game :ball :x] #(+ radius))
        (swap! state update-in [:game :ball :speed-x] -)
        (when (>= (get-in @state [:game :score :ai]) 5)
          (js/alert "AI wins!")
          (stop-game!)
          (reset-game!))))))


(defn check-paddle-collision! [ball player-paddle ai-paddle]
  (let [{ball-x :x ball-y :y radius :radius} ball

        ; Player paddle checks
        player-x2 (+ (:x player-paddle) (:width player-paddle))
        player-y1 (:y player-paddle)
        player-y2 (+ player-y1 (:height player-paddle))

        ; AI paddle checks
        ai-x1 (:x ai-paddle)
        ai-y1 (:y ai-paddle)
        ai-y2 (+ ai-y1 (:height ai-paddle))]

    ; Check player paddle collision
    (when (and (<= (- ball-x radius) player-x2)
               (>= ball-x (:x player-paddle))
               (>= ball-y player-y1)
               (<= ball-y player-y2))
      (swap! state update-in [:game :ball :speed-x] #(Math/abs %))
      (swap! state update-in [:game :ball :speed-x] #(+ % 0.5)))

    ; Check AI paddle collision
    (when (and (>= (+ ball-x radius) ai-x1)
               (<= ball-x (+ ai-x1 (:width ai-paddle)))
               (>= ball-y ai-y1)
               (<= ball-y ai-y2))
      (swap! state update-in [:game :ball :speed-x] #(- (Math/abs %)))
      (swap! state update-in [:game :ball :speed-x] #(- % 0.5)))))

(defn game-loop-tick! []
  (when (get-in @state [:game :playing?])
    (update-ball!)
    (move-ai-paddle!)

    ; Check for BCI input and move paddle based on confidence values
    (let [bci-confidence (get-in @state [:game :bci-confidence])
          up-confidence (:up bci-confidence)
          down-confidence (:down bci-confidence)
          threshold 0.6]
      (cond
        (> up-confidence threshold) (move-paddle! :up)
        (> down-confidence threshold) (move-paddle! :down)))))

(defn is-game-playing? []
  (get-in @state [:game :playing?]))

(defn handle-key-down [e]
  (let [key (.-key e)]
    (when (contains? #{"ArrowUp" "ArrowDown"} key)
      (.preventDefault e)
      (swap! state update :keys-pressed #(conj (or % #{}) key)))))

(defn handle-key-up [e]
  (let [key (.-key e)]
    (when (contains? #{"ArrowUp" "ArrowDown"} key)
      (.preventDefault e)
      (swap! state update :keys-pressed #(disj (or % #{}) key)))))

; Initialize keyboard listeners
(defn init-keyboard-controls! []
  (js/window.addEventListener "keydown" handle-key-down)
  (js/window.addEventListener "keyup" handle-key-up))

; Clean up
(defn cleanup-keyboard-controls! []
  (js/window.removeEventListener "keydown" handle-key-down)
  (js/window.removeEventListener "keyup" handle-key-up))

; Initialize game
(defn init-game! []
  (js/console.log "Initializing game state")
  (reset! state default-state)
  (init-keyboard-controls!)
  (js/console.log "Intitialized!"))