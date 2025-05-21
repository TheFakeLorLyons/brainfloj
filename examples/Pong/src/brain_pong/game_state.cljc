(ns brain-pong.game-state
  (:require
   [hyperfiddle.electric3 :as e]
   #?(:clj [floj.state :as state])
   #?(:clj [floj.profiles :as profile])))

(def rand-start
  #?(:clj (Math/random) :cljs (js/Math.random)))
(defn flex-abs [dx]
  #?(:clj (Math/abs dx) :cljs (js/Math.abs dx)))

(def default-state
  {:app  {:active-profile nil
          :current-screen :main-menu
          :selection-mode :selective}
   :game {:court {:width 800 :height 500}
          :ball {:x 400 :y 250 :dx 5 :dy 3 :radius 10}
          :player-paddle {:x 735 :y 200 :width 15 :height 100}
          :ai-paddle {:x 50 :y 200 :width 15 :height 100}
          :score {:player 0 :ai 0}
          :playing? false
          :training-mode? false
          :current-training nil
          :bci-confidence {:up 0.0 :down 0.0}}
   :bci  {:device-connected? false
          :up-patterns []
          :down-patterns []}
   :keys-pressed #{}})

(def state (atom default-state))

(e/defn init-bci-state! []
  (swap! state update :bci merge
         {:device-connected? false
          :active-profile nil
          :match-interval nil
          :confidence {:up 0.0 :down 0.0}
          :threshold 0.6   ; Default confidence threshold
          :sensitivity 0.5 ; Default movement sensitivity
          :recording? false
          :current-category nil}))

(defn init-game-state! []
  (reset! state default-state))

(defn reset-ball! []
  (let [{:keys [court]} (:game @state)
        direction (if (> rand-start 0.5) 1 -1)
        current-radius (or (get-in @state [:game :ball :radius]) 10)]
    (swap! state assoc-in [:game :ball]
      {:x (/ (:width court) 2)
       :y (/ (:height court) 2)
       :dx (* 5 direction) ; Keep consistent speed at 5 
       :dy (-> rand-start (* 4) (- 2))
       :radius current-radius}))) 

(e/defn start-game! []
  ; Move the ball to center and give it a random direction
  (reset-ball!)
  ; Set game to playing state
  (swap! state assoc-in [:game :playing?] true)
  )

(defn stop-game! []
  (reset-ball!)
  (swap! state assoc-in [:game :playing?] false))

(defn reset-game! []
  (let [keys-pressed (:keys-pressed @state)
        bci-state (:bci @state)] ; Preserve BCI state across game resets
    (reset! state (-> default-state
                      (assoc :keys-pressed keys-pressed)
                      (assoc :bci bci-state)))))


(defn move-paddle! [direction]
  (let [{:keys [player-paddle]} (:game @state)
        {:keys [court]} (:game @state)
        speed (or (:speed player-paddle) 10)
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
        speed (or (:speed ai-paddle) 7)
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

(def max-ball-speed 10)
(def min-ball-speed 3)

(defn capped-speed [v]
  (cond
    (> v max-ball-speed) max-ball-speed
    (< v (- max-ball-speed)) (- max-ball-speed)
    (and (> v 0) (< v min-ball-speed)) min-ball-speed
    (and (< v 0) (> v (- min-ball-speed))) (- min-ball-speed)
    :else v))

(defn handle-paddle-collision [ball paddle is-player?]
  (let [ball-y (get-in ball [:y])
        current-dx  (get-in ball [:dx])
        current-dy (get-in ball [:dy])
        paddle-center (+ (:y paddle) (/ (:height paddle) 2))
        hit-offset (- ball-y paddle-center)
        normalized-offset (/ hit-offset (/ (:height paddle) 2))
        ; Allow more extreme angles (between -0.95 and 0.95 of max)
        ; This makes some balls challenging but still possible
        limited-offset (-> normalized-offset
                         (max -0.95)
                         (min 0.95))
        ; Calculate new dx (horizontal speed)
        dx-direction (if is-player? -1 1)
        ; Speed increases slightly on each hit for more challenge
        dx-magnitude (-> (flex-abs current-dx)
                       (+ 0.2)
                       (capped-speed))
        new-dx (* dx-direction dx-magnitude)
        ; Calculate new dy (vertical speed)
        ; Blend current dy with paddle influence
        current-dy (get-in ball [:dy])
        ; More paddle influence (40%) makes player skill more important
        paddle-influence (* limited-offset max-ball-speed)
        ; 60% current direction, 40% paddle influence
        new-dy (capped-speed (+ (* 0.6 current-dy)
                               (* 0.4 paddle-influence)))
        ; Position the ball at the edge of the paddle
        new-x (if is-player?
                (- (:x paddle) (get-in ball [:radius]))
                (+ (:x paddle) (:width paddle) (get-in ball [:radius])))]
    {:x new-x
     :y ball-y
     :dx new-dx
     :dy new-dy
     :radius (get-in ball [:radius])}))

(defn update-ball! []
  (let [game-state (:game @state)
        {:keys [ball player-paddle ai-paddle court score]} game-state
        {:keys [x y dx dy radius]} ball
        new-x (+ x dx)
        new-y (+ y dy)

        ; Check for collisions with top/bottom walls
        new-dy (cond
                 (<= new-y radius) (flex-abs dy)
                 (>= new-y (- (:height court) radius)) (- (flex-abs dy))
                 :else dy)

        ; Initial state before paddle collision
        ball-after-collision
        (cond
          ; Player paddle collision (right side)
          (and (>= new-x (- (:x player-paddle) radius))
            (<= new-x (+ (:x player-paddle) (:width player-paddle) radius))
            (>= (+ new-y radius) (:y player-paddle))
            (<= (- new-y radius) (+ (:y player-paddle) (:height player-paddle))))
          (handle-paddle-collision
            {:x new-x :y new-y :dx dx :dy new-dy :radius radius}
            player-paddle
            true)

          ; AI paddle collision (left side)
          (and (<= new-x (+ (:x ai-paddle) (:width ai-paddle) radius))
            (>= new-x (- (:x ai-paddle) radius))
            (>= (+ new-y radius) (:y ai-paddle))
            (<= (- new-y radius) (+ (:y ai-paddle) (:height ai-paddle))))
          (handle-paddle-collision
            {:x new-x :y new-y :dx dx :dy new-dy :radius radius}
            ai-paddle
            false)

          ; No paddle collision
          :else
          {:x new-x
           :y new-y
           :dx dx
           :dy new-dy
           :radius radius})

        ; Scoring logic
        [new-score reset?] (cond
                             (<= (:x ball-after-collision) radius)
                             [(update score :player inc) true]

                             (>= (:x ball-after-collision) (- (:width court) radius))
                             [(update score :ai inc) true]

                             :else [score false])]
    (if reset?
      (do
        (swap! state assoc-in [:game :score] new-score)
        (when (or (>= (:player new-score) 5) (>= (:ai new-score) 5))
          #?(:cljs
             (do
               (js/alert (if (>= (:player new-score) 5)
                           "You win!"
                           "AI wins!"))
               (stop-game!)
               (js/setTimeout reset-game! 500))))
        (reset-ball!))
      (swap! state assoc-in [:game :ball] ball-after-collision))))

(e/defn check-wall-collision! [ball court]
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
          (e/client
            (js/alert "You win!"))
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

#_(defn game-loop-tick! []
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

(defn game-loop-tick! []
  (when (get-in @state [:game :playing?])
    #?(:cljs (js/console.log "Game tick"))
    (update-ball!)
    (move-ai-paddle!)))
#?(:cljs
   (defn handle-key-down [e]
     (let [key (.-key e)]
       (when (contains? #{"ArrowUp" "ArrowDown"} key)
         (.preventDefault e)
         (swap! state update :keys-pressed #(conj (or % #{}) key)))))
   :clj
   (defn handle-key-down [e]
     (println "Key down event - server-side implementation")))

#_#?(:cljs
    (defn game-loop-tick!
      "Process one frame of the game logic"
      []
      (let [{:keys [ball player-paddle ai-paddle court]} (:game @state)]
       ;; Update ball position
        (swap! state update-in [:game :ball :x] + (get-in ball [:velocity :x]))
        (swap! state update-in [:game :ball :y] + (get-in ball [:velocity :y]))

       ;; Ball collision with top/bottom walls
        (when (or (<= (- (:y ball) (:radius ball)) 0)
                  (>= (+ (:y ball) (:radius ball)) (:height court)))
          (swap! state update-in [:game :ball :velocity :y] -))

       ;; Ball collision with paddles
        (let [updated-ball (get-in @state [:game :ball])]
         ;; Player paddle collision
          (when (and (<= (- (:x updated-ball) (:radius updated-ball)) (+ (:x player-paddle) (:width player-paddle)))
                     (>= (- (:x updated-ball) (:radius updated-ball)) (:x player-paddle))
                     (>= (+ (:y updated-ball) (:radius updated-ball)) (:y player-paddle))
                     (<= (- (:y updated-ball) (:radius updated-ball)) (+ (:y player-paddle) (:height player-paddle))))
            (swap! state update-in [:game :ball :velocity :x] -)
           ;; Slightly randomize y velocity on paddle hit for variety
            (let [random-factor (- (rand) 0.5)]
              (swap! state update-in [:game :ball :velocity :y] + random-factor)))

         ;; AI paddle collision
          (when (and (>= (+ (:x updated-ball) (:radius updated-ball)) (:x ai-paddle))
                     (<= (+ (:x updated-ball) (:radius updated-ball)) (+ (:x ai-paddle) (:width ai-paddle)))
                     (>= (+ (:y updated-ball) (:radius updated-ball)) (:y ai-paddle))
                     (<= (- (:y updated-ball) (:radius updated-ball)) (+ (:y ai-paddle) (:height ai-paddle))))
            (swap! state update-in [:game :ball :velocity :x] -)
           ;; Slightly randomize y velocity on paddle hit
            (let [random-factor (- (rand) 0.5)]
              (swap! state update-in [:game :ball :velocity :y] + random-factor))))

       ;; Simple AI paddle movement
        (let [updated-ball (get-in @state [:game :ball])
              ai-y (+ (:y ai-paddle) (/ (:height ai-paddle) 2))
              ball-y (:y updated-ball)
              ai-speed 3]
          (cond
            (< ai-y (- ball-y 10))
            (swap! state update-in [:game :ai-paddle :y] + ai-speed)

            (> ai-y (+ ball-y 10))
            (swap! state update-in [:game :ai-paddle :y] - ai-speed)))

       ;; Keep AI paddle within court bounds
        (swap! state update-in [:game :ai-paddle :y]
               (fn [y] (max 0 (min (- (:height court) (:height ai-paddle)) y))))

       ;; Check if ball goes out of bounds (scoring)
        (let [updated-ball (get-in @state [:game :ball])]
          (cond
           ;; AI scores
            (<= (:x updated-ball) 0)
            (do
              (swap! state update-in [:game :score :ai] inc)
              (swap! state assoc-in [:game :ball]
                     {:x 300
                      :y 200
                      :radius 10
                      :velocity {:x -3 :y 2}}))

           ;; Player scores
            (>= (:x updated-ball) (:width court))
            (do
              (swap! state update-in [:game :score :player] inc)
              (swap! state assoc-in [:game :ball]
                     {:x 300
                      :y 200
                      :radius 10
                      :velocity {:x 3 :y 2}})))))))

#?(:cljs
   (defn handle-key-up [e]
     (let [key (.-key e)]
       (when (contains? #{"ArrowUp" "ArrowDown"} key)
         (.preventDefault e)
         (swap! state update :keys-pressed #(disj (or % #{}) key)))))
   :clj
   (defn handle-key-up [e]
     (println "Key up event - server-side implementation")))
#?(:cljs
   (defn init-keyboard-controls! []
     (js/console.log "Initializing keyboard controls")
     ;; Remove existing listeners first to prevent duplicates
     (js/window.removeEventListener "keydown" handle-key-down)
     (js/window.removeEventListener "keyup" handle-key-up)
     ;; Add listeners
     (js/window.addEventListener "keydown" handle-key-down)
     (js/window.addEventListener "keyup" handle-key-up))
   :clj
   (defn init-keyboard-controls! []
     (println "Initializing keyboard controls - server-side implementation")))

;; Clean up
#?(:cljs
   (defn cleanup-keyboard-controls! []
     (js/window.removeEventListener "keydown" handle-key-down)
     (js/window.removeEventListener "keyup" handle-key-up))
   :clj
   (defn cleanup-keyboard-controls! []
     (println "Cleaning up keyboard controls - server-side implementation")))

;; Initialize game
#?(:cljs
   (defn init-game! []
     (js/console.log "Initializing game state")
     (reset! state default-state)
     (init-keyboard-controls!)
     (js/console.log "Game initialized!"))
   :clj
   (defn init-game! []
     (println "Initializing game state")
     (reset! state default-state)
     (println "Game initialized!")))

; experimental

(def paddle-speed 10)
(def paddle-threshold 0.6) ; Confidence threshold for BCI movement

(defn brain-move-paddle!
  "Move the player paddle in the specified direction"
  [direction]
  (let [current-y (get-in @state [:game :player-paddle :y])
        court-height (get-in @state [:game :court :height])
        paddle-height (get-in @state [:game :player-paddle :height])
        min-y 0
        max-y (- court-height paddle-height)
        delta (case direction
                :up (- paddle-speed)
                :down paddle-speed
                0)
        new-y (-> current-y
                  (+ delta)
                  (max min-y)
                  (min max-y))]
    (swap! state assoc-in [:game :player-paddle :y] new-y)))

(defn brain-game-loop-tick!
  "Process one tick of the game loop"
  []
  (let [{:keys [ball player-paddle ai-paddle court]} (:game @state)]

    ; Process brain signal if BCI is active
    (let [bci-confidence (get-in @state [:bci :confidence] {:up 0.0 :down 0.0})
          up-confidence (get bci-confidence :up 0.0)
          down-confidence (get bci-confidence :down 0.0)]
      (when (> up-confidence paddle-threshold)
        (move-paddle! :up))
      (when (> down-confidence paddle-threshold)
        (move-paddle! :down)))

    ; Ball movement (x, y positions)
    (let [{:keys [x y dx dy radius]} ball
          new-x (+ x dx)
          new-y (+ y dy)

          ; Detect wall collisions (top/bottom)
          hit-top? (<= new-y radius)
          hit-bottom? (>= new-y (- (:height court) radius))

          ; Detect paddle collisions
          left-paddle-x (+ (:x ai-paddle) (:width ai-paddle))
          right-paddle-x (:x player-paddle)

          ; Check if ball hits player paddle
          hit-player-paddle? (and (>= (+ new-x radius) right-paddle-x)
                                  (<= (- new-x radius) (+ right-paddle-x (:width player-paddle)))
                                  (>= (+ new-y radius) (:y player-paddle))
                                  (<= (- new-y radius) (+ (:y player-paddle) (:height player-paddle))))

          ; Check if ball hits AI paddle
          hit-ai-paddle? (and (<= (- new-x radius) left-paddle-x)
                              (>= (+ new-x radius) (:x ai-paddle))
                              (>= (+ new-y radius) (:y ai-paddle))
                              (<= (- new-y radius) (+ (:y ai-paddle) (:height ai-paddle))))

          ; Calculate new velocities based on collisions
          new-dx (cond
                   hit-player-paddle? (- (Math/abs dx)) ; Reverse x direction with same speed
                   hit-ai-paddle? (Math/abs dx)         ; Reverse x direction with same speed
                   :else dx)

          new-dy (cond
                   hit-top? (Math/abs dy)        ; Bounce off top
                   hit-bottom? (- (Math/abs dy)) ; Bounce off bottom
                   :else dy)

          ; Calculate final new position with updated velocities
          final-new-x (+ x new-dx)
          final-new-y (+ y new-dy)]

      ; Update ball position and velocity
      (swap! state assoc-in [:game :ball]
             {:x final-new-x
              :y final-new-y
              :dx new-dx
              :dy new-dy
              :radius radius})

      ; Simple AI paddle movement - follows the ball
      (let [ai-paddle-y (:y ai-paddle)
            ai-paddle-height (:height ai-paddle)
            ai-paddle-center (+ ai-paddle-y (/ ai-paddle-height 2))
            ball-y new-y
            ai-speed 3 ; Slower than player for fairness
            ai-move-dir (cond
                          (< ai-paddle-center ball-y) ai-speed     ; Move down
                          (> ai-paddle-center ball-y) (- ai-speed) ; Move up
                          :else 0)                                 ; Don't move
            new-ai-y (-> ai-paddle-y
                         (+ ai-move-dir)
                         (max 0)
                         (min (- (:height court) ai-paddle-height)))]
        (swap! state assoc-in [:game :ai-paddle :y] new-ai-y))

      ; Check for scoring (ball off sides)
      (when (< final-new-x 0)
        ; Player scores
        (swap! state update-in [:game :score :player] inc)
        (reset-ball!))

      (when (> final-new-x (:width court))
        ; AI scores
        (swap! state update-in [:game :score :ai] inc)
        (reset-ball!)))))