(ns brain-pong.game-state
  (:require
   [hyperfiddle.electric3 :as e]
   #?(:clj [floj.state :as state])
   #?(:clj [floj.profiles :as profile])))

(defn flex-abs [dx]
  #?(:clj (Math/abs dx) :cljs (js/Math.abs dx)))

(def default-state
  {:app  {:active-profile nil
          :current-screen :main-menu
          :selection-mode :selective}
   :game {:court {:width 800 :height 500}
          :ball {:x 400 :y 250 :dx 0 :dy 0 :radius 10}
          :player-paddle {:x 735 :y 200 :width 15 :height 100}
          :ai-paddle {:x 50 :y 200 :width 15 :height 100}
          :score {:player 0 :ai 0}
          :playing? false
          :training-mode? false
          :current-training nil}
   :bci  {:confidence {:up 0.0 :down 0.0 :confidence 0.0
                       :processing-mode "initialization" :fresh? false
                       :age 0 :timestamp 0}
          :device-connected? false
          :running? false
          :streaming? false
          :matching? false
          :incoming-data-queue []
          :last-poll-time 0
          :last-action-time 0
          :last-consumer-update nil
          :action-count 0
          :user-profile {:name "default"}
          :action-history []
          :confidence-history []
          :training-performance []
          :threshold-analysis {}
          :latest-output {}
          :pipeline-channels {:channels {}
                              :control-channels {}
                              :multiplexers {}}
          :pipeline-running? false  ; Add this
          :pipeline-stats {:samples-received 0 :samples-processed 0
                           :features-extracted 0 :confidence-calculated 0
                           :outputs-generated 0 :errors 0 :pipeline-starts 0}}
   :keys-pressed #{}})

(def state (atom default-state))

(defn init-bci-state! []
  (try
    (swap! state update-in :bci merge
           {:device-connected? false
            :streaming? false
            :matching? false
            :active-profile nil
            :match-interval nil
            :confidence {:up 0.0 :down 0.0 :confidence 0.0
                         :processing-mode "initialization" :fresh? false
                         :age 0 :timestamp 0}
            :incoming-data-queue []
            :threshold 0.15   ; Default confidence threshold
            :sensitivity 0.5  ; Default movement sensitivity
            :running? false
            :recording? false
            :current-category nil
            :pending-connect false
            :pending-disconnect false
            :pending-record nil
            :pending-stop-record false
            :status-check-needed false
            :last-poll-time 0
            :last-action-time 0
            :last-consumer-update nil
            :action-count 0
            :user-profile {:name "default"}
            :action-history []
            :confidence-history []
            :training-performance []
            :threshold-analysis {}
            :latest-output {}
            :pipeline-channels {:channels {}
                                :control-channels {}
                                :multiplexers {}}
            :pipeline-running? false
            :pipeline-stats {:samples-received 0 :samples-processed 0
                             :features-extracted 0 :confidence-calculated 0
                             :outputs-generated 0 :errors 0 :pipeline-starts 0}})
    (catch #?(:clj Exception :cljs :default) e
      (println "Error initializing BCI state:" e))))


(defn reset-ball! []
  "Reset ball to center with proper random trajectory"
  (let [current-state @state
        current-game (:game current-state)
        {:keys [court]} (:game @state)
        base-speed 6

        ; Random angle between -60 and 60 degrees (in radians)
        angle-degrees (+ -60 (rand 120)) ; -60 to +60 degrees
        angle-radians (* angle-degrees (/ #?(:clj Math/PI :cljs js/Math.PI) 180))

        ; Direction: 1 for right (toward player), -1 for left (toward AI)
        ; Can be changed to always go right: (direction 1)
        direction (if (and (= (:player (:score current-game)) 0)
                           (= (:ai (:score current-game)) 0))
                    1  ; Always go right, toward player on first serve
                    (if (< (rand) 0.5) 1 -1))

        ; cos is the horizontal component, sin gives us the vertical
        base-dx (* base-speed #?(:clj (Math/cos angle-radians) :cljs (js/Math.cos angle-radians)))
        base-dy (* base-speed #?(:clj (Math/sin angle-radians) :cljs (js/Math.sin angle-radians)))

        ; Apply direction to horizontal component
        dx (* direction base-dx)
        dy base-dy

        ; Could be changed to simulate 'height' above the table
        current-radius (or (get-in @state [:game :ball :radius]) 10)]
    (swap! state assoc-in [:game :ball]
           {:x (/ (:width court) 2)
            :y (/ (:height court) 2)
            :dx dx
            :dy dy
            :radius current-radius})))

(defn init-game-state! []
  (reset! state default-state)
  (init-bci-state!))

(defn start-game! []
  #?(:cljs
     (try
       (reset-ball!)
       (swap! state assoc-in [:game :playing?] true)
       (js/console.log "Game started")
       (catch :default e
         (js/console.error "Error starting game:" e)))))

(defn stop-game! []
  #?(:cljs
     (try
       (swap! state assoc-in [:game :playing?] false)
       (js/console.log "Game stopped")
       (catch :default e
         (js/console.error "Error stopping game:" e)))))


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
(def min-ball-speed 8)

(defn calculate-speed [dx dy]
  "Calculate the actual speed (magnitude) from velocity components"
  #?(:clj (Math/sqrt (+ (* dx dx) (* dy dy)))
     :cljs (js/Math.sqrt (+ (* dx dx) (* dy dy)))))

(defn normalize-velocity [dx dy target-speed]
  "Normalize velocity components to maintain target speed"
  (let [current-speed (calculate-speed dx dy)]
    (if (> current-speed 0)
      (let [scale (/ target-speed current-speed)]
        [(* dx scale) (* dy scale)])
      [target-speed 0])))

(defn capped-speed [v]
  (cond
    (> v max-ball-speed) max-ball-speed
    (< v (- max-ball-speed)) (- max-ball-speed)
    (and (> v 0) (< v min-ball-speed)) min-ball-speed
    (and (< v 0) (> v (- min-ball-speed))) (- min-ball-speed)
    :else v))

(defn clamp-speed [dx dy]
  "Ensure the ball speed stays within min/max bounds"
  (let [current-speed (calculate-speed dx dy)]
    (cond
      (> current-speed max-ball-speed)
      (normalize-velocity dx dy max-ball-speed)

      (< current-speed min-ball-speed)
      (normalize-velocity dx dy min-ball-speed)

      :else [dx dy])))

(defn handle-paddle-collision [ball paddle is-player?]
  "Handle paddle collision with proper angle calculation"
  (let [ball-y (:y ball)
        current-speed (calculate-speed (:dx ball) (:dy ball))
        paddle-center (+ (:y paddle) (/ (:height paddle) 2))
        hit-offset (- ball-y paddle-center)
        ; Normalize to -1 to 1 range
        normalized-offset (/ hit-offset (/ (:height paddle) 2))
        ; Clamp to prevent extreme angles
        clamped-offset (-> normalized-offset
                           (max -0.8)
                           (min 0.8))

        ; Calculate new direction
        dx-direction (if is-player? -1 1)

        ; Increase speed slightly on each hit
        new-speed (min max-ball-speed (+ current-speed 0.3))

        ; Calculate angle based on where ball hits paddle
        ; Map offset to angle range (-45 to 45 degrees)
        max-angle (* 45 (/ #?(:clj Math/PI :cljs js/Math.PI) 180)) ; 45 degrees in radians
        angle (* clamped-offset max-angle)

        ; Calculate new velocity components
        new-dx (* new-speed dx-direction #?(:clj (Math/cos angle) :cljs (js/Math.cos angle)))
        new-dy (* new-speed #?(:clj (Math/sin angle) :cljs (js/Math.sin angle)))

        ; Position ball at paddle edge
        new-x (if is-player?
                (- (:x paddle) (:radius ball))
                (+ (:x paddle) (:width paddle) (:radius ball)))]

    {:x new-x
     :y ball-y
     :dx new-dx
     :dy new-dy
     :radius (:radius ball)}))

(defn update-ball! []
  "Updated ball physics with proper speed handling"
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

        ; Apply speed clamping to maintain consistent speed
        [clamped-dx clamped-dy] (clamp-speed dx new-dy)

        ; Update positions with clamped velocities
        final-new-x (+ x clamped-dx)
        final-new-y (+ y clamped-dy)

        ; Initial state before paddle collision
        ball-after-collision
        (cond
          ; Player paddle collision (right side)
          (and (>= final-new-x (- (:x player-paddle) radius))
               (<= final-new-x (+ (:x player-paddle) (:width player-paddle) radius))
               (>= (+ final-new-y radius) (:y player-paddle))
               (<= (- final-new-y radius) (+ (:y player-paddle) (:height player-paddle))))
          (handle-paddle-collision
           {:x final-new-x :y final-new-y :dx clamped-dx :dy clamped-dy :radius radius}
           player-paddle
           true)

          ; AI paddle collision (left side)
          (and (<= final-new-x (+ (:x ai-paddle) (:width ai-paddle) radius))
               (>= final-new-x (- (:x ai-paddle) radius))
               (>= (+ final-new-y radius) (:y ai-paddle))
               (<= (- final-new-y radius) (+ (:y ai-paddle) (:height ai-paddle))))
          (handle-paddle-collision
           {:x final-new-x :y final-new-y :dx clamped-dx :dy clamped-dy :radius radius}
           ai-paddle
           false)

          ; No paddle collision
          :else
          {:x final-new-x
           :y final-new-y
           :dx clamped-dx
           :dy clamped-dy
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

(defn game-loop-tick! []
  (when (get-in @state [:game :playing?])
    (update-ball!)
    (move-ai-paddle!)

    ; Check for BCI input and move paddle based on confidence values
    (let [bci-confidence (get-in @state [:bci :confidence])
          up-confidence (:up bci-confidence)
          down-confidence (:down bci-confidence)
          threshold 0.05]
      (cond
        (> up-confidence threshold) (move-paddle! :up)
        (> down-confidence threshold) (move-paddle! :down)))))

#?(:cljs
   (defn handle-key-down [e]
     (let [key (.-key e)]
       (when (contains? #{"ArrowUp" "ArrowDown"} key)
         (.preventDefault e)
         (swap! state update :keys-pressed #(conj (or % #{}) key)))))
   :clj
   (defn handle-key-down [e]
     (println "Key down event - server-side implementation")))

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
     ; Remove existing listeners first to prevent duplicates
     (js/window.removeEventListener "keydown" handle-key-down)
     (js/window.removeEventListener "keyup" handle-key-up)
     ; Add listeners
     (js/window.addEventListener "keydown" handle-key-down)
     (js/window.addEventListener "keyup" handle-key-up))
   :clj
   (defn init-keyboard-controls! []
     (println "Initializing keyboard controls - server-side implementation")))

; Clean up
#?(:cljs
   (defn cleanup-keyboard-controls! []
     (js/window.removeEventListener "keydown" handle-key-down)
     (js/window.removeEventListener "keyup" handle-key-up))
   :clj
   (defn cleanup-keyboard-controls! []
     (println "Cleaning up keyboard controls - server-side implementation")))

; Initialize game
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

(def paddle-speed 10)
(def paddle-threshold 0.05) ; Confidence threshold for BCI movement

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
        (brain-move-paddle! :up))
      (when (> down-confidence paddle-threshold)
        (brain-move-paddle! :down)))

    (update-ball!)

    (move-ai-paddle!)))