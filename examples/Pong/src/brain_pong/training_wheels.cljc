(ns brain-pong.training-wheels
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [brain-pong.game-state :as pong-state]
            #?(:clj [floj.state :as state])
            #?(:clj [floj.wave-lexicon :as lexi])
            #?(:clj [brain-pong.signature :as signature])))

(defn calculate-assistance-level
  "Calculate current assistance level based on user's BCI performance
   Returns value between 0.0 (no assistance) and 1.0 (full assistance)"
  [user-profile category-data game-stats]
  (let [; Get user's current confidence/accuracy from category data
        avg-confidence (get-in category-data [:performance :avg-confidence] 0.3)
        accuracy-rate (get-in category-data [:performance :accuracy-rate] 0.5)
        session-count (get-in category-data [:performance :session-count] 0)

        ; Current game performance
        recent-success-rate (get-in game-stats [:recent-success-rate] 0.0)

        ; Calculate base assistance level (starts high, decreases with improvement)
        confidence-factor (- 1.0 (min 1.0 avg-confidence))
        accuracy-factor (- 1.0 (min 1.0 accuracy-rate))
        experience-factor (/ 1.0 (+ 1.0 (* 0.1 session-count))) ; Decreases with experience

        ; Weighted combination
        base-assistance (* 0.4 confidence-factor)
        accuracy-assistance (* 0.3 accuracy-factor)
        experience-assistance (* 0.3 experience-factor)

        total-assistance (+ base-assistance accuracy-assistance experience-assistance)

        ; Apply recent performance modifier
        performance-modifier (if (> recent-success-rate 0.7) 0.8 1.0)

        final-assistance (* total-assistance performance-modifier)]
    (max 0.0 (min 1.0 final-assistance))))

(defn should-provide-assistance?
  "Determine if we should provide assistance based on game state and ball position"
  [game-state assistance-level]
  (let [{:keys [ball player-paddle]} (:game game-state)
        {:keys [x y dy]} ball
        {:keys [x paddle-x y paddle-y height paddle-height]} player-paddle

        ; Calculate relative positions
        ball-paddle-distance (Math/abs (- y (+ paddle-y (/ paddle-height 2))))
        ball-approaching? (> (:dx ball) 0) ; Ball moving toward player

        ; Determine if player "needs help"
        ball-above-paddle? (< y paddle-y)
        ball-below-paddle? (> y (+ paddle-y paddle-height))
        ball-moving-up? (< dy 0)
        ball-moving-down? (> dy 0)

        ; Assistance scenarios
        need-down-help? (and ball-above-paddle? ball-moving-up? ball-approaching?)
        need-up-help? (and ball-below-paddle? ball-moving-down? ball-approaching?)

        ; Apply assistance probability
        assistance-chance (rand)
        should-assist? (< assistance-chance assistance-level)]

    (when should-assist?
      (cond
        need-down-help? :down
        need-up-help? :up
        :else nil))))

(defn apply-progressive-boost
  "Apply a movement boost based on assistance level"
  [direction assistance-level base-speed]
  (let [boost-multiplier (+ 1.0 (* 2.0 assistance-level))
        boosted-speed (* base-speed boost-multiplier)]
    (dotimes [_ (Math/ceil boost-multiplier)]
      (pong-state/move-paddle! direction))

    boosted-speed))

(defn update-game-stats!
  "Update game statistics for assistance calculation"
  [result]
  (swap! pong-state/state update-in [:game :stats]
         (fn [stats]
           (let [updated-stats (-> stats
                                   (update :total-attempts inc)
                                   (update (if (= result :success) :successes :failures) inc))
                 success-rate (/ (:successes updated-stats) (:total-attempts updated-stats))]
             (assoc updated-stats :recent-success-rate success-rate)))))

(e/defn Get-adaptive-threshold
  "Get adaptive threshold based on current category intelligence"
  [category]
  (e/client
   (let [category-intelligence (e/server (lexi/load-category-summary category))]
     (if category-intelligence
       (let [intelligence-metrics (get-in category-intelligence [:summary :all :intelligence-metrics])
             separation-score (or (:separation-score intelligence-metrics) 0.5)
             triangulation-quality (or (:triangulation-quality intelligence-metrics) 1.0)
             stability-score (or (:stability-score intelligence-metrics) 0.0)
             
             base-threshold (if (> separation-score 0.6) 0.02 0.05)
             quality-adjustment (* (- 1.0 separation-score) 0.03)
             stability-adjustment (* (max 0.0 (- 1.0 stability-score)) 0.02)
             dynamic-threshold (max 0.005 (+ base-threshold quality-adjustment stability-adjustment))]
         
         (js/console.log "Adaptive threshold calculation:")
         (js/console.log "  Separation score:" separation-score)
         (js/console.log "  Triangulation quality:" triangulation-quality)
         (js/console.log "  Stability score:" stability-score)
         (js/console.log "  Base threshold:" base-threshold)
         (js/console.log "  Quality adjustment:" quality-adjustment)
         (js/console.log "  Stability adjustment:" stability-adjustment)
         (js/console.log "  Final dynamic threshold:" dynamic-threshold)
         
         dynamic-threshold)
       (do
         (js/console.log "No category intelligence found, using fallback threshold")
         0.05)))))

#?(:clj
   (defn get-assistance-data-server
     "Server function to get category data for assistance calculation"
     []
     (try
       (let [user-profile ((:get-active-profile @state/state))
             category-data (lexi/load-category-summary "pong")]
         {:category-data category-data
          :timestamp (System/currentTimeMillis)})
       (catch Exception e
         (println "Error getting assistance data:" (.getMessage e))
         {:error (.getMessage e)}))))

(e/defn AssistanceManager []
  (e/client
   (let [state (e/watch pong-state/state)
         playing? (get-in state [:game :playing?])
         connected? (get-in state [:bci :device-connected?])]

     ; Load assistance data when game starts with BCI
     (when (and playing? connected?
                (not (get-in state [:bci :assistance-data-loaded?])))
       (js/console.log "Loading assistance data...")
       (let [assistance-data (e/server (get-assistance-data-server))]
         (when (:category-data assistance-data)
           (swap! pong-state/state assoc-in [:bci :category-data]
                  (:category-data assistance-data))
           (swap! pong-state/state assoc-in [:bci :assistance-data-loaded?] true)
           (js/console.log "Assistance data loaded:" (clj->js assistance-data)))))

     ; Reset assistance data when game stops
     (when (and (not playing?) (get-in state [:bci :assistance-data-loaded?]))
       (swap! pong-state/state assoc-in [:bci :assistance-data-loaded?] false))

     nil)))

(e/defn AssistanceDisplay []
  (e/client
   (let [state (e/watch pong-state/state)
         category-data (get-in state [:bci :category-data])
         game-stats (get-in state [:game :stats])
         connected? (get-in state [:bci :device-connected?])
         playing? (get-in state [:game :playing?])]

     (when (and connected? playing? category-data)
       (let [assistance-level (calculate-assistance-level
                               (get-in state [:bci :user-profile])
                               category-data
                               game-stats)
             assistance-percent (* 100 assistance-level)
             training-wheels-text (cond
                                    (> assistance-level 0.8) "Full Training Wheels"
                                    (> assistance-level 0.5) "Partial Training Wheels"
                                    (> assistance-level 0.2) "Light Assistance"
                                    :else "Training Wheels Off!")]

         (dom/div
          (dom/props {:class "assistance-display"})
          (dom/h4 (dom/text "BCI Assistance"))
          (dom/div
           (dom/props {:class "assistance-level"})
           (dom/text (str training-wheels-text " (" (Math/round assistance-percent) "%)")))

          (when game-stats
            (dom/div
             (dom/props {:class "game-stats"})
             (dom/div (dom/text (str "Natural inputs: " (get game-stats :natural-inputs 0))))
             (dom/div (dom/text (str "Assisted inputs: " (get game-stats :assisted-inputs 0))))
             (dom/div (dom/text (str "Success rate: "
                                     (Math/round (* 100 (get game-stats :recent-success-rate 0))) "%")))))))))))