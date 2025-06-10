(ns brain-pong.bci-integration
  (:require [hyperfiddle.electric3 :as e]
            [brain-pong.game-state :as pong-state]
            [brain-pong.training-wheels :as wheels]
            [hyperfiddle.electric-dom3 :as dom]
            [mount.core :as mount]
            #?(:clj [brain-pong.signature :as signature])
            #?(:clj [floj.api :as api])
            #?(:clj [floj.state :as state])
            #?(:clj [floj.io :as fio])
            #?(:clj [floj.lor :as lor])
            #?(:clj [floj.profiles :as profiles])
            #?(:clj [floj.wave-lexicon :as lexi])
            #?(:clj [floj.calibration :as calibrate])
            #?(:clj [floj.record :as record])
            #?(:clj [floj.wave-refraction :as refraction])
            #?(:clj [floj.keybindings :as kb])))

#?(:cljs (declare process-bci-input))

#?(:clj
   (defn initialize-modules! []
     (api/initialize-brainflow!)
     (fio/initialize-io!)
     (lor/initialize-lor!)
     (kb/initialize-keybindings!)
     (profiles/initialize-profiles!)
     (lexi/initialize-lexicon!)
     (record/initialize-record!)
     (calibrate/initialize-calibration!)
     (refraction/initialize-baseline!)))

#?(:clj
   (defn connect-device-server []
     (try
       (initialize-modules!)
       (mount/start)
       (println "Server: Connecting to BCI device with active profile")
       (let [active-profile ((:get-active-profile @state/state))
             active-profile-name (:name active-profile)
             connected? (api/connect-from-profile! active-profile)]
         (println "Connection result:" connected?)
         {:connected connected?
          :profile-name active-profile-name})
       (catch Exception e
         (println "Error in connect-device-server:" (.getMessage e))
         {:connected false :error (.getMessage e)}))))

#?(:clj
   (defn disconnect-device-server []
     (println "Server: Disconnecting from BCI device")
     (try
       (when-let [f (:release-board! @state/state)]
         (f @state/shim))
       {:connected true}
       (catch Exception e
         (println "Error disconnecting:" (.getMessage e))
         {:connected false :error (.getMessage e)}))))

#?(:clj
   (defn get-device-status-server []
     (try
       (let [connected? (boolean @state/shim)
             active-profile ((:get-active-profile @state/state))
             active-profile-name (:name active-profile)]
         {:connected connected?
          :profile-name active-profile-name})
       (catch Exception e
         (println "Error getting device status:" (.getMessage e))
         {:success false
          :connected false
          :error (.getMessage e)}))))

#?(:clj
   (defn start-recording-server [category]
     (println "Server: Starting recording for category:" category)
     (try
       (lexi/start-wave-signature-recording! category "pong")
       (catch Exception e
         (println "Error starting recording:" (.getMessage e))
         nil))))

#?(:clj
   (defn stop-recording-server [category]
     (try
       (lexi/stop-wave-signature-recording! category "pong")
       {:connected true}
       (catch Exception e
         (println "Error stopping recording:" (.getMessage e))
         {:connected false :error (.getMessage e)}))))

(e/defn Start-recording! [category]
  (e/client
   (js/console.log "Client: Starting recording for category:" category)
   (let [result (e/server (start-recording-server category))]
     (when (:connected result)
       (swap! pong-state/state assoc-in [:bci :recording?] true)
       (swap! pong-state/state assoc-in [:bci :current-category] category))
     result)))

(e/defn Stop-recording! [category]
  (e/client
   (js/console.log "Client: Stopping recording")
   (let [result (e/server (stop-recording-server category))]
     (swap! pong-state/state assoc-in [:bci :recording?] false)
     (swap! pong-state/state assoc-in [:bci :current-category] nil)
     result)))

#?(:cljs
   (defn calculate-paddle-speed
     "Calculate paddle speed based on confidence score, threshold, and sensitivity"
     [confidence threshold sensitivity]
     (when (>= confidence threshold)
       ; Map confidence from threshold-1.0 range to 0-1.0 range
       (let [normalized-confidence (/ (- confidence threshold) (- 1.0 threshold))
             ; Apply sensitivity factor (higher sensitivity = more movement)
             ; Base speed of 5-15 pixels per frame based on sensitivity and confidence
             base-speed (* 5 (+ 1 (* 2 sensitivity))) ; Apply sensitivity multiplier (higher sensitivity = more responsive)
             speed (* base-speed normalized-confidence)]
         speed))))

#?(:clj
   (defn get-eeg-data-status-server []
     (try
       (let [recording? @state/recording?
             eeg-data @state/eeg-data
             data-available? (and eeg-data
                                  recording?
                                  (seq eeg-data))]
         {:data-available data-available?
          :recording recording?
          :data-type (type eeg-data)
          :data-count (if data-available? (count eeg-data) 0)})
       (catch Exception e
         (println "Error checking EEG data status:" (.getMessage e))
         {:data-available false :recording false :error (.getMessage e)}))))

#?(:cljs
   (defn calculate-adaptive-threshold
     "Calculate client-side adaptive threshold based on recent performance
      This is a second layer on top of the server's static threshold"
     [base-threshold confidence-history paddle-actions]
     (let [; Recent performance metrics
           recent-window 10000 ; 10 seconds of data
           current-time (js/Date.now)
           recent-actions (filter #(< (- current-time (:timestamp %)) recent-window) paddle-actions)
           recent-confidences (filter #(< (- current-time (:timestamp %)) recent-window) confidence-history)

           ; Calculate success/failure rates
           total-recent-actions (count recent-actions)
           successful-actions (count (filter :successful recent-actions))
           success-rate (if (> total-recent-actions 0)
                          (/ successful-actions total-recent-actions)
                          0.5)

           ; Calculate average confidence levels
           avg-confidence (if (seq recent-confidences)
                            (/ (reduce + (map :max-confidence recent-confidences))
                               (count recent-confidences))
                            0.3)

           ; Calculate confidence variance (stability)
           confidence-variance (if (> (count recent-confidences) 1)
                                 (let [mean avg-confidence
                                       squared-diffs (map #(Math/pow (- (:max-confidence %) mean) 2) recent-confidences)]
                                   (/ (reduce + squared-diffs) (count recent-confidences)))
                                 0.1)
           ; Adaptive adjustments
           ; If success rate is low, lower threshold (make it easier)
           success-adjustment (cond
                                (< success-rate 0.3) -0.15  ; Much easier
                                (< success-rate 0.5) -0.08  ; Somewhat easier
                                (> success-rate 0.8) 0.05   ; Slightly harder
                                :else 0.0)
           ; If confidence is consistently low, lower threshold
           confidence-adjustment (cond
                                   (< avg-confidence 0.15) -0.1   ; Much easier
                                   (< avg-confidence 0.25) -0.05  ; Somewhat easier
                                   (> avg-confidence 0.4) 0.03    ; Slightly harder
                                   :else 0.0)
           ; If confidence is very unstable, make threshold more forgiving
           stability-adjustment (if (> confidence-variance 0.05) -0.03 0.0)
           ; Combine adjustments
           total-adjustment (+ success-adjustment confidence-adjustment stability-adjustment)

           ; Apply to base threshold with bounds
           adaptive-threshold (max 0.05 (min 0.6 (+ base-threshold total-adjustment)))]
       {:adaptive-threshold adaptive-threshold
        :base-threshold base-threshold
        :success-rate success-rate
        :avg-confidence avg-confidence
        :confidence-variance confidence-variance
        :adjustments {:success success-adjustment
                      :confidence confidence-adjustment
                      :stability stability-adjustment
                      :total total-adjustment}})))

#?(:cljs
   (defn get-adaptive-threshold
     "Get the current adaptive threshold based on performance"
     [base-threshold]
     (let [state @pong-state/state
           action-history (get-in state [:bci :action-history] [])
           confidence-history (get-in state [:bci :confidence-history] [])
           threshold-data (calculate-adaptive-threshold base-threshold confidence-history action-history)
           adaptive-threshold (:adaptive-threshold threshold-data)]

       (swap! pong-state/state assoc-in [:bci :threshold-analysis] threshold-data)
       adaptive-threshold)))

#?(:cljs
   (defn calculate-recent-performance
     "Calculate recent performance metrics on client side"
     [state]
     (let [action-history (get-in state [:bci :action-history] [])
           current-time (js/Date.now)
           recent-window 30000
           recent-actions (filter #(< (- current-time (:timestamp %)) recent-window) action-history)

           total-actions (count recent-actions)
           successful-actions (count (filter :successful recent-actions))
           success-rate (if (> total-actions 0) (/ successful-actions total-actions) 0.5)]

       {:success-rate success-rate
        :total-actions total-actions
        :successful-actions successful-actions})))

#?(:cljs
   (defn update-performance-history
     "Enhanced version that tracks assisted vs natural actions"
     [action-type confidence-data successful? & [assisted?]]
     (let [current-time (js/Date.now)
           max-confidence (max (:up confidence-data) (:down confidence-data))
           assisted-flag (boolean assisted?)

           ; Add to action history
           action-record {:timestamp current-time
                          :action action-type
                          :successful successful?
                          :assisted assisted-flag
                          :confidence confidence-data}

           ; Add to confidence history  
           confidence-record {:timestamp current-time
                              :max-confidence max-confidence
                              :up (:up confidence-data)
                              :down (:down confidence-data)}]
       
       ; Update state with new records
       (swap! pong-state/state update-in [:bci :action-history]
              (fn [history]
                (let [updated (conj (or history []) action-record)]
                  (take-last 50 updated))))

       (swap! pong-state/state update-in [:bci :confidence-history]
              (fn [history]
                (let [updated (conj (or history []) confidence-record)]
                  (take-last 100 updated)))))))

#?(:cljs
   (defn format-number [n decimals]
     (.toFixed n decimals))
   :clj
   (defn format-number [n decimals]
     (format (str "%." decimals "f") n)))

(defn should-provide-training-assistance?
  "Determine if training wheels should activate based on category intelligence and performance"
  [confidence-data game-context category-intelligence recent-performance]
  (let [; Use existing intelligence metrics
        separation-score (get-in category-intelligence [:summary :all :intelligence-metrics :separation-score] 0.5)
        stability-score (get-in category-intelligence [:summary :all :intelligence-metrics :stability-score] 0.0)

        ; Calculate current performance metrics
        avg-confidence (/ (+ (:up confidence-data) (:down confidence-data)) 2)
        success-rate (get recent-performance :success-rate 0.5)
        session-count (get recent-performance :total-sessions 0)

        ; Determine assistance level based on category intelligence
        ; Low separation = more assistance needed
        ; Low stability = more assistance needed
        base-assistance-level (cond
                                (and (< separation-score 0.3) (< stability-score 0.2)) 0.8  ; Heavy assistance
                                (or (< separation-score 0.4) (< stability-score 0.3)) 0.6   ; Moderate assistance
                                (or (< separation-score 0.6) (< success-rate 0.4)) 0.3      ; Light assistance
                                :else 0.0)  ; No assistance

        ; Adjust based on recent performance
        performance-adjustment (cond
                                 (< success-rate 0.2) 0.2   ; Boost assistance
                                 (< success-rate 0.4) 0.1   ; Light boost
                                 (> success-rate 0.8) -0.1  ; Reduce assistance
                                 :else 0.0)

        final-assistance-level (max 0.0 (min 1.0 (+ base-assistance-level performance-adjustment)))

        ; Determine direction based on confidence and game context
        assistance-direction (cond
                               (and (> final-assistance-level 0.2)
                                    (> (:up confidence-data) (:down confidence-data))
                                    (> (:up confidence-data) 0.1)) :up
                               (and (> final-assistance-level 0.2)
                                    (> (:down confidence-data) (:up confidence-data))
                                    (> (:down confidence-data) 0.1)) :down
                               :else nil)]

    (when assistance-direction
      {:direction assistance-direction
       :assistance-level final-assistance-level
       :reason (cond
                 (< separation-score 0.4) "Low signal separation"
                 (< stability-score 0.3) "Unstable performance"
                 (< success-rate 0.4) "Low success rate"
                 :else "General training assistance")
       :metrics {:separation-score separation-score
                 :stability-score stability-score
                 :success-rate success-rate}})))

#?(:clj
   (defn calculate-training-assistance-server
     "Server-side calculation of training assistance"
     [category confidence-data game-context recent-performance]
     (try
       (let [category-intelligence (lexi/load-category category)]
         (if category-intelligence
           (let [; Get intelligence metrics
                 intelligence-metrics (get-in category-intelligence [:summary :all :intelligence-metrics])
                 separation-score (get intelligence-metrics :separation-score 0.5)
                 stability-score (get intelligence-metrics :stability-score 0.0)

                 ; Calculate current performance metrics
                 avg-confidence (/ (+ (:up confidence-data) (:down confidence-data)) 2)
                 success-rate (get recent-performance :success-rate 0.5)

                 ; Determine assistance level
                 base-assistance-level (cond
                                         (and (< separation-score 0.3) (< stability-score 0.2)) 0.8
                                         (or (< separation-score 0.4) (< stability-score 0.3)) 0.6
                                         (or (< separation-score 0.6) (< success-rate 0.4)) 0.3
                                         :else 0.0)

                 ; Adjust based on recent performance
                 performance-adjustment (cond
                                          (< success-rate 0.2) 0.2
                                          (< success-rate 0.4) 0.1
                                          (> success-rate 0.8) -0.1
                                          :else 0.0)

                 final-assistance-level (max 0.0 (min 1.0 (+ base-assistance-level performance-adjustment)))

                 ; Determine direction
                 assistance-direction (cond
                                        (and (> final-assistance-level 0.2)
                                             (> (:up confidence-data) (:down confidence-data))
                                             (> (:up confidence-data) 0.1)) :up
                                        (and (> final-assistance-level 0.2)
                                             (> (:down confidence-data) (:up confidence-data))
                                             (> (:down confidence-data) 0.1)) :down
                                        :else nil)]

             (when assistance-direction
               {:direction assistance-direction
                :assistance-level final-assistance-level
                :reason (cond
                          (< separation-score 0.4) "Low signal separation"
                          (< stability-score 0.3) "Unstable performance"
                          (< success-rate 0.4) "Low success rate"
                          :else "General training assistance")
                :metrics {:separation-score separation-score
                          :stability-score stability-score
                          :success-rate success-rate}}))

           ; No category intelligence available
           (when (< (/ (+ (:up confidence-data) (:down confidence-data)) 2) 0.2)
             {:direction (if (> (:up confidence-data) (:down confidence-data)) :up :down)
              :assistance-level 0.7
              :reason "No category intelligence - providing basic assistance"})))

       (catch Exception e
         (println "Error calculating training assistance:" (.getMessage e))
         nil))))

(defn apply-smart-assistance!
  "Apply training wheels assistance with intelligence-based scaling"
  [assistance-info confidence-data]
  (let [{:keys [direction assistance-level]} assistance-info
        base-speed 1.0]

    ; Use existing training wheels boost logic
    (wheels/apply-progressive-boost direction assistance-level base-speed)

    ; Track assistance for adaptive learning
    (swap! pong-state/state update-in [:training-wheels :assistance-count] (fnil inc 0))

    (println "Training assistance applied:" direction "at level" assistance-level)))

#?(:cljs
   (defn update-client-performance-tracking!
     "Enhanced performance tracking that feeds back to server"
     [action-type confidence-data successful? assisted?]
     (do
       (update-performance-history action-type confidence-data successful?)
       ; Additional tracking for training wheels
       (let [current-time (js/Date.now)
             performance-record {:timestamp current-time
                                 :action action-type
                                 :successful successful?
                                 :assisted assisted?
                                 :confidence confidence-data}]

         (swap! pong-state/state update-in [:bci :training-performance]
                (fn [history]
                  (take-last 20 (conj (or history []) performance-record))))))))

#?(:clj
   (defn process-live-training-integration!
     "Main function to handle live training and signature enhancement"
     [category confidence-data eeg-sample game-context recent-performance]
     (try
       (let [category-intelligence (lexi/load-category category)]
         (when category-intelligence
           ; 1. Check for live signature enhancement opportunity
           (signature/capture-live-enhancement! category confidence-data eeg-sample game-context)

           ; 2. Check for training wheels assistance
           (let [assistance-info (should-provide-training-assistance?
                                  confidence-data game-context category-intelligence recent-performance)]

             (when assistance-info
               (apply-smart-assistance! assistance-info confidence-data)

               ; 3. Update performance tracking for future assistance calculations
               (wheels/update-game-stats!
                (if (> (:assistance-level assistance-info) 0.5) :assisted :natural))))))

       (catch Exception e
         (println "Error in live training integration:" (.getMessage e))))))

#?(:clj
   (defn enhanced-brain-activity-response-server
     "Enhanced version with fixed state management"
     [profile-name category game-context recent-performance]
     (try
       (let [brain-response (signature/match-brain-activity)
             ; Ensure confidence data has valid numbers
             raw-up (or (:up brain-response) 0.0)
             raw-down (or (:down brain-response) 0.0)
             confidence-data {:up (if (number? raw-up) raw-up 0.0)
                              :down (if (number? raw-down) raw-down 0.0)}

             ; Calculate overall confidence
             category-intelligence (try
                                     (lexi/load-category category)
                                     (catch Exception e
                                       (println "Could not load category intelligence:" (.getMessage e))
                                       nil))

             overall-confidence (if category-intelligence
                                  (signature/calculate-overall-confidence
                                   (:up confidence-data)
                                   (:down confidence-data)
                                   category-intelligence)
                                  0.0)

             ; Calculate training assistance
             training-assistance (try
                                   (calculate-training-assistance-server
                                    category confidence-data game-context recent-performance)
                                   (catch Exception e
                                     (println "Training assistance error:" (.getMessage e))
                                     nil))

             ; Get current state
             current-time (System/currentTimeMillis)
             category-info (signature/get-or-create-category-state category)
             last-update (:last-update category-info)
             time-since-last-update (- current-time last-update)
             update-interval-ms 7500
             failures (:failures category-info)]

         (let [; Manual circuit breaker check (don't call the function that modifies state)
               circuit-breaker-ok? (< failures 5)
               time-interval-ok? (> time-since-last-update update-interval-ms)
               high-confidence-update? (and (> time-since-last-update 3000)
                                            (or (> (:up confidence-data) 0.6)
                                                (> (:down confidence-data) 0.6))
                                            (> (Math/abs (- (:up confidence-data)
                                                            (:down confidence-data))) 0.15))

               should-update-intelligence? (and circuit-breaker-ok?
                                                (or time-interval-ok?
                                                    high-confidence-update?))]

           (println "  Circuit breaker OK?:" circuit-breaker-ok?)
           (println "  Time interval OK?:" time-interval-ok?)
           (println "  High confidence update?:" high-confidence-update?)
           (println "  Should update intelligence?:" should-update-intelligence?)

           ; Always update the last-update timestamp when we attempt
           (when should-update-intelligence?
             (swap! signature/category-state update-in [category :last-update] (constantly current-time)))

           ; Perform update
           (let [update-result (if should-update-intelligence?
                                 (do
                                   (let [eeg-sample (try
                                                      (when-let [recent-data (signature/get-recent-data 1)]
                                                        (first recent-data))
                                                      (catch Exception e
                                                        (println "Could not get EEG sample:" (.getMessage e))
                                                        nil))

                                         result (signature/update-category-thread-safe!
                                                 category confidence-data eeg-sample
                                                 game-context recent-performance)]

                                     ; Handle success/failure properly
                                     (if (:success result)
                                       (do
                                         (println "✅ Category update succeeded for:" category)
                                         ; Reset failures on success
                                         (swap! signature/category-state update-in [category :failures] (constantly 0)))
                                       (do
                                         (println "❌ Category update failed for:" category ":" (:error result))
                                         ; Only increment failures for actual errors, not skips
                                         (when (not= (:error result) "lock-timeout")
                                           (swap! signature/category-state update-in [category :failures] inc))))
                                     result))
                                 (do
                                   (println "Skipping category update for:" category)
                                   (cond
                                     (not circuit-breaker-ok?) (println "  Reason: Circuit breaker open (" failures "failures)")
                                     (not time-interval-ok?) (println "  Reason: Time interval not met (" time-since-last-update "ms)")
                                     :else (println "  Reason: Conditions not met"))
                                   {:success true :skipped true :reason "conditions-not-met"}))

                 ; Build response - ensure the original brain-response structure is returned
                 enhanced-response (-> brain-response
                                       (assoc :confidence overall-confidence)
                                       (assoc :training-assistance training-assistance)
                                       (assoc :signature-enhanced (:success update-result))
                                       (assoc :intelligence-updated (and (:success update-result)
                                                                         (not (:skipped update-result)))))]
             (try
               (println "  Confidence - Up:" (format-number "%.4f" (or (:up confidence-data) 0.0))
                        "Down:" (format-number "%.4f" (or (:down confidence-data) 0.0)))
               (println "  Overall confidence:" (format-number "%.4f" (or overall-confidence 0.0)))
               (catch Exception e
                 (println "  Confidence - Up:" (:up confidence-data) "Down:" (:down confidence-data))
                 (println "  Overall confidence:" overall-confidence)))
             (println "  Update attempted:" should-update-intelligence?)
             (println "  Update successful:" (:success update-result))
             (println "  Intelligence updated:" (:intelligence-updated enhanced-response))
             enhanced-response)))

       (catch InterruptedException e
         (println "Brain activity response interrupted")
         {:up 0.0 :down 0.0 :confidence 0.0 :error "interrupted"})

       (catch Exception e
         (println "Error in enhanced brain activity response:" (.getMessage e))
         (.printStackTrace e)
         {:up 0.0 :down 0.0 :confidence 0.0 :error (.getMessage e)}))))


#?(:cljs
   (defn process-bci-input
     "BCI input processing with client-side adaptive threshold - FIXED"
     []
     (let [state @pong-state/state
           connected? (get-in state [:bci :device-connected?])
           matching? (get-in state [:bci :matching?])
           confidence (or (get-in state [:bci :confidence]) {:up 0.0 :down 0.0})
           last-action-time (get-in state [:bci :last-action-time] 0)
           current-time (js/Date.now)
           time-since-action (- current-time last-action-time)]
       (if (and connected? matching?)
         (do
           ; Get base threshold from server data - FIXED to use correct path
           (let [base-threshold (or (get-in state [:bci :confidence :dynamic-threshold]) 0.05)]
             ; Calculate client-side adaptive threshold
             (let [adaptive-threshold (get-adaptive-threshold base-threshold)
                   min-action-interval 100]
                 (if (> time-since-action min-action-interval)
                   (do
                     (let [up-confidence (or (:up confidence) 0.0)
                           down-confidence (or (:down confidence) 0.0)
                           confidence-gap (Math/abs (- up-confidence down-confidence))
                           min-gap 0.002
                           avg-confidence-level (/ (+ up-confidence down-confidence) 2)

                           ; If confidence levels are high, require bigger gap
                           ; If confidence levels are low, allow smaller gap
                           adaptive-min-gap (cond
                                              (> avg-confidence-level 0.6) (* min-gap 1.5)
                                              (> avg-confidence-level 0.4) min-gap
                                              (> avg-confidence-level 0.2) (* min-gap 0.5)
                                              :else (* min-gap 0.25))

                           ; Use much lower thresholds to allow movement
                           effective-threshold (min adaptive-threshold 0.15) ; Cap at 0.15

                           should-move-up? (and (>= up-confidence effective-threshold)
                                                (> up-confidence down-confidence)
                                                (> confidence-gap adaptive-min-gap))
                           should-move-down? (and (>= down-confidence effective-threshold)
                                                  (> down-confidence up-confidence)
                                                  (> confidence-gap adaptive-min-gap))]
                       (cond
                         should-move-up?
                         (do
                           (js/console.log "🔴 BCI: UP action triggered!")
                           (pong-state/move-paddle! :up)
                           (swap! pong-state/state assoc-in [:bci :last-action-time] current-time)
                           (update-performance-history :up confidence true))

                         should-move-down?
                         (do
                           (js/console.log "🔵 BCI: DOWN action triggered!")
                           (pong-state/move-paddle! :down)
                           (swap! pong-state/state assoc-in [:bci :last-action-time] current-time)
                           (update-performance-history :down confidence true))

                         :else
                         (do
                           (js/console.log "⚪ BCI: No action - thresholds not met")
                           ; Still track confidence readings even when no action taken
                           (let [poll-trigger (get-in state [:bci :poll-trigger] 0)]
                             (when (= 0 (mod poll-trigger 5))
                               (js/console.log "Recording no-action data point")
                               (update-performance-history :none confidence false)))))))
                   (do
                     (js/console.log "Time interval check failed - too soon since last action")))))))
         (do
           ;(js/console.log "❌ BCI conditions not met:")
           (when (not connected?) (js/console.log "❌  - Not connected"))
           (when (not matching?) (js/console.log "❌  - Not matching"))))))