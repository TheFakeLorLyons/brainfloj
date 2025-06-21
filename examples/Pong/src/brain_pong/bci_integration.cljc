(ns brain-pong.bci-integration
  (:require [hyperfiddle.electric3 :as e]
            [brain-pong.game-state :as pong-state]
            [brain-pong.training-wheels :as wheels]
            [hyperfiddle.electric-dom3 :as dom]
            [missionary.core :as m]
            [mount.core :as mount]
            #?(:clj [clojure.core.async :as async :refer [go go-loop chan >! <! timeout poll! close! pipeline-async mult tap untap alt! alts!]])
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

#?(:clj
   (do
     (def pipeline-config
       {:raw-eeg-buffer-size 100        ; Buffer 1 seconds at 100Hz
        :feature-buffer-size 100        ; Buffer ~5 seconds of features at 20Hz
        :confidence-buffer-size 50      ; Buffer ~2.5 seconds of confidence at 20Hz
        :output-buffer-size 20          ; Small output buffer for immediate consumption
        :feature-extraction-rate 30     ; Extract features at 20Hz
        :confidence-calculation-rate 15 ; Calculate confidence at 15Hz
        :backpressure-threshold 0.8     ; Start dropping when 80% full
        :parallelism 2                  ; Parallel processing threads
        :ingestion-rate-ms 50           ; 20Hz ingestion
        :poll-rate-ms 50})))            ; 10Hz polling

(defn get-pipeline-stats []
  (get-in @pong-state/state [:bci :pipeline-stats]))

(defn update-pipeline-stats! [update-fn & args]
  (apply swap! pong-state/state update-in [:bci :pipeline-stats] update-fn args))

(defn get-pipeline-running? []
  (get-in @pong-state/state [:bci :pipeline-running?] false))

(defn set-pipeline-running! [running?]
  (println "🔧 Setting pipeline-running? to:" running?)
  (swap! pong-state/state assoc-in [:bci :pipeline-running?] running?)
  (let [actual-value (get-in @pong-state/state [:bci :pipeline-running?])]
    (println "🔧 Verified pipeline-running? is now:" actual-value)
    (when (not= running? actual-value)
      (println "⚠️ WARNING: pipeline-running? mismatch! Expected:" running? "Actual:" actual-value))
    actual-value))

(defn get-bci-state []
  (get-in @pong-state/state [:bci]))

(defn update-bci-state! [update-fn & args]
  (apply swap! pong-state/state update-in [:bci] update-fn args))

(defn get-confidence-data []
  (get-in @pong-state/state [:bci :confidence]))

(defn update-confidence-data! [confidence-data]
  (swap! pong-state/state assoc-in [:bci :confidence] confidence-data))

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

#?(:clj
   (defn calculate-recent-performance-clj
     "Calculate recent performance metrics from the :action-history buffer in :bci"
     [state-atom]
     (try
       (let [state @state-atom
             bci-state (get-bci-state)
             action-history (:action-history bci-state) ; Should be a vector of maps
             current-time (System/currentTimeMillis)
             recent-window 2000
             recent-actions (filter #(<= (- current-time (:timestamp %)) recent-window)
                                    action-history)
             total-actions (count recent-actions)
             successful-actions (count (filter :successful recent-actions))
             assisted-actions (count (filter :assisted recent-actions))
             natural-actions (- successful-actions assisted-actions)
             success-rate (if (pos? total-actions)
                            (/ successful-actions total-actions)
                            0.5)
             confidence-values (keep :confidence recent-actions)
             avg-confidence (if (seq confidence-values)
                              (/ (reduce + confidence-values) (count confidence-values))
                              0.3)
             recent-sample-count (count (filter #(<= (- current-time (:timestamp %)) 5000)
                                                action-history))]

         {:success-rate success-rate
          :total-actions total-actions
          :successful-actions successful-actions
          :assisted-actions assisted-actions
          :natural-actions natural-actions
          :avg-confidence avg-confidence
          :recent-sample-count recent-sample-count})

       (catch Exception e
         (println "Error calculating recent performance:" (.getMessage e))
         {:success-rate 0.5
          :total-actions 0
          :successful-actions 0
          :assisted-actions 0
          :natural-actions 0
          :avg-confidence 0.3
          :recent-sample-count 0}))))

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

#?(:cljs
   (defn update-client-performance-tracking!
     "Performance tracking that feeds back to server"
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

; Used in both the synchronous and asynchronous versions in order to call the back end processing
#?(:clj
   (defn match-brain-activity-server
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
           (println "recent performance " recent-performance)
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

; Synchronous input processing
#?(:cljs
   (defn process-bci-input
     "BCI input processing with client-side adaptive threshold"
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
           ; Get base threshold from server data
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
         ; BCI connection conditions not met
         (when (not connected?) (js/console.log "❌  - Not connected"))
         (when (not matching?) (js/console.log "❌  - Not matching"))))))

;; Async Code Begins
#?(:clj
   (do
     (defn create-pipeline-channels []
       {:raw-eeg (chan (:raw-eeg-buffer-size pipeline-config))
        :features (chan (:feature-buffer-size pipeline-config))
        :confidence (chan (:confidence-buffer-size pipeline-config))
        :output (chan (:output-buffer-size pipeline-config))})

     (defn create-control-channels []
       {:ingestion-control (chan 2)
        :feature-control (chan 2)
        :confidence-control (chan 2)
        :output-control (chan 2)})

     (defn create-multiplexers [channels]
       {:confidence-mult (mult (:confidence channels))})

     (defn safe-put!
       ([channel value]
        (try
          (async/put! channel value)
          (catch Exception e
            (println "Error putting to channel:" (.getMessage e))
            false)))
       ([channel value drop-counter-key]
        (try
          (async/put! channel value)
          (catch Exception e
            (update-bci-state! update-in [:pipeline :errors] inc)
            false))))

     (defn should-drop-sample? [channel buffer-size]
       (try
         (let [current-size (count (.buf channel))
               threshold (* buffer-size (:backpressure-threshold pipeline-config))]
           (> current-size threshold))
         (catch Exception e
           false)))

     (defn start-eeg-ingestion! [channels control-channels]
       (let [{:keys [raw-eeg]} channels
             {:keys [ingestion-control]} control-channels]
         (println "🎯 Starting EEG ingestion thread...")
         (go
           (loop [iteration 0 consecutive-errors 0]
             (if-not (:running? (get-bci-state))
               (println "EEG ingestion stopping - not running")
               (let [timeout-ch (timeout (:ingestion-rate-ms pipeline-config))
                     [val port] (alts! [ingestion-control timeout-ch])]
                 (cond
                   (= port ingestion-control)
                   (when (= val :stop)
                     (println "🛑 EEG ingestion received stop signal"))

                   (= port timeout-ch)
                   (let [[next-iteration next-errors]
                         (try
                           (let [recording-status (try
                                                    (and (bound? #'state/recording?) @state/recording?)
                                                    (catch Exception e false))
                                 eeg-data-count (try
                                                  (count @state/eeg-data)
                                                  (catch Exception e 0))]
                             (if (and recording-status (>= eeg-data-count 20))
                               (let [recent-data (signature/get-recent-data 1.0)]
                                 (cond
                                   (and (seq recent-data) (>= (count recent-data) 20))
                                   (do
                                     (when (= 0 (mod iteration 20))
                                       (println "📥 Got" (count recent-data) "recent samples for processing"))
                                     (let [processed-data (mapv (fn [sample]
                                                                  (if (map? sample)
                                                                    sample
                                                                    {:eeg sample
                                                                     :timestamp (System/currentTimeMillis)}))
                                                                recent-data)]
                                       (if (>! raw-eeg processed-data)
                                         (do
                                           (update-bci-state! update-in [:pipeline-stats :samples-received] + (count processed-data))
                                           [(inc iteration) 0])
                                         (do
                                           (println "⚠️ Failed to put samples into raw-eeg channel")
                                           [(inc iteration) (inc consecutive-errors)]))))
                                   :else
                                   (do
                                     (when (= 0 (mod iteration 50))
                                       (println "⚠️ Insufficient recent data - Count:" (count recent-data) "Required: 20"))
                                     [iteration consecutive-errors])))
                               (do
                                 (when (= 0 (mod iteration 100))
                                   (println "⏳ Waiting for recording state and data. Recording:" recording-status "Data count:" eeg-data-count))
                                 [iteration consecutive-errors])))
                           (catch Exception e
                             (println "❌ EEG ingestion error:" (.getMessage e))
                             (update-bci-state! update-in [:pipeline-stats :errors] inc)
                             (let [new-error-count (inc consecutive-errors)]
                               (if (> new-error-count 10)
                                 (do
                                   (println "❌ Too many consecutive errors, pausing...")
                                   (<! (timeout 2000))
                                   [iteration 0])
                                 [iteration new-error-count]))))]
                     (recur next-iteration next-errors))

                   :else
                   (recur iteration consecutive-errors))))))))

     (defn extract-features-safe [eeg-batch]
       (try
         (when (seq eeg-batch)
           (println "🔬 Processing EEG batch with" (count eeg-batch) "samples")
           (cond
             ; If already processed with enhanced response, use directly
             (and (map? (first eeg-batch))
                  (contains? (first eeg-batch) :confidence)
                  (contains? (first eeg-batch) :up)
                  (contains? (first eeg-batch) :down))
             (do
               (println "📋 Using pre-processed enhanced response")
               (first eeg-batch))

             ; Otherwise, just structure the data for the confidence stage
             :else
             (do
               (println "📊 Preparing data for enhanced processing")
               {:raw-eeg-batch eeg-batch
                :timestamp (System/currentTimeMillis)
                :sample-count (count eeg-batch)
                :processing-mode "raw-for-enhancement"})))
         (catch Exception e
           (println "❌ Feature extraction error:" (.getMessage e))
           (update-bci-state! update-in [:pipeline-stats :errors] inc)
           nil)))

     (defn start-feature-extraction! [channels control-channels]
       (let [{:keys [raw-eeg features]} channels
             {:keys [feature-control]} control-channels]
         (println "🧠 Starting feature extraction thread...")
         (go-loop []
           (if-not (:running? (get-bci-state))
             (println "Feature extraction stopping - not running")
             (alt!
               feature-control ([msg]
                                (when (= msg :stop)
                                  (println "Feature extraction received stop signal")))

               raw-eeg ([eeg-batch]
                        (when eeg-batch
                          (try
                            (when-let [extracted-features (extract-features-safe eeg-batch)]
                              (when (safe-put! features extracted-features :features-dropped)
                                (update-bci-state! update-in [:pipeline-stats :features-extracted] inc)))
                            (catch Exception e
                              (println "Feature processing error:" (.getMessage e))
                              (update-bci-state! update-in [:pipeline-stats :errors] inc)))
                          (recur))))))))

     (defn start-signature-enhancement-background! [channels category]
       "Background signature enhancement - non-blocking"
       (let [{:keys [confidence]} channels
             {:keys [confidence-mult]} (:multiplexers @channels)
             enhancement-chan (chan 5)]

         ; Tap into confidence stream for signature enhancement
         (tap confidence-mult enhancement-chan)

         (go-loop []
           (when (:running? (:bci @pong-state/state))
             (try
               (when-let [confidence-data (<! enhancement-chan)]
                 ; Existing signature enhancement logic - but non-blocking
                 (when (signature/should-attempt-category-update? category confidence-data)
                   (let [eeg-sample (signature/get-recent-data 1)]
                     (when eeg-sample
                       ; Run in separate thread to not block pipeline
                       (future
                         (signature/update-category-thread-safe!
                          category confidence-data (first eeg-sample)
                          {:timestamp (System/currentTimeMillis)}
                          nil))))))
               (catch Exception e
                 (println "Background enhancement error:" (.getMessage e))))
             (recur)))

         ; Cleanup
         (untap confidence-mult enhancement-chan)
         (close! enhancement-chan)))

     (defn calculate-confidence-safe [features profile-name category state-atom]
       (try
         (cond
           ; If features already contain confidence scores
           (and (map? features)
                (contains? features :confidence)
                (contains? features :up)
                (contains? features :down))
           (do
             (println "✅ Using existing enhanced confidence scores")
             (let [result (merge features
                                 {:timestamp (System/currentTimeMillis)
                                  :profile-name profile-name
                                  :category category
                                  :processing-mode "async-pipeline-enhanced"
                                  :fresh? true})]
               (println "📋 Existing scores - Up:" (:up result) "Down:" (:down result))
               result))

           ; Call the enhanced response server
           :else
           (do
             (println "🧮 Calling enhanced-brain-activity-response-server with fresh data")
             (let [recent-performance (calculate-recent-performance-clj state-atom)
                   current-state @state-atom
                   game-context {:ball-position-validates-intent
                                 (let [ball (get-in current-state [:game :ball])
                                       paddle (get-in current-state [:game :player-paddle])]
                                   (and ball paddle
                                        (< (Math/abs (- (:x ball) (:x paddle))) 200)))
                                 :game-active (get-in current-state [:game :playing?])
                                 :timestamp (System/currentTimeMillis)}

                   enhanced-result (match-brain-activity-server
                                    profile-name
                                    category
                                    game-context
                                    recent-performance)]

               (if (:error enhanced-result)
                 (do
                   (println "❌ Enhanced response error:" (:error enhanced-result))
                   {:up 0.0 :down 0.0 :confidence 0.0
                    :error (:error enhanced-result)
                    :processing-mode "error"
                    :fresh? false
                    :timestamp (System/currentTimeMillis)})
                 (let [final-result (merge enhanced-result
                                           {:timestamp (System/currentTimeMillis)
                                            :profile-name profile-name
                                            :category category
                                            :processing-mode "async-pipeline-calculated"
                                            :fresh? true
                                            :feature-data features
                                            :recent-performance recent-performance
                                            :game-context game-context})]
                   (println "✅ FINAL RESULT:")
                   (println "  Final up:" (:up final-result))
                   (println "  Final down:" (:down final-result))
                   (println "  Final confidence:" (:confidence final-result))

                   ; Ensure actual numbers, not nil
                   (if (and (:up final-result) (:down final-result) (:confidence final-result))
                     final-result
                     (do
                       (println "⚠️ WARNING: Enhanced result missing values, using defaults")
                       (merge final-result
                              {:up (or (:up final-result) 0.5)
                               :down (or (:down final-result) 0.5)
                               :confidence (or (:confidence final-result) 0.25)}))))))))
         (catch Exception e
           (println "❌ Enhanced confidence calculation error:" (.getMessage e))
           (update-bci-state! update-in [:pipeline-stats :errors] inc)
           {:up 0.0 :down 0.0 :confidence 0.0
            :error (.getMessage e)
            :processing-mode "error"
            :fresh? false
            :timestamp (System/currentTimeMillis)})))

     (defn start-confidence-calculation! [channels control-channels profile-name state-atom]
       (let [{:keys [features confidence]} channels
             {:keys [confidence-control]} control-channels]
         (println "🧮 Starting confidence calculation thread...")
         (go-loop []
           (if-not (:running? (get-bci-state))
             (println "Confidence calculation stopping - not running")
             (alt!
               confidence-control ([msg]
                                   (when (= msg :stop)
                                     (println "Confidence calculation received stop signal")))

               ; Add a timeout to ensure we continuous calling even without new features
               (timeout 500) ([_]
                              (try
                                (println "🔄 Timeout-triggered confidence calculation")
                                (let [confidence-result (calculate-confidence-safe
                                                         nil  ; No specific features, will trigger enhanced response
                                                         profile-name
                                                         "pong"
                                                         state-atom)]
                                  (when (safe-put! confidence confidence-result :confidence-dropped)
                                    (update-bci-state! update-in [:pipeline-stats :confidence-calculated] inc)))
                                (catch Exception e
                                  (println "Timeout confidence processing error:" (.getMessage e))
                                  (update-bci-state! update-in [:pipeline-stats :errors] inc)))
                              (recur))

               features ([feature-data]
                         (when feature-data
                           (try
                             (println "📥 Feature-triggered confidence calculation")
                             (let [confidence-result (calculate-confidence-safe
                                                      feature-data
                                                      profile-name
                                                      "pong"
                                                      state-atom)]
                               (when (safe-put! confidence confidence-result :confidence-dropped)
                                 (update-bci-state! update-in [:pipeline-stats :confidence-calculated] inc)))
                             (catch Exception e
                               (println "Feature confidence processing error:" (.getMessage e))
                               (update-bci-state! update-in [:pipeline-stats :errors] inc))))
                         (recur)))))))

     (defn calculate-training-assistance [confidence-data category game-context recent-performance]
       (try
         (let [profile-name (:profile-name confidence-data)
               category-data (lexi/load-category category)
               assistance-level (when category-data
                                  (wheels/calculate-assistance-level
                                   profile-name category-data recent-performance))
               assistance-direction (when (and assistance-level (> assistance-level 0.1))
                                      (wheels/should-provide-assistance?
                                       game-context assistance-level))]
           (when assistance-direction
             {:direction assistance-direction
              :assistance-level assistance-level
              :reason "adaptive-assistance"
              :timestamp (System/currentTimeMillis)}))
         (catch Exception e
           (println "Training assistance error:" (.getMessage e))
           nil)))

     (defn start-confidence-consumer! [channels state-atom]
       (let [{:keys [confidence]} channels]
         (println "🔄 Starting CONFIDENCE consumer thread...")
         (go
           (loop [consumed-count 0]
             (if-not (:running? (get-bci-state))
               (println "🛑 Confidence consumer stopping")
               (when-let [val (<! confidence)]
                 (let [current-time (System/currentTimeMillis)
                       enhanced-data (merge val
                                            {:consumer-processed-time current-time
                                             :consumer-sequence consumed-count
                                             :age 0
                                             :fresh? true
                                             :source "confidence-consumer"
                                             :timestamp current-time})]

                   (println "📤 CONSUMER #" consumed-count " - Up:" (:up enhanced-data) "Down:" (:down enhanced-data))

                   ; Update state to trigger Electric reactivity
                   (swap! state-atom
                          (fn [current-state]
                            (-> current-state
                                (assoc-in [:bci :confidence] enhanced-data)
                                (assoc-in [:bci :latest-output] enhanced-data)
                                (assoc-in [:bci :reactive-timestamp] current-time)    ; Key for reactivity
                                (assoc :last-bci-update current-time))))              ; Also at root level

                   (recur (inc consumed-count)))))))))

     (defn stop-optimized-bci-pipeline! []
       (println "⏹️ Stopping UNIFIED optimized BCI pipeline")
       (try
         (update-bci-state! assoc :running? false)

         ; Wait for threads to stop
         (Thread/sleep 200)

         ; Close all channels
         (let [state (get-bci-state)
               all-channels (:pipeline-channels state)]
           (when-let [channels (:channels (:pipeline-channels all-channels))]
             (doseq [chan (vals channels)]
               (try
                 (async/close! chan)
                 (catch Exception e
                   (println "Error closing channel:" (.getMessage e))))))

           (when-let [control-channels (:control-channels (:pipeline-channels all-channels))]
             (doseq [control-chan (vals control-channels)]
               (try
                 (async/close! control-chan)
                 (catch Exception e
                   (println "Error closing control channel:" (.getMessage e))))))

           (when-let [multiplexers (:multiplexers (:pipeline-channels all-channels))]
             (doseq [multi-chan (vals multiplexers)]
               (try
                 (async/close! multi-chan)
                 (catch Exception e
                   (println "Error closing control channel:" (.getMessage e))))))

           (update-bci-state! assoc  :pipeline-channels {:channels {}
                                                         :control-channels {}
                                                         :multiplexers {}}
                              :latest-output {:up 0.0
                                              :down 0.0
                                              :confidence 0.0
                                              :timestamp (System/currentTimeMillis)
                                              :fresh? false})

           (println "✅ UNIFIED Pipeline stopped successfully")
           {:stopped true})

         (catch Exception e
           (println "❌ UNIFIED Pipeline stop error:" (.getMessage e))
           {:stopped false :error (.getMessage e)})))

     (defn get-latest-bci-output []
       (let [current-state @pong-state/state
             current-output (get-in current-state [:bci :latest-output])
             current-time (System/currentTimeMillis)
             generation-time (or (:generation-time current-output)
                                 (:timestamp current-output)
                                 0)
             age (- current-time generation-time)
             pipeline-running? (:running? current-state)]

         ; Create the output data structure
         (let [output-data {:up (or (:up current-output) 0.0)
                            :down (or (:down current-output) 0.0)
                            :confidence (or (:confidence current-output) 0.0)
                            :age age
                            :generation-time generation-time
                            :timestamp current-time
                            :pipeline-running? (boolean pipeline-running?)
                            :processing-mode (or (:processing-mode current-output) "unknown")
                            :fresh? (< age 1000)
                            :dynamic-threshold (or (:dynamic-threshold current-output) 0.05)
                            :confidence-gap (Math/abs (- (or (:up current-output) 0.0)
                                                         (or (:down current-output) 0.0)))
                            :separation-score (or (:separation-score current-output) 0.0)
                            :triangulation-quality (or (:triangulation-quality current-output) 0.0)
                            :intelligence-grade (or (:intelligence-grade current-output) "unknown")}]

           ;(println "🔍 SERVER OUTPUT DEBUG:")
           ;(println "  Pipeline running:" pipeline-running?)
           ;(println "  Raw up value:" (:up current-output))
           ;(println "  Raw down value:" (:down current-output))
           ;(println "  Output data up:" (:up output-data))
           ;(println "  Output data down:" (:down output-data))
           ;(println "  Processing mode:" (:processing-mode output-data))
           ;(println "  Fresh:" (:fresh? output-data))
           output-data)))

     (defn update-confidence-from-pipeline [confidence-data]
       "Server-side ONLY function to update confidence state"
       (let [current-time (System/currentTimeMillis)
             enhanced-data (merge confidence-data
                                  {:server-sync-time current-time
                                   :age 0  ; Fresh from server
                                   :fresh? true
                                   :source "server-pipeline"})]

         ;(println "📤 SERVER updating confidence state:")
         ;(println "  Up:" (:up enhanced-data))
         ;(println "  Down:" (:down enhanced-data))
         ;(println "  Mode:" (:processing-mode enhanced-data))

         ; Single atomic update to main state
         (swap! pong-state/state
                (fn [current-state]
                  (-> current-state
                      (assoc-in [:bci :confidence] enhanced-data)
                      (assoc-in [:bci :latest-output] enhanced-data)
                      (assoc-in [:bci :last-poll-time] current-time))))

         ; Verification
         (let [updated-confidence (get-in @pong-state/state [:bci :confidence])]
           (println "✅ Server state update verified - Up:" (:up updated-confidence)))

         enhanced-data))


     (defn start-output-manager! [channels control-channels category]
       "Manages pipeline output, no state mutation"
       (let [{:keys [confidence output]} channels
             {:keys [output-control]} control-channels]
         (go-loop [output-count 0]
           (if-not (:running? (get-bci-state))
             (println "Output manager stopping - not running")
             (let [next-count
                   (alt!
                     output-control ([msg]
                                     (when (= msg :stop)
                                       (println "Output manager received stop signal"))
                                     output-count)

                     confidence ([val]
                                 (if val
                                   (do
                                     (try
                                       (let [current-time (System/currentTimeMillis)
                                             final-output (merge val
                                                                 {:processing-mode "async-pipeline"
                                                                  :fresh? true
                                                                  :timestamp current-time
                                                                  :generation-time current-time
                                                                  :output-sequence output-count
                                                                  :age 0})]

                                         ;(println "  Output manager processing:")
                                         ;(println "  Up:" (format "%.4f" (:up final-output)))
                                         ;(println "  Down:" (format "%.4f" (:down final-output)))

                                         (update-bci-state! assoc :latest-output final-output)

                                         (update-bci-state!
                                          (fn [p]
                                            (-> p
                                                (assoc :latest-output final-output)
                                                (update :action-history
                                                        #(let [history (conj (or % []) final-output)]
                                                           (if (> (count history) 20)
                                                             (subvec history (- (count history) 20))
                                                             history))))))

                                         (println "✅ Pipeline state updated - Up:" (:up final-output) "Down:" (:down final-output))
                                         (update-bci-state! update-in [:pipeline-stats :outputs-generated] inc)
                                         (inc output-count))
                                       (catch Exception e
                                         (println "❌ Output processing error:" (.getMessage e))
                                         (update-bci-state! update-in [:pipeline-stats :errors] inc)
                                         output-count))
                                     output-count))

                                 :default
                                 (do
                                   (<! (timeout 50))
                                   output-count)))]
               (recur next-count))))))


     (defn start-optimized-bci-pipeline! [profile-name category]
       (try
         ;(println "Starting UNIFIED optimized BCI pipeline with profile:" profile-name "category:" category)
         (let [recording? (try
                            (and (bound? #'state/recording?) @state/recording?)
                            (catch Exception e
                              (println "Recording check failed:" (.getMessage e))
                              false))
               eeg-available? (try
                                (pos? (count @state/eeg-data))
                                (catch Exception e
                                  (println "EEG data check failed:" (.getMessage e))
                                  false))]

           ; Stop existing pipeline
           (when (:running? (get-pipeline-stats))
             (println "📋 Stopping existing pipeline...")
             (stop-optimized-bci-pipeline!)
             (Thread/sleep 300))

           ; Initialize pipeline
           (let [channels (create-pipeline-channels)
                 control-channels (create-control-channels)
                 multiplexers (create-multiplexers channels)]

             (update-bci-state! assoc
                                :running? true
                                :profile-name profile-name
                                :category category
                                :pipeline-channels {:channels channels
                                                    :control-channels control-channels
                                                    :multiplexers multiplexers}
                                :latest-output  {:up 0.0
                                                 :down 0.0
                                                 :confidence 0.0
                                                 :timestamp (System/currentTimeMillis)
                                                 :processing-mode "pipeline-starting"
                                                 :fresh? true}
                                :pipeline-stats {:samples-received 0
                                                 :samples-processed 0
                                                 :features-extracted 0
                                                 :confidence-calculated 0
                                                 :outputs-generated 0
                                                 :errors 0
                                                 :pipeline-starts (inc (get-in (get-pipeline-stats) [:stats :pipeline-starts] 0))})

             ; Update main state to show pipeline is starting
             (swap! pong-state/state assoc-in [:bci :confidence :processing-mode] "pipeline-starting")

             ; Start threads
             (start-eeg-ingestion! channels control-channels)
             (Thread/sleep 100)
             (start-feature-extraction! channels control-channels)
             (Thread/sleep 100)
             (start-confidence-calculation! channels control-channels profile-name pong-state/state)
             (Thread/sleep 100)
             (start-output-manager! channels control-channels category)
             (Thread/sleep 200)
             (start-confidence-consumer! channels pong-state/state)
             (Thread/sleep 200)

             (println "✅ UNIFIED Pipeline started successfully")
             {:started true
              :profile-name profile-name
              :category category
              :timestamp (System/currentTimeMillis)}))

         (catch Exception e
           (println "❌ UNIFIED PIPELINE STARTUP ERROR:" (.getMessage e))
           (swap! pong-state/state assoc-in [:bci :confidence :processing-mode] "startup-error")
           {:started false
            :error (.getMessage e)
            :timestamp (System/currentTimeMillis)})))))

#?(:cljs
   (do
     (defn get-client-latest-bci-output []
       (let [current-state (get-bci-state)
             current-output (:latest-output current-state)
             current-time (js/Date.now)
             generation-time (or (:generation-time current-output)
                                 (:timestamp current-output)
                                 0)
             age (- current-time generation-time)
             pipeline-running? (:running? current-state)]

         (let [fallback-confidence (get-confidence-data)
               actual-up (if (and (:up current-output) (not (zero? (:up current-output))))
                           (:up current-output)
                           (:up fallback-confidence 0.0))
               actual-down (if (and (:down current-output) (not (zero? (:down current-output))))
                             (:down current-output)
                             (:down fallback-confidence 0.0))
               actual-confidence (if (and (:confidence current-output) (not (zero? (:confidence current-output))))
                                   (:confidence current-output)
                                   (:confidence fallback-confidence 0.0))

               output-data {:up actual-up
                            :down actual-down
                            :confidence actual-confidence
                            :age age
                            :generation-time generation-time
                            :timestamp current-time
                            :pipeline-running? (boolean pipeline-running?)
                            :processing-mode (or (:processing-mode current-output) "unknown")
                            :fresh? (< age 1000)
                            :dynamic-threshold (or (:dynamic-threshold current-output) 0.05)
                            :confidence-gap (Math/abs (- actual-up actual-down))
                            :separation-score (or (:separation-score current-output) 0.0)
                            :triangulation-quality (or (:triangulation-quality current-output) 0.0)
                            :intelligence-grade (or (:intelligence-grade current-output) "unknown")
                            :source "server-output-fixed"}]

           output-data)))

     (defn apply-smart-assistance!
       "Apply training wheels assistance with intelligence-based scaling"
       [assistance-info confidence-data]
       (let [{:keys [direction assistance-level]} assistance-info
             base-speed 1.0]

         (wheels/apply-progressive-boost direction assistance-level base-speed)

         (swap! pong-state/state update-in [:training-wheels :assistance-count] (fnil inc 0))

         (println "Training assistance applied:" direction "at level" assistance-level)))

     (defn process-async-bci-input! []
       "Async BCI input processing - pure/read-only with adaptive thresholds"
       (let [fresh-state @pong-state/state
             output (get-client-latest-bci-output)
             bci (:bci fresh-state)
             confidence (:confidence bci)
             connected? (get-in fresh-state [:bci :device-connected?])
             matching? (get-in fresh-state [:bci :matching?])
             streaming? (get-in fresh-state [:bci :streaming?])
             pipeline-running? (get-in fresh-state [:bci :pipeline-running?])
             confidence-data (get-in fresh-state [:bci :confidence])
             last-action-time (get-in fresh-state [:bci :last-action-time] 0)
             server-timestamp (get confidence-data :timestamp 0)
             current-time (js/Date.now)
             actual-age (- current-time server-timestamp)]

         (js/console.log "🔍 === ASYNC BCI INPUT ===")
         (js/console.log "🧠 Full BCI state:" (clj->js bci))
         (js/console.log "🔍 Connected:" connected? "Matching:" matching? "Streaming:" streaming?)
         (js/console.log "🔍 Pipeline running:" pipeline-running? "Connected:" connected?)
         (js/console.log "🔍 Raw Confidence:" confidence-data)
         (js/console.log "🔍 Confidence age:" (:age confidence-data) "ms" "Last Action:" actual-age)
         #_(js/console.log "🔍 Age since last query:" (:age output) "ms")
         (js/console.log "🔍 Up:" (:up confidence-data) "Down:" (:down confidence-data) "Confidence:" (:confidence confidence-data))
         (js/console.log "🔍 Fresh:" (:fresh? confidence-data))

         (when (and connected? matching? streaming? pipeline-running? confidence-data
                    (> actual-age 120))
           (let [up-confidence (get confidence-data :up 0.0)
                 down-confidence (get confidence-data :down 0.0)
                 is-fresh? (get confidence-data :fresh? false)
                 consumer-time (get confidence-data :consumer-processed-time 0)
                 threshold (get-in fresh-state [:bci :threshold])
                 confidence-gap (Math/abs (- up-confidence down-confidence))
                 min-gap 0.01]

             (js/console.log "🎯 Action check - Fresh:" is-fresh? "Age:" actual-age "Gap:" confidence-gap)
             (js/console.log "🎯 Thresholds - Base:" threshold "Min gap:" min-gap)

             (when (and is-fresh? (< actual-age 2000))
               (cond
                 (and (>= up-confidence threshold)
                      (> up-confidence down-confidence)
                      (> confidence-gap min-gap))
                 (do
                   (js/console.log "🔴 BCI: UP action! (up:" up-confidence " down:" down-confidence ")")
                   (pong-state/move-paddle! :up)
                   (swap! pong-state/state assoc-in [:bci :last-action-time] current-time))

                 (and (>= down-confidence threshold)
                      (> down-confidence up-confidence)
                      (> confidence-gap min-gap))
                 (do
                   (js/console.log "🔵 BCI: DOWN action! (up:" up-confidence " down:" down-confidence ")")
                   (pong-state/move-paddle! :down)
                   (swap! pong-state/state assoc-in [:bci :last-action-time] current-time))

                 :else
                 (js/console.log "⚪ BCI: No action - thresholds not met")))))))))


(e/defn Process-live-training-integration!
  "Currently Unimplemented
   Main function to handle live training and signature enhancement"
  [category confidence-data eeg-sample game-context recent-performance]
  (e/server
   (let [category-intelligence (lexi/load-category category)]
     (when category-intelligence
       ; Check for live signature enhancement opportunity
       (signature/capture-live-enhancement! category confidence-data eeg-sample game-context)

       ; Check for training wheels assistance
       (let [assistance-info (should-provide-training-assistance?
                              confidence-data game-context category-intelligence recent-performance)]

         (when assistance-info
           (e/client (apply-smart-assistance! assistance-info confidence-data))

           ; Update performance tracking for future assistance calculations
           (wheels/update-game-stats!
            (if (> (:assistance-level assistance-info) 0.5) :assisted :natural))))))))