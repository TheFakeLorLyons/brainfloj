(ns pong.bci-integration
  (:require
   [hyperfiddle.electric :as e]
   #?(:clj  [floj.api :as api])
   #?(:clj  [floj.state :as state])
   #?(:clj  [floj.profiles :as profiles])
   #?(:clj  [floj.stream-manager :as stream])
   #?(:clj  [floj.wave-lexicon :as lexi])
   #?(:cljs [pong.game-state :as pong-state])))

; Server function to get the active profile
(e/def get-active-profile
  (e/server
   (fn []
     (println "Server: Getting active profile")
     #?(:clj
        (try
          ((:get-active-profile @state/state))
          (catch Exception e
            (println "Error getting active profile:" (.getMessage e))
            {:name "default" :error (.getMessage e)}))))))

; Server function to connect to a device
(e/def connect-device-server
  (e/server
   (fn []
     (println "Server: Connecting to BCI device with active profile")
     #?(:clj
        (try
          (let [profile ((:get-active-profile @state/state))
                profile-name (:name profile)
                success? (api/connect-to-default-device profile-name)]
            (println "Connection result:" success? "with profile" profile-name)
            {:success success? :profile-name profile-name})
          (catch Exception e
            (println "Error connecting to device:" (.getMessage e))
            {:success false :error (.getMessage e)}))))))

; Server function to disconnect device
(e/def disconnect-device-server
  (e/server
   (fn []
     (println "Server: Disconnecting from BCI device")
     #?(:clj
        (try
          (when-let [f (:release-board! @state/state)]
            (f @state/shim))
          true
          (catch Exception e
            (println "Error disconnecting:" (.getMessage e))
            false))))))

; Server function to start recording for a specific category
(e/def start-recording-server
  (e/server
   (fn [category]
     (println "Server: Starting recording for category:" category)
     #?(:clj
        (try
          (lexi/start-wave-signature-recording! category "game")
          (catch Exception e
            (println "Error starting recording:" (.getMessage e))
            nil))))))

; Server function to stop recording
(e/def stop-recording-server
  (e/server
   (fn []
     (println "Server: Stopping recording")
     #?(:clj
        (try
          (lexi/stop-wave-signature-recording!)
          (catch Exception e
            (println "Error stopping recording:" (.getMessage e))
            nil))))))

; Server function to match brain activity and return confidence values
(e/def match-brain-activity-server
  (e/server
   (fn []
     #?(:clj
        (try
          (when-let [board-shim @state/shim]
            (let [profile ((:get-active-profile @state/state))
                  profile-name (:name profile)

                  eeg-data @state/eeg-data
                  current-data (last (:data eeg-data))

                  features (when current-data
                             (lexi/extract-signature-features
                              current-data
                              api/CURRENT_SRATE))

                  ;; Load templates
                  up-template (lexi/load-category-template profile-name "pong/up")
                  down-template (lexi/load-category-template profile-name "pong/down")

                  up-confidence (if up-template
                                  (lexi/calculate-signature-similarity features up-template)
                                  0.0)
                  down-confidence (if down-template
                                    (lexi/calculate-signature-similarity features down-template)
                                    0.0)]

              {:up up-confidence :down down-confidence}))
          (catch Exception e
            (println "Error matching brain activity:" (.getMessage e))
            {:up 0.0 :down 0.0}))))))


(defn match-signal2 [current-signal patterns]
  (if (empty? patterns)
    0.0 ; No patterns to match against
    (let [similarity-scores (map (fn [pattern]
                                   (let [min-len (min (count current-signal) (count pattern))
                                         truncated-current (take min-len current-signal)
                                         truncated-pattern (take min-len pattern)
                                         squared-diffs (map #(Math/pow (- %1 %2) 2)
                                                            truncated-current
                                                            truncated-pattern)
                                         mse (/ (apply + squared-diffs) (max 1 (count squared-diffs)))
                                        ; Convert MSE to a similarity score (0-1)
                                         similarity (/ 1 (+ 1 (* 10 mse)))]
                                     similarity))
                                 patterns)
          ; Use the best match
          max-similarity (apply max (conj similarity-scores 0.0))]
      max-similarity)))

; Client functions for BCI integration
(e/defn connect-device! []
  (e/client
   (println "Client: Connecting to BCI device")
   #?(:cljs (let [result (connect-device-server)]
              (when (:success result)
                (swap! pong-state/state assoc-in [:bci :device-connected?] true)
                (swap! pong-state/state assoc-in [:bci :active-profile] (:profile-name result)))
              (:success result)))))

(e/defn disconnect-device! []
  (e/client
   (println "Client: Disconnecting from BCI device")
   ; Stop any active recording
   #?(:cljs (when (get-in @pong-state/state [:bci :recording?])
              (stop-recording-server)))

   ; Stop any match intervals
   #?(:cljs  (when-let [interval (get-in @pong-state/state [:bci :match-interval])]
               (when-let [interval (get-in @pong-state/state [:bci :match-interval])]
                 (js/clearInterval interval)
                 (swap! pong-state/state assoc-in [:bci :match-interval] nil))

   ; Disconnect on server
               (let [result (disconnect-device-server)]
                 #?(:cljs (when result
                            (swap! pong-state/state assoc-in [:bci :device-connected?] false)))
                 result)))))

(e/defn start-recording! [category]
  (e/client
   (println "Client: Starting recording for category:" category)
   #?(:cljs (let [session-id (start-recording-server category)]
              (when session-id
                (swap! pong-state/state assoc-in [:bci :recording?] true)
                (swap! pong-state/state assoc-in [:bci :current-category] category)
                (swap! pong-state/state assoc-in [:bci :session-id] session-id))))
   nil))

(e/defn stop-recording! []
  (e/client
   (println "Client: Stopping recording")
   #?(:cljs (let [result (stop-recording-server)]
              (swap! pong-state/state assoc-in [:bci :recording?] false)
              (swap! pong-state/state assoc-in [:bci :current-category] nil)
              (swap! pong-state/state assoc-in [:bci :session-id] nil)
              result))))

(e/defn start-activity-matching! []
  (e/client
   (println "Client: Starting brain activity matching")
   #?(:cljs (let [interval (js/setInterval
                            (fn []
                              (let [confidence-data (match-brain-activity-server)]
                                (swap! pong-state/state assoc-in [:game :bci-confidence] confidence-data)))
                            100)]
              (swap! pong-state/state assoc-in [:bci :match-interval] interval)))
   true))

(e/defn stop-activity-matching! []
  (e/client
   (println "Client: Stopping brain activity matching")
   #?(:cljs (when-let [interval (get-in @pong-state/state [:bci :match-interval])]
              (js/clearInterval interval)
              (swap! pong-state/state assoc-in [:bci :match-interval] nil)))
   true))

