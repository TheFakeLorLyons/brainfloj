(ns pong.paddle-calibration
  (:require
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   #?(:cljs [clojure.edn :as edn])
   #?(:cljs [clojure.string :as str])
   #?(:cljs [pong.game-state :as game-state])
   #?(:cljs [pong.bci-integration :as bci])))

(e/def !calibration-state (e/client
                           #?(:cljs
                              (atom {:active false
                                     :phase :setup  ; :setup, :record-up, :record-down, :processing, :complete
                                     :progress {:up 0 :down 0}
                                     :target-samples 10
                                     :samples-collected {:up 0 :down 0}
                                     :sample-quality {:up {:avg 0 :scores []} :down {:avg 0 :scores []}}
                                     :calibration-history []
                                     :current-session nil
                                     :using-game-data false
                                     :available-game-sessions []
                                     :selected-sessions []
                                     :status-message ""
                                     :last-calibration-date nil}))))

(e/defn timestamp->readable-date [timestamp]
  (e/client
   #?(:cljs (-> (js/Date. timestamp)
                .toLocaleString))))

#_(e/defn load-game-sessions! [profile-name]
  (e/client
   #?(:cljs
      (when (fn? bci/load-game-sessions)
        (let [sessions (bci/load-game-sessions profile-name)]
          (when (seq sessions)
            (swap! !calibration-state assoc :available-game-sessions sessions)))))))

(e/defn select-game-session! [session-id select?]
  (e/client
   #?(:cljs
      (swap! !calibration-state update :selected-sessions
             (if select?
               #(conj % session-id)
               #(filterv (fn [id] (not= id session-id)) %))))))

(e/defn extract-wave-signatures-from-games! []
  (e/client
   #?(:cljs
      (do
        (swap! !calibration-state assoc :status-message "Processing game data...")
        (let [selected-sessions (:selected-sessions @!calibration-state)
              available-sessions (:available-game-sessions @!calibration-state)
              profile-name (bci/get-active-profile)]

          (when (seq selected-sessions)
            (swap! !calibration-state assoc :phase :processing)

            (bci/process-game-sessions profile-name selected-sessions)

            (swap! !calibration-state assoc
                   :phase :complete
                   :last-calibration-date (js/Date.now)
                   :status-message "Calibration from game data complete!")

            (count selected-sessions)))))))

(e/defn start-calibration! []
  (e/client
   #?(:cljs
      (swap! !calibration-state assoc
             :active true
             :phase :setup
             :status-message "Ready to start calibration"))))

(e/defn stop-calibration! []
  (e/client
   #?(:cljs
      (do
        (when (= (:phase @!calibration-state) :record-up)
          (bci/stop-recording!))
        (when (= (:phase @!calibration-state) :record-down)
          (bci/stop-recording!))
        (swap! !calibration-state assoc
               :active false
               :phase :setup
               :status-message "Calibration cancelled")))))

(e/defn start-recording-direction! [direction]
  (e/client
   #?(:cljs
      (let [category (case direction
                       :up "pong-up"
                       :down "pong-down")]

        (swap! !calibration-state assoc
               :phase (if (= direction :up) :record-up :record-down)
               :status-message (str "Recording " (name direction) " movements..."))

        (let [session-id (bci/start-recording! category)]
          (swap! !calibration-state assoc :current-session session-id))))))

(e/defn stop-recording-direction! [direction]
  (e/client
   #?(:cljs
      (let [result (bci/stop-recording!)]
        (when (= (:status result) "success")
          (swap! !calibration-state update-in [:samples-collected direction] inc)

          (let [samples-collected (get-in @!calibration-state [:samples-collected direction])
                target-samples (:target-samples @!calibration-state)]

            (if (>= samples-collected target-samples)
              ; Finished with this direction
              (if (= direction :up)
                ; Switch to down direction if we finished up
                (swap! !calibration-state assoc
                       :phase :record-down
                       :status-message "Now let's record downward movements")
                ; Otherwise we're done with both directions
                (swap! !calibration-state assoc
                       :phase :complete
                       :last-calibration-date (js/Date.now)
                       :status-message "Calibration complete!"))
              ; Still need more samples for this direction
              (swap! !calibration-state assoc
                     :status-message (str "Recorded " samples-collected
                                          " of " target-samples " "
                                          (name direction) " movements")))))))))

; UI Components - Converted from Reagent to Electric

(e/defn ProgressBar [value]
  (e/client
   (dom/div (dom/props {:class "progress-bar"})
            (dom/div (dom/props {:class "progress-fill"
                                 :style {:width (str (* 100 value) "%")}})))))

(e/defn CalibrationSetupPanel []
  (e/client
   #?(:cljs
      (let [{:keys [target-samples available-game-sessions selected-sessions]} @!calibration-state]
        (dom/div (dom/props {:class "calibration-setup"})
                 (dom/h3 (dom/text "BCI Calibration Setup"))

                 (dom/div (dom/props {:class "calibration-options"})
                          (dom/div (dom/props {:class "manual-calibration"})
                                   (dom/h4 (dom/text "Manual Calibration"))
                                   (dom/div (dom/props {:class "form-group"})
                                            (dom/label (dom/text "Number of samples for each direction:"))
                                            (dom/input (dom/props {:type "number"
                                                                   :min 5
                                                                   :max 50
                                                                   :value target-samples})
                                                       (dom/on "input" (e/fn [e]
                                                                         (swap! !calibration-state assoc
                                                                                :target-samples (js/parseInt (.. e -target -value)))))))

                                   (dom/button (dom/props {:class "calibration-button"})
                                               (dom/on "click" (e/fn [_] (start-recording-direction! :up)))
                                               (dom/text "Begin Manual Calibration")))

                          (dom/div (dom/props {:class "game-data-calibration"})
                                   (dom/h4 (dom/text "Use Previous Game Data"))

                                   (if (empty? available-game-sessions)
                                     (dom/div (dom/text "No previous game data available"))
                                     (dom/div
                                      (dom/p (dom/text (str "Select from " (count available-game-sessions) " previous games:")))

                                      (dom/div (dom/props {:class "session-list"})
                                               (e/for [session available-game-sessions]
                                                 (dom/div (dom/props {:class "session-item"})
                                                          (dom/input (dom/props {:type "checkbox"
                                                                                 :checked (contains? (set selected-sessions) (:id session))})
                                                                     (dom/on "change" (e/fn [e]
                                                                                        (select-game-session!
                                                                                         (:id session)
                                                                                         (.. e -target -checked)))))
                                                          (dom/div (dom/props {:class "session-info"})
                                                                   (dom/div (dom/props {:class "session-date"})
                                                                            (dom/text (:readable-date session)))
                                                                   (dom/div (dom/props {:class "session-score"})
                                                                            (dom/text (str "Score: "
                                                                                           (get-in session [:score :left] 0)
                                                                                           " - "
                                                                                           (get-in session [:score :right] 0))))))))

                                      (dom/button (dom/props {:class "calibration-button"
                                                              :disabled (empty? selected-sessions)})
                                                  (dom/on "click" (e/fn [_] (extract-wave-signatures-from-games!)))
                                                  (dom/text "Calibrate From Game Data")))))))))))

(e/defn RecordingPanel [direction]
  (e/client
   #?(:cljs
      (let [{:keys [samples-collected target-samples]} @!calibration-state
            progress (/ (get samples-collected direction 0) target-samples)]
        (dom/div (dom/props {:class "recording-panel"})
                 (dom/h3 (dom/text (str "Recording " (name direction) " Movements")))

                 (dom/div (dom/props {:class "progress-container"})
                          (dom/p (dom/text (str (get samples-collected direction 0) " of " target-samples)))
                          (ProgressBar progress))

                 (dom/div (dom/props {:class "recording-instructions"})
                          (dom/p (dom/text (case direction
                                             :up "Press the UP arrow key or think about moving the paddle upward"
                                             :down "Press the DOWN arrow key or think about moving the paddle downward"
                                             "Follow the instructions"))))

                 (dom/div (dom/props {:class "recording-controls"})
                          (dom/button (dom/props {:class "recording-button"})
                                      (dom/on "click" (e/fn [_] (stop-recording-direction! direction)))
                                      (dom/text "Record Sample"))

                          (dom/button (dom/props {:class "cancel-button"})
                                      (dom/on "click" (e/fn [_] (stop-calibration!)))
                                      (dom/text "Cancel"))))))))

(e/defn ProcessingPanel []
  (e/client
   (dom/div (dom/props {:class "processing-panel"})
            (dom/h3 (dom/text "Processing Calibration Data"))
            (dom/div (dom/props {:class "spinner"}))
            (dom/p (dom/text "Please wait while your calibration data is processed...")))))

(e/defn CompletePanel []
  (e/client
   #?(:cljs
      (let [{:keys [last-calibration-date status-message]} @!calibration-state]
        (dom/div (dom/props {:class "complete-panel"})
                 (dom/h3 (dom/text "Calibration Complete"))

                 (dom/div (dom/props {:class "status-message"})
                          (dom/p (dom/text status-message)))

                 (when last-calibration-date
                   (dom/div (dom/props {:class "calibration-date"})
                            (dom/p (dom/text (str "Last calibrated: " (timestamp->readable-date last-calibration-date))))))

                 (dom/button (dom/props {:class "close-button"})
                             (dom/on "click" (e/fn [_] (swap! !calibration-state assoc :active false)))
                             (dom/text "Close")))))))

(e/defn CalibrationDialog []
  (e/client
   #?(:cljs
      (let [{:keys [active phase]} @!calibration-state]
        (when active
          (dom/div (dom/props {:class "calibration-dialog-overlay"})
                   (dom/div (dom/props {:class "calibration-dialog"})
                            (dom/h2 (dom/text "BCI Calibration"))

                            (case phase
                              :setup (CalibrationSetupPanel)
                              :record-up (RecordingPanel :up)
                              :record-down (RecordingPanel :down)
                              :processing (ProcessingPanel)
                              :complete (CompletePanel)
                              (dom/div (dom/text "Unknown phase"))))))))))

; Public API - for integration with the main app
(e/def pong-calibration-api
  (e/client
   {:start-calibration! (fn [] (start-calibration!))
    :stop-calibration! (fn [] (stop-calibration!))
    :is-calibration-active? #?(:cljs #(:active @!calibration-state))
    #?(:clj nil)
    #_#_:load-game-sessions! (fn [profile-name] (load-game-sessions! profile-name))
    #_#_:calibration-dialog CalibrationDialog}))