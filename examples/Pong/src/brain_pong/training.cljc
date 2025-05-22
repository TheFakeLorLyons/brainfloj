(ns brain-pong.training
   (:require [hyperfiddle.electric3 :as e]
             [hyperfiddle.electric-dom3 :as dom]
             [brain-pong.game-state :as pong-state]
             [brain-pong.bci-integration :as bci]))

;; Training state management
 (def training-state (atom {:active? false
                            :current-step :start
                            :step-timer nil
                            :countdown 5
                            :current-direction nil
                            :completed-directions {}
                            :recordings-complete false}))

 (defn reset-training-state! []
   (swap! training-state assoc
          :active? false
          :current-step :start
          :step-timer nil
          :countdown 5
          :current-direction nil
          :completed-directions {}
          :recordings-complete false))

 (e/defn TrainingVisualizer []
   (e/client
    (let [state (e/watch training-state)
          current-direction (:current-direction state)
          countdown (:countdown state)]
      (dom/div
       (dom/props {:class "training-visualizer"})

        ;; Visual cue for paddle direction
       (dom/div
        (dom/props {:class (str "direction-indicator "
                                (case current-direction
                                  :up "direction-up"
                                  :down "direction-down"
                                  ""))})
        (dom/div
         (dom/props {:class "paddle-visualization"})
         (dom/div
          (dom/props {:class "virtual-paddle"})
          (dom/text "Paddle"))

            ;; Arrow indicator
         (when current-direction
           (dom/div
            (dom/props {:class "arrow"})
            (dom/text (case current-direction
                        :up "↑"
                        :down "↓"
                        ""))))

            ;; Countdown display
         (when (and current-direction countdown)
           (dom/div
            (dom/props {:class "countdown"})
            (dom/text countdown))))))))))

(e/defn start-direction-training! [direction]
  (e/client
   (swap! training-state assoc
          :current-direction direction
          :countdown 5)
   (let [timer-id (atom nil)]
     (reset! timer-id
             (js/setInterval
              (fn []
                (swap! training-state update :countdown dec)
                (when (<= (:countdown @training-state) 0)
                  (js/clearInterval @timer-id)
                  (e/run
                   (bci/stop-recording!)
                   (swap! training-state update-in [:completed-directions] assoc direction true)
                   (let [completed (get-in @training-state [:completed-directions])]
                     (when (and (get completed :up) (get completed :down))
                       (swap! training-state assoc
                              :recordings-complete true
                              :current-direction nil
                              :current-step :complete))))))
              1000))
     (swap! training-state assoc :step-timer @timer-id)
     (e/run (bci/start-recording! (name direction))))))

(e/defn TrainingInstructions []
  (e/client
   (let [state (e/watch training-state)
         current-step (:current-step state)
         completed-directions (:completed-directions state)
         recordings-complete (:recordings-complete state)]
     (dom/div
      (dom/props {:class "training-instructions"})
      (case current-step
        :start
        (dom/div
         (dom/h3 (dom/text "Brain-Computer Interface Training"))
         (dom/p (dom/text "Welcome to BCI training mode. In this mode, you'll train the system to recognize your brain patterns when imagining paddle movements."))
         (dom/p (dom/text "The training consists of two 5-second recording sessions:"))
         (dom/ul
          (dom/li (dom/text "Imagine moving the paddle UP"))
          (dom/li (dom/text "Imagine moving the paddle DOWN")))
         (dom/p (dom/text "Click 'Start Training' when ready."))
         (dom/button
          (dom/props {:class "training-button"})
          (dom/text "Start Training")
          (dom/on "click" (e/fn [_]
                            (swap! training-state assoc :current-step :record-up)))))

        :record-up
        (dom/div
         (dom/h3 (dom/text "Record 'UP' Movement"))
         (dom/p (dom/text "Imagine moving the paddle UP"))
         (dom/p (dom/text "The recording will begin when you click 'Start Recording'"))
         (dom/p (dom/text "Focus and maintain the mental state for the entire 5 seconds"))
         (if (get completed-directions :up)
           (dom/div
            (dom/p (dom/text "UP movement recorded!"))
            (dom/button
             (dom/props {:class "training-button"})
             (dom/text "Continue to DOWN movement")
             (dom/on "click" (e/fn [_]
                               (swap! training-state assoc :current-step :record-down)))))
           (dom/button
            (dom/props {:class "training-button"})
            (dom/text "Start Recording UP")
            (dom/on "click" (e/fn [_]
                              (start-direction-training! :up))))))

        :record-down
        (dom/div
         (dom/h3 (dom/text "Record 'DOWN' Movement"))
         (dom/p (dom/text "Imagine moving the paddle DOWN"))
         (dom/p (dom/text "The recording will begin when you click 'Start Recording'"))
         (dom/p (dom/text "Focus and maintain the mental state for the entire 5 seconds"))
         (if (get completed-directions :down)
           (dom/div
            (dom/p (dom/text "DOWN movement recorded!"))
            (dom/button
             (dom/props {:class "training-button"})
             (dom/text "Complete Training")
             (dom/on "click" (e/fn [_]
                               (swap! training-state assoc :current-step :complete)))))
           (dom/button
            (dom/props {:class "training-button"})
            (dom/text "Start Recording DOWN")
            (dom/on "click" (e/fn [_]
                              (start-direction-training! :down))))))

        :complete
        (dom/div
         (dom/h3 (dom/text "Training Complete!"))
         (dom/p (dom/text "Great job! The system has recorded your brain patterns for UP and DOWN movements."))
         (dom/p (dom/text "You can now return to the game or recalibrate again if needed."))
         (dom/div
          (dom/props {:class "button-row"})
          (dom/button
           (dom/props {:class "training-button"})
           (dom/text "Return to Main Menu")
           (dom/on "click" (e/fn [_]
                             (reset-training-state!)
                             (swap! pong-state/state assoc :mode :menu))))
          (dom/button
           (dom/props {:class "training-button"})
           (dom/text "Recalibrate")
           (dom/on "click" (e/fn [_]
                             (reset-training-state!)
                             (swap! training-state assoc :current-step :record-up)))))))))))

(e/defn TrainingMode []
  (e/client
   (let [state (e/watch training-state)]
     (dom/div
      (dom/props {:class "training-container"})
      (dom/h2 (dom/text "BCI Training Mode"))
      (TrainingInstructions)
      (TrainingVisualizer)))))