(ns brain-pong.main
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [brain-pong.bci-integration :as bci]
            [brain-pong.game-state :as pong-state]
            [brain-pong.components :as component]
            #?(:clj [brain-pong.signature :as signature])
            #?(:clj [floj.brainflow.board-ids :as bids])))

(def client-state (atom nil))

; Synchronous game loop
(e/defn GameLoop []
  (let [state (e/watch pong-state/state)
        profile-name (or (get-in state [:bci :user-profile :name]) "default")]
    (e/client
     (let [playing? (get-in state [:game :playing?])
           frame-id-atom (atom nil)
           last-frame-time (atom 0)]

       (when @frame-id-atom
         (js/cancelAnimationFrame @frame-id-atom)
         (reset! frame-id-atom nil))

       (when playing?
         (letfn [(enhanced-game-loop [current-time]
                   (let [time-since-last (- current-time @last-frame-time)
                         target-fps 60
                         target-frame-time (/ 1000 target-fps)]
                     (when (>= time-since-last target-frame-time)
                       (reset! last-frame-time current-time)

                       (let [current-state @pong-state/state
                             matching? (get-in current-state [:bci :matching?])
                             connected? (get-in current-state [:bci :device-connected?])
                             streaming? (get-in current-state [:bci :streaming?])
                             training-assistance (get-in current-state [:bci :training-assistance])]

                         ; Handle keyboard input
                         (let [keys-pressed (get current-state :keys-pressed #{})]
                           (when (contains? keys-pressed "ArrowUp")
                             (pong-state/move-paddle! :up))
                           (when (contains? keys-pressed "ArrowDown")
                             (pong-state/move-paddle! :down)))

                         ; BCI processing including training wheels
                         (when (and connected? matching? streaming?)

                           ; Check for training assistance first
                           (if (and training-assistance
                                    (> (:assistance-level training-assistance) 0.2))
                             (do
                               (js/console.log "Applying training assistance:"
                                               (:direction training-assistance)
                                               "at level" (:assistance-level training-assistance))

                               ; Apply training wheels assistance
                               (case (:direction training-assistance)
                                 :up (do
                                       (pong-state/move-paddle! :up)

                                       ; Track that this was an assisted action
                                       (let [confidence (get-in current-state [:bci :confidence] {:up 0 :down 0})]
                                         (swap! pong-state/state update-in [:bci :action-history]
                                                (fn [history]
                                                  (take-last 50
                                                             (conj (or history [])
                                                                   {:timestamp (js/Date.now)
                                                                    :action :up
                                                                    :successful true
                                                                    :assisted true
                                                                    :confidence confidence}))))))
                                 :down (do
                                         (pong-state/move-paddle! :down)
                                         ; Track that this was an assisted action
                                         (let [confidence (get-in current-state [:bci :confidence] {:up 0 :down 0})]
                                           (swap! pong-state/state update-in [:bci :action-history]
                                                  (fn [history]
                                                    (take-last 50
                                                               (conj (or history [])
                                                                     {:timestamp (js/Date.now)
                                                                      :action :down
                                                                      :successful true
                                                                      :assisted true
                                                                      :confidence confidence}))))))
                                 nil)
                               ; Clear assistance after use
                               (swap! pong-state/state assoc-in [:bci :training-assistance] nil))
                             ; Normal BCI processing - call your existing function
                             (bci/process-bci-input)))

                         ; Regular game tick
                         (pong-state/brain-game-loop-tick!)))

                     (when (get-in @pong-state/state [:game :playing?])
                       (reset! frame-id-atom (js/requestAnimationFrame enhanced-game-loop)))))]

           (reset! frame-id-atom (js/requestAnimationFrame enhanced-game-loop))))

       ; Cleanup
       (e/on-unmount
        #(when @frame-id-atom
           (js/cancelAnimationFrame @frame-id-atom)
           (reset! frame-id-atom nil)))
       nil))))

; ...Asynchronous game loop
(e/defn AsyncGameLoop []
  (let [state (e/watch pong-state/state)
        profile-name (or (get-in state [:bci :user-profile :name]) "default")]
    (e/client
     (let [playing? (get-in state [:game :playing?])
           frame-id-atom (atom nil)
           last-frame-time (atom 0)
           last-bci-debug (atom 0)]

       (when @frame-id-atom
         (js/cancelAnimationFrame @frame-id-atom)
         (reset! frame-id-atom nil))

       (when playing?
         (letfn [(enhanced-game-loop [current-time]
                   (let [time-since-last (- current-time @last-frame-time)
                         target-fps 60
                         target-frame-time (/ 1000 target-fps)]
                     (when (>= time-since-last target-frame-time)
                       (reset! last-frame-time current-time)

                       ; Get fresh state for each frame
                       (let [fresh-state @pong-state/state
                             matching? (get-in fresh-state [:bci :matching?])
                             connected? (get-in fresh-state [:bci :device-connected?])
                             streaming? (get-in fresh-state [:bci :streaming?])]

                         ; Handle keyboard input
                         (let [keys-pressed (get fresh-state :keys-pressed #{})]
                           (when (contains? keys-pressed "ArrowUp")
                             (pong-state/move-paddle! :up))
                           (when (contains? keys-pressed "ArrowDown")
                             (pong-state/move-paddle! :down)))

                         ; Handle BCI input using processing-function
                         (when (and connected? matching? streaming?)
                           (bci/process-async-bci-input!))

                         (pong-state/brain-game-loop-tick!)))

                     (when (get-in @pong-state/state [:game :playing?])
                       (reset! frame-id-atom (js/requestAnimationFrame enhanced-game-loop)))))]
           (reset! frame-id-atom (js/requestAnimationFrame enhanced-game-loop))))

       (e/on-unmount
        #(when @frame-id-atom
           (js/cancelAnimationFrame @frame-id-atom)
           (reset! frame-id-atom nil)))
       nil))))

(e/defn PeriodicStatusChecker []
  (e/client
   (let [timer-atom (atom 0)
         interval-id (atom nil)]

     ; Start the timer if not already running
     (when-not @interval-id
       (js/console.log "Starting periodic BCI status checker with Electric timer")
       (reset! interval-id
               (js/setInterval
                (fn []
                  ; Increments timer, triggering Electric reactivity
                  (swap! timer-atom inc))
                5000)))

     ; Register cleanup
     (e/on-unmount
      (fn [_]
        (when @interval-id
          (js/clearInterval @interval-id)
          (reset! interval-id nil))))

     ; Watch the timer atom - this makes the component reactive to timer changes
     (let [timer-val (e/watch timer-atom)
           state (e/watch pong-state/state)
           pending-connect? (get-in state [:bci :pending-connect])
           pending-disconnect? (get-in state [:bci :pending-disconnect])]

       ; Only check status when timer changes and we're not in pending state
       (when (and (> timer-val 0)  ; Skip the initial render
                  (not pending-connect?)
                  (not pending-disconnect?))
         (js/console.log "Running periodic status check, timer:" timer-val)

         ; Now possible to call the Electric server function
         (let [status (e/server (bci/get-device-status-server))]
           (js/console.log "Periodic status result:" (clj->js status))
           (when (:connected status)
             (let [current-state (get-in @pong-state/state [:bci])
                   new-connected? (:connected status)
                   new-profile (:profile-name status)]
               (when (or (not= (:device-connected? current-state) new-connected?)
                         (not= (:active-profile current-state) new-profile))
                 (js/console.log "Updating status - Connected:" new-connected? "Profile:" new-profile)
                 (swap! pong-state/state update-in [:bci] merge
                        {:device-connected? new-connected?
                         :active-profile (str new-profile)})))))))
     nil)))

(e/defn Main [ring-request]
  (e/client
   (when-not @client-state
     (pong-state/init-game-state!)
     (pong-state/init-keyboard-controls!)
     (reset! client-state true)
     (js/console.log "Game state initialized successfully"))

   (binding [dom/node js/document.body] ;; Don't forget to bind and use a wrapper div
     (dom/div ;; Mandatory wrapper div https://github.com/hyperfiddle/electric/issues/74
      (let [server? (dom/label (dom/text "Toggle client/server:")
                               (dom/input (dom/props {:type "checkbox", :checked true})
                                          (dom/On "change" (fn [js-event] (-> js-event .-target .-checked)) true)))]
        (dom/pre (dom/text (if server?
                             (e/server (str "`1` on server is `" (class 1) "`"))
                             (e/client (str "`1` on client is `" (goog/typeOf 1) "`"))))))
      (dom/hr)
      (dom/p (dom/text "Source code is in ") (dom/code (dom/text "src/brain_pong/main.cljc")))
      (dom/p (dom/text "Check out ") (dom/a (dom/text "Electric examples")
                                            (dom/props {:href "https://electric.hyperfiddle.net" :target "_blank"})))

      (dom/div
       (dom/props {:class "main-container"})

       ; Page header
       (dom/div
        (dom/props {:class "header"})
        (dom/h1 (dom/text "Electric Pong")))

       ; Game container
       (dom/div
        (dom/props {:class "pong-container"})
        (component/Instructions)
        (component/PongCourt)
        (AsyncGameLoop)

        (dom/div
         (dom/props {:class "bci-area"})
         (component/BCIPanel)

         (PeriodicStatusChecker)))

       (component/DebugPanel))))))

(defn electric-boot [ring-request]
  #?(:clj  (e/boot-server {} Main (e/server ring-request))  ; inject server-only ring-request ;copy this verbatim nil on client not on server
     :cljs (e/boot-client {} Main (e/server (e/amb)))))     ; symmetric – same arity – no-value hole in place of server-only ring-request