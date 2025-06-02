(ns brain-pong.main
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [brain-pong.bci-integration :as bci]
            [brain-pong.game-state :as pong-state]
            [brain-pong.components :as component]
            #?(:clj [brain-pong.signature :as signature])
            #?(:clj [floj.brainflow.board-ids :as bids])))

(def client-state (atom nil))

(e/defn GameLoop []
  (e/client
   (let [state (e/watch pong-state/state)
         playing? (get-in state [:game :playing?])
         matching? (get-in state [:bci :matching?])
         connected? (get-in state [:bci :device-connected?])
         streaming? (get-in state [:bci :streaming?])
         frame-id-atom (atom nil)]

     (when @frame-id-atom
       (js/cancelAnimationFrame @frame-id-atom)
       (reset! frame-id-atom nil))

     (when (and playing? connected? (not matching?))
       (js/console.log "Starting BCI brain activity matching")
       (swap! pong-state/state assoc-in [:bci :matching?] true))

     ;; Stop BCI control when game stops or device disconnected
     (when (and matching? (or (not playing?) (not connected?)))
       (js/console.log "Stopping BCI brain activity matching")
       (swap! pong-state/state assoc-in [:bci :matching?] false))

     (when playing?
       
       (js/console.log "Starting new game loop")
       (letfn [(game-loop []
                          (let [keys-pressed (get @pong-state/state :keys-pressed #{})]
                            (when (contains? keys-pressed "ArrowUp")
                              (pong-state/move-paddle! :up))
                            (when (contains? keys-pressed "ArrowDown")
                              (pong-state/move-paddle! :down)))

                          (if (and connected? matching?)
                            (bci/process-bci-input!)
                            (pong-state/game-loop-tick!))

                          (when (get-in @pong-state/state [:game :playing?])
                            (reset! frame-id-atom (js/requestAnimationFrame game-loop))))]

         (reset! frame-id-atom (js/requestAnimationFrame game-loop))))

     (e/on-unmount
      #(do
         (when @frame-id-atom
           (js/cancelAnimationFrame @frame-id-atom)
           (reset! frame-id-atom nil))

         (when matching?
           #_(bci/stop-activity-matching!)
           #_(bci/stop-eeg-streaming!))))

     nil)))

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

       ;; Page header
       (dom/div
        (dom/props {:class "header"})
        (dom/h1 (dom/text "Electric Pong")))

       ;; Game container
       (dom/div
        (dom/props {:class "pong-container"})
        (component/Instructions)
        (component/PongCourt)
        (GameLoop)
      
        (dom/div
         (dom/props {:class "bci-area"})
         (component/BCIPanel)

         (PeriodicStatusChecker)))
       #_(component/StateDebugging)
     
       (component/DebugPanel))))))

(defn electric-boot [ring-request]
  #?(:clj  (e/boot-server {} Main (e/server ring-request))  ; inject server-only ring-request ;copy this verbatim nil on client not on server
     :cljs (e/boot-client {} Main (e/server (e/amb)))))     ; symmetric – same arity – no-value hole in place of server-only ring-request