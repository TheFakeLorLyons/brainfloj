(ns brain-pong.main
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [brain-pong.game-state :as pong-state]
            [brain-pong.components :as component]
            #?(:clj [floj.brainflow.board-ids :as bids])))

(def client-state (atom nil))

(e/defn GameLoop []
  (e/client
    (let [state (e/watch pong-state/state)
          playing? (get-in state [:game :playing?])
          frame-id-atom (atom nil)]
      ; Cleanup any existing animation frame on each re-render
      (when @frame-id-atom
        (js/console.log "Cleaning up previous game loop")
        (js/cancelAnimationFrame @frame-id-atom)
        (reset! frame-id-atom nil))
      
      ; Start a new game loop if we're playing
      (when playing?
        (js/console.log "Starting new game loop")
        (letfn [(game-loop []
                  ; Process keyboard input from keys-pressed set
                  (let [keys-pressed (get @pong-state/state :keys-pressed #{})]
                    (when (contains? keys-pressed "ArrowUp")
                      (pong-state/move-paddle! :up))
                    (when (contains? keys-pressed "ArrowDown")
                      (pong-state/move-paddle! :down)))

                  ; Run the game logic tick
                  (pong-state/game-loop-tick!)

                  ; Continue the loop if still playing
                  (when (get-in @pong-state/state [:game :playing?])
                    (reset! frame-id-atom (js/requestAnimationFrame game-loop))))]

          ; Start the game loop immediately
          (reset! frame-id-atom (js/requestAnimationFrame game-loop))))

      ; Cleanup on unmount
      (e/on-unmount #(when @frame-id-atom
                       (js/console.log "Component unmounting, cleaning up game loop")
                       (js/cancelAnimationFrame @frame-id-atom)
                       (reset! frame-id-atom nil)))

      ; Return nil for the component (it doesn't render anything)
      nil)))

(e/defn Main [ring-request]
  (e/client
    #_(init-game-state!)
    (binding [dom/node js/document.body] ; Don't forget to bind and use a wrapper div
      (dom/div ; Mandatory wrapper div https://github.com/hyperfiddle/electric/issues/74
        (let [server? (dom/label (dom/text "Toggle client/server:")
                        (dom/input (dom/props {:type "checkbox", :checked true})
                          (dom/On "change" (fn [js-event] (-> js-event .-target .-checked)) true)))]
          (dom/pre (dom/text (if server?
                               (e/server (str "`1` on server is `" (class 1) "`"))
                               (e/client (str "`1` on client is `" (goog/typeOf 1) "`"))))))
        (dom/hr)
        (dom/p (dom/text "Source code is in ") (dom/code (dom/text "src/brain_pong/main.cljc")))
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
                  ; Game Loop
            (GameLoop)

            (dom/div
             (dom/props {:class "bci-area"})
             (component/BCIPanel))

            #_(component/StateDebugging)
            (component/DebugPanel)))))))

(defn electric-boot [ring-request]
  #?(:clj  (e/boot-server {} Main (e/server ring-request))  ; inject server-only ring-request ;copy this verbatim nil on client not on server
     :cljs (e/boot-client {} Main (e/server (e/amb)))))     ; symmetric – same arity – no-value hole in place of server-only ring-request