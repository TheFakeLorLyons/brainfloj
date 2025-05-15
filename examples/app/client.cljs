(ns app.client
  (:require
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   user
   [pong.components :as pongcoment]
   [pong.game-state :as pong-state]))

; Root component that renders the Pong game
(e/defn App []
  (e/client
   (js/console.log "Client rendering with version:" user/USER_VERSION)
   ; Create a wrapper div to ensure proper rendering context
   (dom/div
    (dom/text "Hello!!!")
    (dom/props {:id "pong-root" :class "electric-pong-container"})
    (pongcoment/!Pong))))

; Reactor ref to manage lifecycle
(defonce reactor nil)

(defn init-game-state! []
  (js/console.log "Initializing game state...")
  (try
    (pong-state/init-game!)
    (js/console.log "Game state initialized successfully")
    (catch js/Error e
      (js/console.error "Error initializing game state:" e))))

(defn monitor-websocket-connection [url]
  (js/console.log "🔍 Manually testing WebSocket connection to:" url)
  (let [test-ws (new js/WebSocket url)]
    (set! (.-onopen test-ws)
          (fn [event]
            (js/console.log "✅ TEST WebSocket CONNECTED SUCCESSFULLY:" event)))

    (set! (.-onclose test-ws)
          (fn [event]
            (js/console.warn "❌ TEST WebSocket CLOSED:"
                             (if (.-wasClean event) "cleanly" "unexpectedly")
                             "Code:" (.-code event)
                             "Reason:" (.-reason event))))

    (set! (.-onerror test-ws)
          (fn [error]
            (js/console.error "❌ TEST WebSocket ERROR:" error)))

    test-ws))

; Initialize the Electric client
(defn init []
  (js/console.log "Starting BCI Pong client initialization...")
  (init-game-state!)

  (let [pong-element (.getElementById js/document "pong")
        pong-root-element (.getElementById js/document "pong-root")
        container (or pong-element
                      pong-root-element
                      (let [c (js/document.createElement "div")]
                        (set! (.-id c) "pong")
                        (js/document.body.appendChild c)
                        c))]

    (when container
      (js/console.log "Found #pong container, initializing Electric client")
      (try
        (let [ws-url (str "ws://" js/location.hostname ":" js/location.port "/electric-ws")]
          (js/console.log "🔌 Will attempt connection to:" ws-url)
          (monitor-websocket-connection ws-url)
          (js/setTimeout
           (fn []
             (js/console.log "⚡ Now attempting Electric client boot...")
             (try
               (set! reactor
                     (e/boot-client
                      {:url ws-url
                       :hyperfiddle.electric/user-version user/USER_VERSION}

                      ; on-success callback
                      (js/console.log "✅ Electric reactor booted successfully")

                      ; on-error callback
                      #(do
                         (js/console.error "❌ Electric reactor failed to boot:" %)
                         (set! (.-innerHTML container)
                               (str "<div class='error-message'>Failed to connect to Electric WebSocket: "
                                    (or (.-message %) "Unknown error") "</div>")))))
               (catch js/Error e
                 (js/console.error "❌ Error during Electric boot:" e)
                 (js/console.error "Stack trace:" (.-stack e))
                 (set! (.-innerHTML container)
                       (str "<div class='error-message'>Electric boot error: "
                            (.-message e) "</div>")))))
           500))
        (catch js/Error e
          (js/console.error "❌ Error in initialization:" e)
          (js/console.error "Stack trace:" (.-stack e))
          (set! (.-innerHTML container)
                (str "<div class='error-message'>Error initializing BCI Pong: "
                     (.-message e) "</div>")))))))

(defn ^:dev/before-load stop! []
  (js/console.log "Stopping Electric reactor")
  (when reactor (reactor))
  (set! reactor nil))

(defn ^:export start! []
  (js/console.log "Starting Electric from main entry point")
  (init))