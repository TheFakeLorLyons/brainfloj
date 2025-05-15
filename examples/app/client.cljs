(ns app.client
  (:require
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   user
   [pong.electric :as pong-electric]))

; Root component that renders the Pong game
(e/defn App []
  (e/client
   (js/console.log "Client rendering with version:" user/USER_VERSION)
   (pong-electric/!Pong)))

; Reactor ref to manage lifecycle
(defonce reactor nil)

; Initialize the Electric client
(defn init []
  (js/console.log "Starting BCI Pong client initialization...")
  (js/console.log "CLIENT USER VERSION:" user/USER_VERSION)
  (let [container (.getElementById js/document "pong")]
    (if container
      (do
        (js/console.log "Found #pong container, initializing Electric client")
        (set! (.-innerHTML container) "Hi Pong...")
        (js/console.log "Setting up Electric event listeners")
        (js/window.addEventListener "electric-connect"
                                    (fn []
                                      (js/console.log "Electric connected successfully!")
                                      (set! (.-innerHTML container) "")))
        (js/window.addEventListener "electric-disconnect"
                                    (fn []
                                      (js/console.log "Electric disconnected!")
                                      (set! (.-innerHTML container)
                                            "BCI Pong disconnected - reconnecting...")))
        (js/console.log "Starting Electric reactor")
        (try
          (set! reactor
                (e/boot-client
                 {:url (str "ws://" js/location.hostname ":"
                            (or js/location.port "8080") "/electric-ws")
                  :hyperfiddle.electric/user-version user/USER_VERSION}
                 
                 ; on-success callback
                 (fn [_]
                   (js/console.log "✅ Electric reactor booted successfully"))
                 ; on-error callback
                 (fn [err]
                   (js/console.error "❌ Electric reactor failed to boot" err)
                   (set! (.-innerHTML container)
                         (str "Error initializing BCI Pong: " (.-message err))))))
          (js/console.log "Electric client started")
          (catch js/Error e
            (js/console.error "Error starting Electric:", e)
            (set! (.-innerHTML container)
                  (str "Error initializing BCI Pong: " (.-message e))))))
      (do
        (js/console.error "Container #pong not found in DOM")
        (js/alert "Error: Element with ID 'pong' not found. Cannot initialize Pong game.")))))

(defn ^:dev/before-load stop! []
  (js/console.log "Stopping Electric reactor")
  (when reactor (reactor))
  (set! reactor nil))

(defn ^:export start! []
  (js/console.log "Starting Electric from main entry point")
  (init))