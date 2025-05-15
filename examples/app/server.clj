(ns app.server
  (:require
   [clojure.java.io :as io]
   [clojure.core.async :as async]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [contrib.assert :refer [check]]
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.electric-ring-adapter :as adapter]
   [org.httpkit.server :as http]
   [ring.middleware.content-type :refer [wrap-content-type]]
   [ring.middleware.cookies :as cookies]
   [ring.middleware.resource :refer [wrap-resource]]
   [ring.middleware.params :refer [wrap-params]]
   [ring.util.response :as res]
   [user :as user]))

(defn wrap-static [handler]
  (wrap-resource handler "public"))  ; Serving files from the "public" directory

(e/defn ServerApp []
  (e/server
   (println "ServerApp running")
   (e/client
    (dom/div
     (dom/text "Electric Pong")))))

(defonce !ws-clients (atom {}))
(defonce !eeg-broadcast-ch (async/chan (async/sliding-buffer 10)))

(defn get-modules 
  "Reads the shadow-cljs manifest file and returns a map of module names to file paths"
  [manifest-path]
  (when-let [manifest (io/resource manifest-path)]
    (println "Found manifest at:" manifest-path)
    (try
      (let [manifest-content (slurp manifest)
            modules-info (edn/read-string manifest-content)]
        (->> modules-info
             (reduce (fn [r module]
                       (assoc r
                              (keyword "hyperfiddle.client.module" (name (:name module)))
                              (str "/example/js/" (:output-name module))))
                     {})))
      (catch Exception e
        (println "Error reading manifest:" (.getMessage e))
        {}))))

(defn template
  "In string template `<div>$:foo/bar$</div>`, replace all instances of $key$
with target specified by map `m`. Target values are coerced to string with `str`.
  E.g. (template \"<div>$:foo$</div>\" {:foo 1}) => \"<div>1</div>\" - 1 is coerced to string."
  [t m] (reduce-kv (fn [acc k v] (str/replace acc (str "$" k "$") (str v))) t m))

(defn electric-ws-handler [req electric-handler]
  (if-not (:websocket? req)
    {:status 400 :body "Expected WebSocket request"}
    (http/as-channel req
                     {:on-open
                      (fn [ch]
                        (println "Electric WebSocket connected")
                        (let [handler (-> electric-handler
                                          (adapter/wrap-reject-stale-client {:hyperfiddle.electric/user-version user/USER_VERSION})
                                          (cookies/wrap-cookies)
                                          (wrap-params))]
                          (adapter/wrap-electric-websocket handler req)))
                      :on-close
                      (fn [ch status]
                        (println "Electric WebSocket closed, status:" status))
                      :on-receive
                      (fn [ch msg]
                        (println "Unexpected direct message on Electric WS:" msg))})))

(defn eeg-ws-handler
  [req]
  (if-not (:websocket? req)
    {:status 400 :body "Expected WebSocket request"}
    (http/as-channel req
                     {:on-open
                      (fn [ch]
                        (let [client-id (str (java.util.UUID/randomUUID))]
                          (println "EEG WebSocket client connected:" client-id)
                          (swap! !ws-clients assoc client-id {:ws ch
                                                              :connected-at (System/currentTimeMillis)})
                          (.setAttachment ch client-id)))
                      :on-close
                      (fn [ch status]
                        (let [client-id (.getAttachment ch)]
                          (println "EEG WebSocket closed:" client-id "status:" status)
                          (swap! !ws-clients dissoc client-id)))
                      :on-receive
                      (fn [ch data]
                        (println "Received EEG WebSocket message" data)
                        (let [msg (try
                                    (edn/read-string data)
                                    (catch Exception _ data))]
                          (println "Parsed EEG WS message:" msg)))})))

(defn routes [req electric-handler config]
  (println "Handling request:" (:uri req) "method:" (:request-method req))
  (let [headers (:headers req)
        _ (println "headers in route : " headers)]
    (cond
      (and (= (:uri req) "/electric-ws")
           (= (get-in req [:headers "upgrade"]) "websocket"))
      (do
        (println "hey from routes" req)
        (electric-ws-handler req electric-handler))

        (and (= (:uri req) "/ws")
             (= (get-in req [:headers "upgrade"]) "websocket"))
        (do
          (println "Handling EEG WebSocket connection")
          (eeg-ws-handler req))

        (or (= (:uri req) "/")
            (= (:uri req) "/index.html"))
        (do
          (println "Serving index.html with template replacements")
          (if-let [resource-path (str (:resources-path config) "/example/index.html")]
            (if-let [resource (io/resource resource-path)]
              (do
                (println "Found index.html resource at:" resource-path)
                (let [modules (or (get-modules (:manifest-path config)) {})
                      content (-> (slurp resource)
                                  (template (merge config
                                                   modules
                                                   {:ELECTRIC_USER_VERSION user/USER_VERSION})))]
                  (-> (res/response content)
                      (res/content-type "text/html")
                      (res/header "Cache-Control" "no-store"))))
              (do
                (println "Index.html resource not found at:" resource-path)
                (-> (res/not-found (str "Index page not found at: " resource-path))
                    (res/content-type "text/plain"))))
            (do
              (println "Index path construction failed")
              (-> (res/not-found "Could not determine index path")
                  (res/content-type "text/plain")))))
        :else
        (let [static-handler
              (-> (constantly {:status 404 :body "Not found"})
                  (wrap-resource (:resources-path config))
                  (wrap-content-type))]
          (static-handler req)))))

(defn start-server! [electric-handler {:keys [host port resources-path manifest-path] :as config}]
  (println "Starting server with config:" config)
  (println "SERVER USER VERSION:" user/USER_VERSION)

  (when-let [resource-dir (io/resource resources-path)]
    (println "Resources directory found at:" resource-dir))

  (get-modules manifest-path)

  (let [handler (fn [req]
                      (routes req electric-handler config))
        server (http/run-server
                handler
                {:port port
                 :host host
                 :manifest-path manifest-path
                 :join? false})]
    (println "WebSocket server started on" (str "ws://" host ":" port "/electric-ws"))
    (println "Started Electric HTTP server on" (str "http://" host ":" port))
    server))

(comment
  #_(def stop-http-server (jetty/start-server! handler config))
  (start-electric-server!)
  (stop-http-server)
  (async/put! !eeg-broadcast-ch (pr-str {:type "eeg-data"
                                         :data [1.0 2.0 3.0 4.0]
                                         :timestamp (System/currentTimeMillis)})))
