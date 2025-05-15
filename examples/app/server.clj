(ns app.server
  (:require
   [clojure.java.io :as io]
   [clojure.core.async :as async]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [contrib.assert :refer [check]]
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-ring-adapter :as adapter]
   [org.httpkit.server :as http]
   [pong.electric :refer [!Pong]]
   [ring.middleware.content-type :refer [wrap-content-type]]
   [ring.middleware.cookies :as cookies]
   [ring.middleware.resource :refer [wrap-resource]]
   [ring.middleware.params :refer [wrap-params]]
   [ring.util.response :as res]
   [user :as user]))

(e/defn App []
  (e/client
   (println "Rendering Pong component on client")
   (!Pong)))

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

(defn wrap-index-page [next-handler config]
  (fn [ring-req]
    (println "wrap-index-page for:" (:uri ring-req))
    (if (= (:uri ring-req) "/")
      (do
        (println "Trying to serve index.html")
        (if-let [response (res/resource-response "public/example/index.html")]
          (do
            (println "Found index.html resource")
            (if-let [modules (get-modules (:manifest-path config))]
              (let [bag (merge config modules)
                    _ (println "Template replacement values:" bag)
                    content (template (slurp (:body response)) bag)]
                (-> (res/response content)
                    (res/content-type "text/html")
                    (res/header "Cache-Control" "no-store")))
              (do
                (println "No modules found in manifest")
                (-> (res/not-found "Missing shadow-cljs build manifest")
                    (res/content-type "text/plain")))))
          (do
            (println "Index.html resource not found")
            (next-handler ring-req))))
      (next-handler ring-req))))

(defn not-found-handler [_ring-request]
  (-> (res/not-found "Not found")
      (res/content-type "text/plain")))

(defn http-middleware [config]
  (let [handler (-> not-found-handler
                    (wrap-index-page config)
                    (wrap-resource (:resources-path config)))]
    (wrap-content-type handler)))

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
  
  (cond
    (and (= (:uri req) "/electric-ws")
         (= (get-in req [:headers "upgrade"]) "websocket"))
    (do
      (println "Handling Electric WebSocket connection with version:" user/USER_VERSION)
      (-> electric-handler
          (adapter/wrap-electric-websocket req)
          (cookies/wrap-cookies)
          (wrap-params)
          (adapter/wrap-reject-stale-client
           {:hyperfiddle.electric/user-version user/USER_VERSION})))

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
    ((-> (fn [_] (res/not-found "Not found"))
         (wrap-resource (:resources-path config))
         (wrap-content-type))
     req)))

(defn start-server! [electric-handler {:keys [host port resources-path manifest-path] :as config}]
  (println "Starting server with config:" config)
  (println "SERVER USER VERSION:" user/USER_VERSION)

  (when-let [resource-dir (io/resource resources-path)]
    (println "Resources directory found at:" resource-dir))

  (get-modules manifest-path)

  (let [handler (-> (fn [req]
                      (routes req electric-handler config))
                    (wrap-index-page config))
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
