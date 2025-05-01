(ns calibration.websocket
  (:require [org.httpkit.server :as server]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.core.async :as async]
            [floj.record :as record]
            [floj.state :as state]))

(def clients (atom {}))
(def broadcast-channel (async/chan (async/sliding-buffer 10)))

(def server-instance (atom nil))

(def connection-status (atom {:connected false
                              :last-error nil}))

(defn connect! [channel]
  (let [client-id (str (java.util.UUID/randomUUID))]
    (swap! clients assoc client-id {:channel channel
                                    :connected-at (System/currentTimeMillis)
                                    :last-activity (System/currentTimeMillis)})
    #_(println "Client connected:" client-id)
    (swap! connection-status assoc :connected true :reconnect-attempts 0)
    client-id))

(defn disconnect! [client-id]
  (swap! clients dissoc client-id)
  (println "Client disconnected:" client-id)
  (when (empty? @clients)
    (swap! connection-status assoc :connected false)))

(defn handle-client-message [channel msg]
  (try
    (let [data (json/read-str msg :key-fn keyword)]
      (server/send! channel (json/write-str {:type "ack"
                                             :original_type (:type data)
                                             :timestamp (System/currentTimeMillis)})))
    (catch Exception e
      (println "Error handling client message:" (.getMessage e)))))

(defn ws-handler [request]
  (server/as-channel
    request
    {:on-open (fn [ch]
                (let [client-id (connect! ch)]
                  (server/send! ch (json/write-str {:type "connected"
                                                    :client_id client-id
                                                    :timestamp (System/currentTimeMillis)}))
                  (server/on-close ch (fn [status]
                                        (println "WebSocket connection closed with status:" status)
                                        (disconnect! client-id)))
                  (server/on-receive ch (fn [msg]
                                          (handle-client-message ch msg)))))}))

(defn serve-static-file [request]
  (let [file-path "resources/public/floj/eeg-visualizer.html"]
    (if (.exists (io/file file-path))
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (slurp file-path)}
      {:status 404
       :body "File not found"})))

(defn start-ws-server! [port]
  (println "Starting WebSocket server on port" port)
  (let [stop-server (server/run-server ws-handler {:port port})]
    (println "WebSocket server started")
    (reset! server-instance stop-server)
    stop-server))

(defn start-web-server! [port]
  (server/run-server
    (fn [request]
      (if (= (:uri request) "/")
        (serve-static-file request)
        {:status 404 :body "Not Found"}))
    {:port port})
  (println "Web server started on port" port))

(defn broadcast! [data]
  (let [json-data (json/write-str data)
        current-time (System/currentTimeMillis)]
    (doseq [[client-id client-data] @clients]
      (try
        (server/send! (:channel client-data) json-data)
        (swap! clients assoc-in [client-id :last-activity] current-time)
        (catch Exception e
          (println "Error sending to client" client-id ":" (.getMessage e))
          (disconnect! client-id))))))


(defn start-broadcast-process! []
  (async/go-loop []
    (when-let [data (async/<! broadcast-channel)]
      (try
        (broadcast! data)
        (catch Exception e
          (println "Error in broadcast process:" (.getMessage e))))
      (recur))))

(defn feed-eeg-data! [eeg-data]
  (try
    (when (seq @clients)
      (async/put! broadcast-channel eeg-data))
    (catch Exception e
      (println "Error feeding EEG data:" (.getMessage e)))))

(defn app-routes [req]
  (case (:uri req)
    "/ws" (ws-handler req)
    {:status 404 :body "Not found"}))

(defn init! [port]
  (start-ws-server! port)
  (start-broadcast-process!)
  (println "EEG WebSocket server initialized and ready to broadcast"))

(defn stop-and-save! []
  (println "Stopping recording...")
  (record/stop-recording!)
  (when @server-instance
    (@server-instance)
    (reset! server-instance nil))
  (let [data @state/eeg-data
        tags @state/tags
        board-id (.get_board_id @state/shim)
        saved-dir ((:write-lor! @state/state) data tags "calibration-session" board-id)]
    (println "Session saved to:" saved-dir)))