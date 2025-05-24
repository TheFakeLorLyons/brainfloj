(ns floj.log
  (:require [floj.brainflow.board-shim :as brainflow]))

(defn configure-brainflow-logging!
  "Configure BrainFlow logging to write to our application logs directory"
  []
  (let [log-dir (str (System/getProperty "user.home") "/.lor/logs/device_logs")
        log-file (str log-dir "/brainflow.log")]
    (try
      (brainflow/enable-dev-logger!)
      (brainflow/set-log-file! log-file)
      (brainflow/set-log-level! (:info brainflow/log-levels))
      (catch Exception e
        (println "Failed to configure BrainFlow logging:" (.getMessage e))))))

(defn redirect-system-output! []
  (let [log-file (str (System/getProperty "user.home") "/.lor/logs/app_logs/sys-out.log")
        log-stream (java.io.PrintStream. (java.io.FileOutputStream. log-file false))]
    (configure-brainflow-logging!)
    (System/setOut log-stream)
    (System/setErr log-stream)
    (System/setProperty "jna.debug_load" "false")
    (System/setProperty "jna.debug_load.jna" "false")
    (System/setProperty "jna.nosys" "true")))

