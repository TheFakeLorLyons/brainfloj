(ns floj.io
  (:require [floj.state :as state]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn config-base-dir
  "Get the base config directory path"
  []
  (str (System/getProperty "user.home") "/.lor"))

(defn ensure-directory!
  "Create directory if it doesn't exist"
  [dir-path]
  (let [dir (io/file dir-path)]
    (when-not (.exists dir)
      (.mkdirs dir))
    dir-path))

(defn ensure-config-directories!
  "Create the .lor config directory and subdirectories"
  []
  (let [base (config-base-dir)]
    (doseq [sub [""
                 "/profiles"
                 "/logs"
                 "/logs/device_logs"
                 "/logs/io_logs"
                 "/logs/app_logs"]]
      (ensure-directory! (str base sub)))))

(defn config-file-path
  "Get the path to the config file"
  []
  (str (config-base-dir) "/config.edn"))

(defn get-recordings-dir
  "Get the recordings directory path relative to the project"
  []
  (let [recordings-dir "resources/recordings"]
    (ensure-directory! recordings-dir)
    recordings-dir))

(defn create-recording-directory!
  "Create a directory for storing recording contents"
  [base-name]
  (let [timestamp (System/currentTimeMillis)
        recordings-dir (get-recordings-dir)
        dir-name (str recordings-dir "/" base-name "_" timestamp)
        dir (io/file dir-name)]
    (.mkdir dir)
    dir-name))

(defn create-default-config!
  "Create a default configuration file if one doesn't exist"
  []
  (let [config-path (config-file-path)
        file (io/file config-path)]
    (when-not (.exists file)
      (let [default-config {:version "1.0"
                            :active-profile "default"
                            :profiles ["default"]
                            :recording-directory "resources/recordings"}]
        (spit file (pr-str default-config))))
    config-path))

(defn create-default-profile!
  "Create a default profile if one doesn't exist"
  []
  (let [profile-path (str (config-base-dir) "/profiles/default.edn")
        file (io/file profile-path)]
    (when-not (.exists file)
      (let [default-profile {:name "default"
                             :created-at (java.util.Date.)
                             :keybindings {:start "s" :stop "e"}
                             :bci-device {:configured false
                                          :board-type "GANGLION_BOARD"
                                          :mac-address ""
                                          :com-port ""
                                          :connection-method "bled112"}}]
        (spit profile-path (pr-str default-profile))))
    profile-path))

(defn application-setup!
  "Ensure the config file exists, create default if needed"
  []
  (println "Starting BrainFloj!")
  (ensure-config-directories!)
  (create-default-config!)
  (create-default-profile!))

(defn load-configurations!
  "Load configuration, creating default if necessary"
  []
  (let [config-path (config-file-path)]
    (try
      (let [config (edn/read-string (slurp config-path))]
        config)
      (catch Exception e
        (println "Error loading config:" (.getMessage e))
        (println "Creating default config and trying again")
        (create-default-config!)
        (try
          (let [config (edn/read-string (slurp config-path))]
            (println "Loaded default config from" config-path)
            config)
          (catch Exception e2
            (println "Failed to load default config:" (.getMessage e2))
            {:active-profile "default"}))))))

(defn initialize-io! []
  (state/register-fn! :default-config            create-default-config!)
  (state/register-fn! :ensure-config-directories ensure-config-directories!)
  (state/register-fn! :load-configurations!      load-configurations!))