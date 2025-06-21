(ns floj.io
  "Responsible for handling general file io used by the rest of BrainFloj."
  (:require [floj.state :as state]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn file-exists?
  "Check if a file exists"
  [path]
  (.exists (io/file path)))

(defn dir-exists?
  "Check if a directory exists at the given path"
  [path]
  (let [f (io/file path)]
    (and (.exists f) (.isDirectory f))))

(defn read-edn-file
  "Read an EDN file"
  [file-path]
  (with-open [r (io/reader file-path)]
    (edn/read (java.io.PushbackReader. r))))

(defn list-subdirectories
  "List subdirectories in a given directory"
  [dir-path]
  (let [dir (io/file dir-path)]
    (->> (.listFiles dir)
         (filter #(.isDirectory %))
         (map #(.getName %)))))

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

(defn ensure-profile-directories!
  "Create the directory structure for a specific user profile"
  [profile-name]
  (let [base (config-base-dir)
        profile-base (str base "/profiles/" profile-name)]
    (doseq [sub ["" "/history" "/wave_lexicon"]]
      (ensure-directory! (str profile-base sub)))))

(defn ensure-config-directories!
  "Create the .lor config directory and subdirectories with enhanced structure"
  []
  (let [base (config-base-dir)]
    (doseq [sub [""
                 "/profiles"
                 "/logs"
                 "/logs/device_logs"
                 "/logs/io_logs"
                 "/logs/app_logs"]]
      (ensure-directory! (str base sub)))

    ; Ensure profile subdirectories exist for default profile
    (let [default-profile-dir (str base "/profiles/default")]
      (doseq [sub [""
                   "/history"
                   "/wave_lexicon"]]
        (ensure-directory! (str default-profile-dir sub))))))

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

(defn extract-timestamp-from-recording-dir
  "Extract timestamp from a recording directory path like 'recording_1746839600607'"
  [dir-path]
  (try
    (when dir-path
      (println "Extracting timestamp from:" dir-path)
      (let [matches (re-find #"recording_(\d+)" dir-path)]
        (when matches
          (println "Found matches:" matches)
          (let [timestamp-str (second matches)]
            (when timestamp-str
              (let [timestamp (Long/parseLong timestamp-str)]
                (println "Extracted timestamp:" timestamp)
                timestamp))))))
    (catch Exception e
      (println "Error extracting timestamp from directory:" dir-path)
      (println "Exception:" (.getMessage e))
      nil)))

(defn create-recording-directory!
  "Create a directory for storing recording contents"
  ([base-name]
   (create-recording-directory! base-name nil))
  ([base-name custom-name]
   (let [timestamp (System/currentTimeMillis)
         recordings-dir (get-recordings-dir)
         dir-name (if custom-name
                    (str base-name "/" custom-name "_" timestamp)
                    (str recordings-dir "/" base-name "_" timestamp))
         dir (io/file dir-name)]
     (.mkdir dir)
     dir-name)))

(defn create-default-config!
  "Create a default configuration file if one doesn't exist"
  []
  (let [config-path (config-file-path)
        file (io/file config-path)]
    (when-not (.exists file)
      (let [default-config {:version "1.0"
                            :active-profile "default"
                            :profiles ["default"]
                            :recording-directory "resources/recordings"
                            :recording-counter 0}]
        (spit file (pr-str default-config))))
    config-path))

(defn application-setup!
  "Ensure the config file exists, create default if needed"
  []
  (println "\n\t\t    *~-= Welcome to BrainFloj! =-~*\n")
  (ensure-config-directories!)
  (create-default-config!))

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

(defn get-wave-lexicon-dir
  "Get the directory for wave lexicon entries"
  [profile-name category]
  (let [base (config-base-dir)]
    (str base "/profiles/" profile-name "/wave_lexicon/" category)))

(defn load-profile-history
  "Load profile history files, sorted by timestamp"
  [profile-name limit]
  (try
    (let [history-dir (str (config-base-dir) "/profiles/" profile-name "/history")
          history-files (when (.exists (io/file history-dir))
                          (->> (.listFiles (io/file history-dir))
                               (filter #(.isFile %))
                               (filter #(.endsWith (.getName %) ".edn"))
                               (sort-by #(.lastModified %))
                               (take-last (or limit 10))))]

      (for [file history-files]
        (try
          (edn/read-string (slurp file))
          (catch Exception e
            (println "Error reading history file:" (.getName file))
            nil))))
    (catch Exception e
      (println "Error loading profile history:" (.getMessage e))
      [])))

(defn update-recording-counter!
  "Increment the recording counter in config"
  []
  (try
    (let [config-path (config-file-path)
          config (edn/read-string (slurp config-path))
          current-count (or (:recording-counter config) 0)
          updated-count (inc current-count)
          updated-config (assoc config :recording-counter updated-count)]
      (spit config-path (pr-str updated-config))
      updated-count)
    (catch Exception e
      (println "Error updating recording counter:" (.getMessage e))
      nil)))

(defn get-recording-counter
  "Get the current recording counter value"
  []
  (try
    (let [config (edn/read-string (slurp (config-file-path)))]
      (or (:recording-counter config) 0))
    (catch Exception e
      (println "Error getting recording counter:" (.getMessage e))
      0)))


(defn initialize-io! []
  (state/register-fn! :default-config            create-default-config!)
  (state/register-fn! :ensure-config-directories ensure-config-directories!)
  (state/register-fn! :load-configurations!      load-configurations!)
  (state/register-fn! :load-profile-history      load-profile-history)
  (state/register-fn! :update-recording-counter! update-recording-counter!)
  (state/register-fn! :get-recording-counter     get-recording-counter)
  (state/register-fn! :update-recording-counter! update-recording-counter!)
  (state/register-fn! :get-recording-counter     get-recording-counter))