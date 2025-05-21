(ns floj.profiles
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [floj.calibration :as calibrate]
            [floj.io :as fio]
            [floj.state :as state]))

(defn get-profiles-dir
  "Get the base directory containing all profiles"
  []
  (str (fio/config-base-dir) "/profiles"))

(defn list-profiles
  "List all available profile names (directories under profiles/)"
  []
  (let [profiles-base-dir (get-profiles-dir)
        dir (io/file profiles-base-dir)]
    (when (.exists dir)
      (->> (.listFiles dir)
           (filter #(.isDirectory %))
           (map #(.getName %))
           (sort)))))

(defn get-profile-history-dir
  "Get the directory for storing profile history"
  [profile-name]
  (let [base (fio/config-base-dir)]
    (str base "/profiles/" profile-name "/history")))

(defn get-profile-history-path
  "Get the path for storing profile history"
  [profile-name timestamp]
  (let [base (fio/config-base-dir)]
    (str base "/profiles/" profile-name "/history/" profile-name "_" timestamp ".edn")))

(defn get-latest-profile-path
  "Get the path for the most recent profile file in history"
  [profile-name]
  (let [history-dir (get-profile-history-dir profile-name)
        dir (io/file history-dir)]
    (if-not (.exists dir)
      nil
      (let [history-files (->> (.listFiles dir)
                               (filter #(.isFile %))
                               (filter #(.endsWith (.getName %) ".edn"))
                               (sort-by #(.lastModified %)))]
        (when (seq history-files)
          (.getPath (last history-files)))))))

(defn get-current-profile-path
  "Get the path for the current profile file - either latest history or create new if none exists"
  [profile-name]
  (or (get-latest-profile-path profile-name)
      (let [timestamp (System/currentTimeMillis)]
        (get-profile-history-path profile-name timestamp))))

(defn create-default-profile!
  "Create a default profile only if none exists (either active or default on disk)."
  []
  (let [profile-name "default"
        timestamp (System/currentTimeMillis)
        existing-active-profile (:get-active-profile @state/state) ;; Adjust depending on how you track this
        history-dir (get-profile-history-dir profile-name)
        existing-history-files (seq (filter #(clojure.string/ends-with? (.getName %) ".edn")
                                            (file-seq (io/file history-dir))))]

    (if (or existing-active-profile existing-history-files)
      nil
      (do
        ;; Ensure directories exist
        (fio/ensure-profile-directories! profile-name)

        ;; Create the profile in history location only
        (let [history-path (get-profile-history-path profile-name timestamp)
              default-profile {:name profile-name
                               :created-at (java.util.Date.)
                               :updated-at (java.util.Date.)
                               :version 1
                               :keybindings {:start "s" :stop "e"}
                               :bci-device {:configured false
                                            :board-type "GANGLION_BOARD"
                                            :mac-address ""
                                            :com-port ""
                                            :connection-method "bled112"}
                               :golden-tensor {:spectral {:frequency-domain
                                                          {:delta 0.2 :theta 0.15 :alpha 0.25 :beta 0.3 :gamma 0.1}}}
                               :calibration-history {:count 0
                                                     :last-update timestamp
                                                     :files []}}]
          ;; Write only to history path
          (spit history-path (pr-str default-profile))
          (println "Created default profile at:" history-path)
          history-path)))))

(defn set-default-profile!
  "Sets the active-profile key in config"
  [name]
  (let [config-path (fio/config-file-path)]
    (spit config-path (pr-str {:active-profile name}))
    (println "Set default profile to:" name)))

(defn create-profile!
  "Create a new profile with the given name"
  [profile-name]
  (fio/ensure-profile-directories! profile-name)
  (let [timestamp (System/currentTimeMillis)
        new-profile {:name profile-name
                     :created-at (java.util.Date.)
                     :updated-at (java.util.Date.)
                     :version 1
                     :keybindings {:start "s" :stop "e"}
                     :bci-device {:configured false
                                  :board-type "GANGLION_BOARD"
                                  :mac-address ""
                                  :com-port ""
                                  :connection-method "bled112"}
                     :golden-tensor {:spectral {:frequency-domain
                                                {:delta 0.2 :theta 0.15 :alpha 0.25 :beta 0.3 :gamma 0.1}}}
                     :calibration-history {:count 0
                                           :last-update timestamp
                                           :files []}}
        history-path (get-profile-history-path profile-name timestamp)]
    (spit history-path (pr-str new-profile))
    (print "Would you like to set that as your default profile? (y/n) ")
    (flush)
    (let [profile-response (read-line)]
      (when (= (str/lower-case (subs profile-response 0 1)) "y")
        (set-default-profile! profile-name)
        (print "Would you like to configure a new device? (y/n) ")
        (flush)
        (let [device-response (read-line)]
          (when (= (str/lower-case (subs device-response 0 1)) "y")
            ((:register-device @state/state))))))
    new-profile))

(defn switch-profile! []
  (let [profiles (list-profiles)]
    (println "Available profiles:" profiles)
    (println "Enter the profile name to switch to:")
    (let [profile-name (read-line)]
      (if (some #{profile-name} profiles)
        (do
          (set-default-profile! profile-name)
          (println "Switched to profile:" profile-name))
        (println "Profile not found.")))))

(defn save-profile!
  "Save a user profile to history - create new file only when necessary"
  [profile]
  (try
    (let [profile-name (:name profile)
          current-time (System/currentTimeMillis)
          calibration-files-count (count (get-in profile [:calibration-history :files] []))
          latest-path (get-latest-profile-path profile-name)

          ;; Create a new file if:
          ;; 1. No profile file exists yet OR
          ;; 2. We've reached MAX_CALIBRATION_FILES in the calibration history
          ;; This triggers a "rotation" of the profile history
          create-new-file? (or (nil? latest-path)
                               (not (.exists (io/file (or latest-path ""))))
                               (>= calibration-files-count calibrate/MAX_CALIBRATION_FILES))

          ; Use existing path or create new timestamp-based path
          save-path (if create-new-file?
                      (get-profile-history-path profile-name current-time)
                      latest-path)

          updated-profile (assoc profile
                                 :updated-at (java.util.Date.)
                                 :version 1.0)]

      (fio/ensure-profile-directories! profile-name)
      (spit save-path (pr-str updated-profile))
      (if create-new-file?
        (println "Created new profile history snapshot at:" save-path
                 (when (>= calibration-files-count calibrate/MAX_CALIBRATION_FILES)
                   (str " (reached " calibration-files-count " files)")))
        (println "Updated existing profile history snapshot at:" save-path
                 " (calibration files:" calibration-files-count ")"))
      true)
    (catch Exception e
      (println "Error saving profile:" (.getMessage e))
      (.printStackTrace e)
      false)))

(defn load-profile
  "Load a user profile by name"
  [profile-name]
  (try
    (let [profile-path (get-current-profile-path profile-name)]
      (if (and profile-path (.exists (io/file profile-path)))
        ; Profile exists, load it
        (edn/read-string (slurp profile-path))
        ; Profile doesn't exist, create it
        (do
          (println "Profile does not exist, creating:" profile-name)
          (fio/ensure-profile-directories! profile-name)
          (let [timestamp (System/currentTimeMillis)
                new-profile {:name profile-name
                             :created-at (java.util.Date.)
                             :updated-at (java.util.Date.)
                             :version 1
                             :keybindings {:start "s" :stop "e"}
                             :bci-device {:configured false
                                          :board-type "GANGLION_BOARD"
                                          :mac-address ""
                                          :com-port ""
                                          :connection-method "bled112"}
                             :golden-tensor {:spectral {:frequency-domain
                                                        {:delta 0.2 :theta 0.15 :alpha 0.25 :beta 0.3 :gamma 0.1}}}
                             :calibration-history {:count 0
                                                   :last-update timestamp
                                                   :files []}}
                history-path (get-profile-history-path profile-name timestamp)]
            (spit history-path (pr-str new-profile))
            new-profile))))
    (catch Exception e
      (println "Error loading profile" profile-name ":" (.getMessage e))
      nil)))


(defn delete-profile! []
  (let [profiles (list-profiles)]
    (println "Available profiles:" profiles)
    (println "Enter the name of the profile to delete:")
    (let [name (read-line)
          path (str (get-profiles-dir) "/" name)
          config-path (fio/config-file-path)
          current-default (:active-profile (edn/read-string (slurp config-path)))]
      (cond
        (not (some #{name} profiles))
        (println "Profile not found.")

        (= name "default")
        (println "Cannot delete the default profile.")

        :else
        (do
          (io/delete-file path)
          (println "Deleted profile:" name)
          (when (= name current-default)
            (spit config-path (pr-str {:active-profile "default"}))
            (println "Deleted profile was active. Reverted to default.")))))))

(defn get-active-profile []
  (let [config-path (fio/config-file-path)
        config (try
                 (edn/read-string (slurp config-path))
                 (catch Exception e
                   {:active-profile "default"}))
        active-profile-name (:active-profile config "default")
        profile-path (get-current-profile-path active-profile-name)]
    (try
      (let [profile (edn/read-string (slurp profile-path))]
        (if (:name profile)
          profile
          (assoc profile :name active-profile-name)))
      (catch Exception e
        (println "Failed to load profile, using default")
        {:name "default"
         :bci-device {}
         :golden-tensor {}}))))

(defn count-profile-recordings
  "Count the total number of recordings for a profile"
  [profile-name]
  (try
    (let [profile-dir (str (:get-active-profile @state/state) "/recordings")
          recording-files (when (.exists (io/file profile-dir))
                            (->> (file-seq (io/file profile-dir))
                                 (filter #(.isDirectory %))
                                 (filter #(not= (.getName %) "recordings"))))]
      (count recording-files))
    (catch Exception e
      (println "Error counting profile recordings:" (.getMessage e))
      0)))

(defn show-current-profile []
  (let [active-profile (get-active-profile)]
    (println "Current active profile:" (:name active-profile))))

(defn initialize-profiles! []
  (state/register-fn!   :create-profile!          create-profile!)
  (state/register-fn!   :set-default-profile!     set-default-profile!)
  (state/register-fn!   :save-profile!            save-profile!)
  (state/register-fn!   :load-profile             load-profile)
  (state/register-fn!   :switch-profile!          switch-profile!)
  (state/register-fn!   :delete-profile!          delete-profile!)
  (state/register-fn!   :get-active-profile       get-active-profile)
  (state/register-fn!   :count-profile-recordings count-profile-recordings))