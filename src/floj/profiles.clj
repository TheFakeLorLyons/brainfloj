(ns floj.profiles
  "Profile management and connection to default devices. Profiles are a foundational part
   of building applications on top of BrainFloj, and each profile contains its own set
   of recordings and categories. Each profile contains a 'pong' category by default."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [floj.calibration :as calibrate]
            [floj.io :as fio]
            [floj.state :as state]))

(defn get-profiles-dir
  "Get the base directory containing all profiles"
  []
  (str (fio/config-base-dir) "/profiles/"))

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
  (let [base (get-profiles-dir)]
    (str base profile-name "/history")))

(defn get-profile-history-path
  "Get the path for storing profile history"
  [profile-name timestamp]
  (let [base (get-profiles-dir)]
    (str base profile-name "/history/" profile-name "_" timestamp ".edn")))

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
        existing-active-profile (:get-active-profile @state/state)
        history-dir (get-profile-history-dir profile-name)
        existing-history-files (seq (filter #(clojure.string/ends-with? (.getName %) ".edn")
                                            (file-seq (io/file history-dir))))]

    (if (or existing-active-profile existing-history-files)
      nil
      (do
        ; Ensure directories exist
        (fio/ensure-profile-directories! profile-name)

        ; Create the profile in history
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
          ; Write to history path
          (spit history-path (pr-str default-profile))
          (println "Created default profile at:" history-path)
          history-path)))))

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

(defn set-active-profile!
  "Updates the in-memory state to reflect the current active profile"
  [profile-name]
  (try
    (let [profile-data (load-profile profile-name)]
      (swap! state/state assoc :get-active-profile 
             (constantly profile-data)))
    (catch Exception e
      (println "Error loading profile data, using minimal profile:" (.getMessage e))
      (swap! state/state assoc :get-active-profile 
             (constantly {:name profile-name})))))

(defn set-default-profile!
  "Sets the active-profile key in config"
  [name]
  (let [config-path (fio/config-file-path)]
    (spit config-path (pr-str {:active-profile name}))
    (set-active-profile! name)
    (println "Set default profile to:" name)))

(defn create-profile!
  "Create a new profile with the given name. If `suppress-device-registration?` is true,
  it will skip asking about device registration."
  [profile-name & [suppress-device-registration?]]
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
      (when (= (clojure.string/lower-case (subs profile-response 0 1)) "y")
        (set-default-profile! profile-name)))
    ; Prompt for device config if not suppressed
    (when (not suppress-device-registration?)
      (print "Would you like to configure a new device? (y/n) ")
      (flush)
      (let [device-response (read-line)]
        (when (= (clojure.string/lower-case (subs device-response 0 1)) "y")
          ((:register-device @state/state) profile-name))))
    ((:fill-initial-lexicon! @state/state) profile-name)
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

(defn consolidate-profile-history!
  "Consolidate multiple profile files into one, keeping the most recent data"
  [profile-name consolidated-profile]
  (try
    (let [history-dir (get-profile-history-dir profile-name)
          history-files (when (.exists (io/file history-dir))
                          (->> (.listFiles (io/file history-dir))
                               (filter #(.isFile %))
                               (filter #(.endsWith (.getName %) ".edn"))
                               (sort-by #(.lastModified %))))]

      (when (> (count history-files) 1)
        (println "Consolidating" (count history-files) "profile files")

        ; Keep only the most recent file, delete the others
        (doseq [file (butlast history-files)]
          (try
            (.delete file)
            (println "Deleted old profile file:" (.getName file))
            (catch Exception e
              (println "Error deleting file" (.getName file) ":" (.getMessage e)))))

        (println "Consolidated profile history, kept most recent file")))
    (catch Exception e
      (println "Error consolidating profile history:" (.getMessage e))
      (.printStackTrace e))))


(defn save-profile!
  "Save a user profile to history - create new file only when necessary"
  [profile]
  (try
    (let [profile-name (:name profile)
          current-time (System/currentTimeMillis)
          calibration-count (get-in profile [:calibration-history :count] 0)
          latest-path (get-latest-profile-path profile-name)

          ; Create a new profile file if:
          ; 1. No profile file exists yet OR
          ; 2. We've reached the rotation interval (every 10 recordings) OR  
          ; 3. We need to consolidate (every 100 calibrations total)
          should-rotate? (and (pos? calibration-count)
                              (zero? (mod calibration-count calibrate/PROFILE_ROTATION_INTERVAL)))

          should-consolidate? (and (pos? calibration-count)
                                   (zero? (mod calibration-count calibrate/MAX_CALIBRATION_FILES)))

          create-new-file? (or (nil? latest-path)
                               (not (.exists (io/file (or latest-path ""))))
                               should-rotate?
                               should-consolidate?)

          ; Use existing path or create new timestamp-based path
          save-path (if create-new-file?
                      (get-profile-history-path profile-name current-time)
                      latest-path)

          updated-profile (assoc profile
                                 :updated-at (java.util.Date.)
                                 :version 1.0)]

      (fio/ensure-profile-directories! profile-name)

      (when should-consolidate?
        (println "Consolidating profile history after" calibration-count "recordings")
        (consolidate-profile-history! profile-name updated-profile))

      (spit save-path (pr-str updated-profile))
      (cond
        should-consolidate?
        (println "Consolidated and created new profile snapshot at:" save-path)

        should-rotate?
        (println "Created new profile rotation at:" save-path
                 "(after" calibration-count "recordings)")

        create-new-file?
        (println "Created new profile history snapshot at:" save-path)

        :else
        (println "Updated existing profile at:" save-path
                 "(calibration count:" calibration-count ")"))
      true)
    (catch Exception e
      (println "Error saving profile:" (.getMessage e))
      (.printStackTrace e)
      false)))

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
        (println "Failed to load profile, using default \n")
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