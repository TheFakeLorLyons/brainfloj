(ns floj.profiles
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [floj.io :as fio]
            [floj.state :as state]))

(defn get-profiles-dir []
  (str (fio/config-base-dir) "/profiles"))

(defn list-profiles []
  (let [profiles-dir (get-profiles-dir)]
    (->> (file-seq (io/file profiles-dir))
      (filter #(str/ends-with? (.getName %) ".edn"))
      (map #(subs (.getName %) 0 (- (count (.getName %)) 4)))
      (sort))))

(defn profile-path [name]
  (str (get-profiles-dir) "/" name ".edn"))

(defn create-profile!
  "Create a new profile with optional config overrides"
  [name & {:keys [sampling-rate eeg-channels keybindings]
           :or {sampling-rate 250
                eeg-channels ["O1" "O2" "C3" "C4"]
                keybindings {:start "s" :stop "e"}}}]
  (let [data {:name name
              :sampling-rate sampling-rate
              :channels {:eeg eeg-channels}
              :keybindings keybindings}]
    (spit (profile-path name) (pr-str data))
    (println "Created profile:" name)))

(defn set-default-profile!
  "Sets the active-profile key in config"
  [name]
  (let [config-path (fio/config-file-path)]
    (spit config-path (pr-str {:active-profile name}))
    (println "Set default profile to:" name)))

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

(defn delete-profile! []
  (let [profiles (list-profiles)]
    (println "Available profiles:" profiles)
    (println "Enter the name of the profile to delete:")
    (let [name (read-line)
          path (profile-path name)
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
        profile-path (str (get-profiles-dir) "/" active-profile-name ".edn")]
    (try
      (let [profile (edn/read-string (slurp profile-path))]
        (if (:name profile)
          profile
          (assoc profile :name active-profile-name)))
      (catch Exception e
        (println "Failed to load profile, using default")
        {:name "default"
         :sampling-rate 250
         :channels {:eeg ["Unknown"]}
         :keybindings {:start "s" :stop "e"}}))))

(defn show-current-profile []
  (let [active-profile (get-active-profile)]
    (println "Current active profile:" (:name active-profile))))

(defn initialize-profiles! []
  (state/register-fn! :create-profile!      create-profile!)
  (state/register-fn! :set-default-profile! set-default-profile!)
  (state/register-fn! :switch-profile!      switch-profile!)
  (state/register-fn! :delete-profile!      delete-profile!)
  (state/register-fn! :get-active-profile   get-active-profile))