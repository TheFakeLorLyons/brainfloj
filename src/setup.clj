(ns setup
  (:require [brainflow-java.core :as bf]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [zprint.core :as zprint]))

; Run this prior to publish anything using brainfloj
(defn clean-deps 
  []
 (bf/remove-brainflow-from-deps))

(defn copy-brainflow-config! [config]
  "Copy brainflow config to current project's deps.edn"
  (let [deps-file (io/file "deps.edn")
        current-deps (edn/read-string (slurp deps-file))

        updated-deps (-> current-deps
                         (assoc-in [:deps 'brainflow/brainflow] (:brainflow-dep config))
                         (assoc-in [:aliases :dev :jvm-opts] (:jvm-opts config)))]

    (spit deps-file
          (zprint/zprint-str updated-deps
                             {:map {:sort? false}
                              :width 80}))))

(defn setup-derived-project!
  "Setup a project that depends on brainfloj by copying existing brainflow config"
  []
  (println "Setting up brainfloj-derived project...")

  ; Look for brainflow config in the standard location
  (let [brainflow-dir (io/file (System/getProperty "user.home") ".brainflow-java")]
    (if (.exists brainflow-dir)
      ; Find the version directory (e.g., "5.16.0") and look for config
      (let [version-dirs (->> (.listFiles brainflow-dir)
                              (filter #(.isDirectory %))
                              (sort)
                              reverse)
            config-file (->> version-dirs
                             (map #(io/file % "brainflow-config.edn"))
                             (filter #(.exists %))
                             first)]
        (if config-file
          (let [brainflow-config (edn/read-string (slurp config-file))]
            (copy-brainflow-config! brainflow-config)
            (println "✓ Copied brainflow configuration from existing setup")
            (println (str "✓ Using config from: " (.getAbsolutePath config-file)))
            (println "Restart with: clojure -A:flow"))
          (do
            (println "❌ No brainflow-config.edn found in any version directory!")
            (println "Please run brainfloj setup first:"))))
      (do
        (println "❌ No .brainflow-java directory found!")
        (println "Please run brainflow-java setup first:")
        (println "`clj -m setup` in the CLI")))))

; Run this in order to download and setup the necessary brainflow-java dependencies
(defn setup-brainfloj! []
  (println "Setting up BrainFloj...")
  (bf/test-brainflow)
  (println "\n✓ Setup complete!")
  (println "Now restart your REPL with: clojure -A:flow -m floj.cli")
  (println "Or run from command line: clojure -A:flow -m floj.cli"))

(defn -main []
  (setup-brainfloj!))