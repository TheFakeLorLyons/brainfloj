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

  (let [deps-file (io/file "deps.edn")]
    (if (.exists deps-file)
      (let [current-deps (edn/read-string (slurp deps-file))
            brainflow-dep (get-in current-deps [:deps 'brainflow/brainflow])
            flow-jvm-opts (get-in current-deps [:aliases :flow :jvm-opts])]

        (if brainflow-dep
          (let [brainflow-config {:brainflow-dep brainflow-dep
                                  :jvm-opts (or flow-jvm-opts [])}]
            (copy-brainflow-config! brainflow-config)
            (println "✓ Copied brainflow configuration from current project")
            (println "Restart with: clojure -A:flow"))
          (do
            (println "❌ No brainflow/brainflow dependency found in current deps.edn!")
            (println "Please run brainfloj setup first:"))))
      (do
        (println "❌ No deps.edn found in current directory!")
        (println "Please run brainfloj setup first:")
        (println "  clojure -Sdeps '{:deps {com.github.thefakelorlyons/brainfloj {:mvn/version \"0.0.71\"}}}' -A:setup")))))

; Run this in order to download and setup the necessary brainflow-java dependencies
(defn setup-brainfloj! []
  (println "Setting up BrainFloj...")
  (bf/test-brainflow)
  (println "\n✓ Setup complete!")
  (println "Now restart your REPL with: clojure -A:flow -m floj.cli")
  (println "Or run from command line: clojure -A:flow -m floj.cli"))

(defn -main []
  (setup-brainfloj!))