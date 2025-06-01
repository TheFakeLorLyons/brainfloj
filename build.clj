(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.java.io :as io]
            [deps-deploy.deps-deploy :as dd]))

;;; See also https://clojure.org/guides/tools_build
;;; To install jar:  clj -T:build jar

(def project 'com.github.thefakelorlyons/brainfloj)
(def version  (format "0.0.73"))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def uber-file (format "target/%s-%s-standalone.jar" project version))
(def jar-file (format "target/%s-%s.jar" project version))

(def target-dir "target")
(def native-lib-dir "lib/native")

(def include-paths
  [#"^src/.*$"
   #"^resources/public/floj/.*$"
   #"^resources/public/fonts/.*$"])

(def exclude-paths
  [#".*\.lor$"
   #"recording_metadata\.edn$"
   #"calibration\.edn$"
   #"tags\.edn$"
   #"^lib/native/.*$"
   #"\.git/.*$"
   #"\.github/.*$"])


(defn clean [_]
  (println "Cleaning target directories...")
  (b/delete {:path target-dir})
  (println "Clean completed"))

(defn compile-clj [_]
  (println "Compiling Clojure code...")
  (b/copy-dir
   {:src-dirs ["src" "resources"]
    :target-dir class-dir
    :ignores [#".*\.lor"
              #"recording_metadata\.edn"
              #"calibration\.edn"
              #"tags\.edn"]})
  (println "Compilation completed"))

(defn jar [_]
  (println "Creating JAR file...")
  (clean nil)
  (compile-clj nil)
  (b/write-pom {:class-dir class-dir
                :lib project
                :version version
                :basis basis
                :src-dirs ["src"]
                :scm {:url "https://github.com/thefakelorlyons/brainfloj"}
                :pom-data
                [[:licenses
                  [:license
                   [:name "MIT License"]
                   [:url "https://opensource.org/licenses/MIT"]
                   [:distribution "repo"]]]]})
  (b/jar {:class-dir class-dir
          :jar-file jar-file})
  (println (format "JAR file created: %s" jar-file)))

(defn uber [_]
  (println "Creating optimized uber JAR file...")
  (clean nil)
  (compile-clj nil)

  #_#_(println "Copying BrainFlow JAR into the class directory...")
  (let [brainflow-jar "lib/brainflow-jar-with-dependencies.jar"]
    (when (.exists (io/file brainflow-jar))
      (b/copy-file {:src brainflow-jar
                    :target (str class-dir "/brainflow-jar-with-dependencies.jar")})
      (println "BrainFlow JAR copied successfully"))
    (when-not (.exists (io/file brainflow-jar))
      (println "WARNING: BrainFlow JAR not found at:" brainflow-jar)))

  (b/compile-clj {:basis basis
                  :src-dirs ["src"]
                  :class-dir class-dir})

  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis
           :main 'floj.cli'
           :exclude exclude-paths}))

(defn verify-native-libs [_]
  (println "Verifying native libraries...")
  (let [native-dir (io/file native-lib-dir)]
    (if (.exists native-dir)
      (let [libs (filter #(.isFile %) (file-seq native-dir))]
        (printf "Found %d native libraries in %s\n" (count libs) native-lib-dir)
        (doseq [lib libs]
          (println " -" (.getName lib))))
      (println "WARNING: Native library directory not found:" native-lib-dir))))

(defn install [_]
  (println "Installing project locally...")
  (jar nil)
  (b/install {:basis basis
              :lib project
              :version version
              :jar-file jar-file
              :class-dir class-dir})
  (println "Installation completed"))

(defn run-tests [_]
  (println "Running tests...")
  (let [test-result
        (b/process {:command-args ["clojure" "-M:test"]
                    :dir (.getPath (io/file "."))
                    :out :inherit
                    :err :inherit})]
    (when-not (zero? (:exit test-result))
      (System/exit (:exit test-result)))))

(defn run [_]
  (println "Running the application...")
  (let [run-result
        (b/process {:command-args ["clojure" "-M:flow" "-m" "floj.cli"]
                    :dir (.getPath (io/file "."))
                    :out :inherit
                    :err :inherit})]
    (when-not (zero? (:exit run-result))
      (System/exit (:exit run-result)))))

(defn doc [_]
  (println "Generating documentation...")
  (let [doc-result
        (b/process {:command-args ["clojure" "-M:doc"]
                    :dir (.getPath (io/file "."))
                    :out :inherit
                    :err :inherit})]
    (when-not (zero? (:exit doc-result))
      (System/exit (:exit doc-result)))))

(defn deploy [_]
  (println "Deploying to Clojars...")
  (jar nil)
  (dd/deploy {:installer :remote
              :artifact jar-file
              :pom-file (format "%s/META-INF/maven/%s/%s/pom.xml"
                                class-dir
                                (if (namespace project) (namespace project) (name project))
                                (name project))})
  (println "Deployed!"))