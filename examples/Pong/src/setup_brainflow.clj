(ns setup-brainflow
  (:require [setup :as setup]))

(defn -main []
  (setup/setup-derived-project!))