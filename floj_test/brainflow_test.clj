(ns floj-test.brainflow-test
  (:import [brainflow BrainFlowInputParams BoardIds]))

(defn test-brainflow-import []
  (println "Testing BrainFlow import...")
  (try
    ;; Create a simple BrainFlow input params object
    (let [params (BrainFlowInputParams.)]
      (println "Successfully created BrainFlowInputParams instance"))

    ;; Try accessing a constant
    (println "Synthetic board ID:" BoardIds/SYNTHETIC_BOARD)

    (println "BrainFlow is properly imported!")
    (catch Exception e
      (println "Error importing BrainFlow:")
      (.printStackTrace e))))

(defn -main [& args]
  (test-brainflow-import))