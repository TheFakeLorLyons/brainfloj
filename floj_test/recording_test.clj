(ns floj-test.recording-test
  (:require [floj.cli :as cli]
            [flow.io :as io]
            [flow.state :as state])
  (:import [brainflow BoardIds]))

(defn generate-square-wave
  "Generate a synthetic square wave for testing"
  [frequency amplitude duration sampling-rate]
  (let [total-samples (int (* duration sampling-rate))
        period-samples (int (/ sampling-rate frequency))
        half-period (int (/ period-samples 2))]
    (into-array
      (repeatedly 4
        #(double-array
           (for [i (range total-samples)]
             (* amplitude
               (if (< (mod i period-samples) half-period) 1.0 -1.0))))))))

(defn create-test-board-shim
  "Create a test board-shim that returns a square wave"
  []
  (let [samples (atom (generate-square-wave 10 50 5 250)) ; 10Hz, 50μV, 5 seconds
        index (atom 0)]
    (proxy [BoardShim] [BoardIds/SYNTHETIC_BOARD (BrainFlowInputParams.)]
      (getBoardId [] BoardIds/SYNTHETIC_BOARD)
      (prepareSession [] nil)
      (startStream [] nil)
      (stopStream [] nil)
      (releaseSession [] nil)
      (getBoardData
        ([] @samples)
        ([num-samples]
         (let [start @index
               end (min (+ start num-samples) (alength (aget @samples 0)))
               result (make-array Double/TYPE (alength @samples) (- end start))]
           (doseq [i (range (alength @samples))]
             (doseq [j (range (- end start))]
               (aset result i j (aget (aget @samples i) (+ start j)))))
           (reset! index end)
           result))))))

(defn start-test-mode []
  (println "Starting in TEST MODE with synthetic square wave")
  (let [board-shim (create-test-board-shim)]
    (cli/cli-program board-shim)))

(defn -main [& args]
  (println "Starting LOR BrainFlow Recording Application")
  (io/ensure-config-directories!)
  (if (and (seq args) (= (first args) "test"))
    (start-test-mode)
    (if-let [board-shim (cli/initialize-board @state/board-id-atom)]
      (cli/cli-program board-shim)
      (println "Failed to initialize board. Exiting."))))