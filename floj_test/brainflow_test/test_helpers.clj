(ns floj-test.brainflow-test.test-helpers
  (:require [clojure.test :refer :all])
  (:import [brainflow BrainFlowInputParams BoardIds BoardShim]
           [java.io File]))

(def synthetic-board-id BoardIds/SYNTHETIC_BOARD)

(defn create-test-params
  "Create BrainFlowInputParams for tests"
  []
  (doto (BrainFlowInputParams.)
    (.set_serial_port "")))

(defn with-test-board
  "Test helper that creates a synthetic board, runs the test function,
   and ensures cleanup happens even if the test fails"
  [f]
  (let [params (create-test-params)
        board (BoardShim. synthetic-board-id (.toJson params))]
    (try
      (.prepare_session board)
      (f board)
      (finally
        (when (.is_prepared board)
          (try
            (.stop_stream board)
            (catch Exception _))
          (.release_session board))))))

(defmacro with-temp-file
  "Create a temporary file, bind it to the provided symbol, 
   and ensure it's deleted after test execution"
  [file-sym & body]
  `(let [~file-sym (File/createTempFile "brainflow-test" ".csv")]
     (try
       ~@body
       (finally
         (when (.exists ~file-sym)
           (.delete ~file-sym))))))

(defn assert-data-shape
  "Assert that the data has the expected shape"
  [data expected-channels expected-samples]
  (is (= expected-channels (count data))
      (str "Expected " expected-channels " channels but got " (count data)))
  (when (pos? (count data))
    (is (= expected-samples (count (first data)))
        (str "Expected " expected-samples " samples but got "
             (count (first data))))))

(defn hardware-available?
  "Check if specific hardware is available for testing"
  [board-id]
  (case board-id
    ;; Synthetic board is always available
    (BoardIds/SYNTHETIC_BOARD) true
    ;; For real hardware, check environment variables
    (boolean (System/getenv "BRAINFLOW_HARDWARE_TESTING"))))