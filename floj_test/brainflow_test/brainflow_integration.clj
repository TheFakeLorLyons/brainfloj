(ns floj-test.brainflow-test.brainflow-integration
  (:require [clojure.test :refer [deftest testing is use-fixtures run-tests]]
            [floj.brainflow.board-shim :as bs]
            [floj-test.brainflow-test.test-helpers :refer [with-test-board with-temp-file]])
  (:import [brainflow BoardShim BrainFlowInputParams BoardIds]))

(def ^:dynamic *board* nil)

(use-fixtures :once
  (fn [f]
    (with-test-board
      (fn [board]
        (binding [*board* board]
          (f))))))

(deftest ^:integration test-synthetic-board-streaming
  (testing "Basic streaming operations with synthetic board"
    (is (bs/board-ready? *board*) "Board should be ready")

    (bs/start-stream! *board* :buffer-size 45000)
    (Thread/sleep 1000)

    (let [data-count (bs/get-board-data-count *board*)
          data (bs/get-current-board-data *board* :num-samples 100)]
      (is (pos? data-count) "Should have collected some data")
      (is (seq data) "Should return data as vector")
      (is (vector? data) "Data should be a vector"))

    (bs/insert-marker! *board* 42.0)
    (Thread/sleep 200)

    (bs/stop-stream! *board*)))

#_(deftest ^:integration test-file-streaming
  (testing "Streaming data to a file"
    (with-temp-file tmp-file
      (let [file-path (.getAbsolutePath tmp-file)]
        ;; Setup streaming to file
        (bs/add-streamer! *board* (str "file://" file-path ":w"))
        (bs/start-stream! *board* :buffer-size 45000)

        ;; Generate data
        (Thread/sleep 1000)
        (bs/insert-marker! *board* 5.0)
        (Thread/sleep 1000)

        ;; Stop streaming  
        (bs/stop-stream! *board*)

        ;; Verify file exists and has content
        (is (.exists tmp-file) "Output file should exist")
        (is (pos? (.length tmp-file)) "Output file should have content")))))

(deftest ^:integration test-channel-info
  (testing "Channel information functions"
    (let [board-id BoardIds/SYNTHETIC_BOARD
          eeg-channels (bs/get-eeg-channels board-id)
          accel-channels (bs/get-accel-channels board-id)
          sampling-rate (bs/get-sampling-rate board-id)]
        (is (vector? eeg-channels) "EEG channels should be a vector")
        (is (vector? accel-channels) "Accel channels should be a vector")
        (is (pos? sampling-rate) "Sampling rate should be positive"))))

(deftest ^:integration test-advanced-data-retrieval
  (testing "Advanced data retrieval functions"
    (bs/start-stream! *board* :buffer-size 45000)
    (Thread/sleep 1000)

    (let [data-count (bs/get-board-data-count *board*)]
      (is (pos? data-count) "Data count should be positive")

      (let [data (bs/get-board-data *board* :num-samples data-count)]
        (is (seq data) "Should return data")
        (is (= (count (first data)) data-count)
            "Should return requested number of samples")))

    (bs/stop-stream! *board*)))

(deftest ^:integration ^:real-hardware test-with-real-hardware
  (testing "Tests with real hardware (skipped if not available)"
    (when (System/getenv "BRAINFLOW_HARDWARE_TESTING")
      (let [params (doto (BrainFlowInputParams.)
                     (.set_serial_port (or (System/getenv "BRAINFLOW_PORT") "COM3")))
            board-id (or (some-> (System/getenv "BRAINFLOW_BOARD_ID") Integer/parseInt)
                         BoardIds/CYTON_BOARD)
            real-board (BoardShim. board-id (.toJson params))]
        (try
          (.prepare_session real-board)
          (is (.is_prepared real-board) "Real board should be prepared")
          (.release_session real-board)
          (catch Exception e
            (println "Skipping real hardware test due to error:" (.getMessage e))))))))

(run-tests)