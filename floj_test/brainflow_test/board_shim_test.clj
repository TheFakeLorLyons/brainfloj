(ns floj-test.brainflow-test.board-shim-test
  (:require [clojure.test :refer :all]
            [floj.brainflow.board-shim :as brainflow])
  (:import [brainflow BoardShim BrainFlowInputParams BoardIds BrainFlowPresets]
           [java.io File]
           [org.mockito Mockito]))

(defn create-mock [class]
  (Mockito/mock class))

(defn verify-call
  ([mock method times]
   (-> (Mockito/verify mock (Mockito/times times))
       (.method)))
  ([mock method times args]
   (-> (Mockito/verify mock (Mockito/times times))
       (.method (into-array Object args)))))

(defn when-call
  ([mock method return-val]
   (-> (Mockito/when (.method mock))
       (.thenReturn return-val)))
  ([mock method args return-val]
   (-> (Mockito/when (.method mock (into-array Object args)))
       (.thenReturn return-val))))

(def test-board-id BoardIds/SYNTHETIC_BOARD)
(def test-preset BrainFlowPresets/DEFAULT_PRESET)

(deftest test-session-management
  (testing "prepare-session!"
    (let [mock-board (create-mock BoardShim)]
      (brainflow/prepare-session! mock-board)
      (verify-call mock-board "prepare_session" 1)))

  (testing "release-session!"
    (let [mock-board (create-mock BoardShim)]
      (brainflow/release-session! mock-board)
      (verify-call mock-board "release_session" 1)))

  (testing "board-ready?"
    (let [mock-board (create-mock BoardShim)]
      (when-call mock-board "is_prepared" true)
      (is (brainflow/board-ready? mock-board)))))

(deftest test-streaming
  (testing "start-stream! with default params"
    (let [mock-board (create-mock BoardShim)]
      (brainflow/start-stream! mock-board)
      (verify-call mock-board "start_stream" 1)))

  (testing "start-stream! with buffer size"
    (let [mock-board (create-mock BoardShim)
          buffer-size 1000]
      (brainflow/start-stream! mock-board :buffer-size buffer-size)
      (verify-call mock-board "start_stream" 1 [buffer-size])))

  (testing "start-stream! with buffer size and streamer params"
    (let [mock-board (create-mock BoardShim)
          buffer-size 1000
          streamer-params "file://test.csv:w"]
      (brainflow/start-stream! mock-board :buffer-size buffer-size :streamer-params streamer-params)
      (verify-call mock-board "start_stream" 1 [buffer-size streamer-params])))

  (testing "stop-stream!"
    (let [mock-board (create-mock BoardShim)]
      (brainflow/stop-stream! mock-board)
      (verify-call mock-board "stop_stream" 1))))

(deftest test-board-data
  (testing "get-board-data with default params"
    (let [mock-board (create-mock BoardShim)
          mock-data (make-array Double/TYPE 2 2)]
      (aset mock-data 0 0 1.0)
      (aset mock-data 0 1 2.0)
      (aset mock-data 1 0 3.0)
      (aset mock-data 1 1 4.0)
      (when-call mock-board "get_board_data" [250 test-preset] mock-data)
      (is (= [[1.0 2.0] [3.0 4.0]] (brainflow/get-board-data mock-board)))))

  (testing "get-current-board-data"
    (let [mock-board (create-mock BoardShim)
          mock-data (make-array Double/TYPE 2 2)]
      (aset mock-data 0 0 1.0)
      (aset mock-data 0 1 2.0)
      (aset mock-data 1 0 3.0)
      (aset mock-data 1 1 4.0)
      (when-call mock-board "get_current_board_data" [250 test-preset] mock-data)
      (is (= [[1.0 2.0] [3.0 4.0]] (brainflow/get-current-board-data mock-board)))))

  (testing "get-board-data-count"
    (let [mock-board (create-mock BoardShim)]
      (when-call mock-board "get_board_data_count" [test-preset] 42)
      (is (= 42 (brainflow/get-board-data-count mock-board))))))

(deftest test-channel-information
  (testing "get-eeg-channels"
    (is (vector? (brainflow/get-channel-data :eeg test-board-id)))
    (is (every? number? (brainflow/get-channel-data :eeg test-board-id))))

  (testing "get-accel-channels"
    (is (vector? (brainflow/get-channel-data :accel test-board-id)))
    (is (every? number? (brainflow/get-channel-data :accel test-board-id))))

  (testing "get-sampling-rate"
    (is (number? (brainflow/get-sampling-rate test-board-id)))))

;Integration tests (conditionally run when hardware is available)
(def hardware-available?
  "Check if real hardware is available for testing.
   This could be a simple check for a environment variable or config file."
  (boolean (System/getenv "BRAINFLOW_HARDWARE_TESTING")))

(deftest ^:integration test-with-real-board
  (when hardware-available?
    (let [params (doto (BrainFlowInputParams.)
                   (.set_serial_port "COM3"));COM port individually varies
          board (brainflow/create-board-shim test-board-id :params params)]

      (testing "Full session lifecycle"
        (brainflow/prepare-session! board)
        (is (brainflow/board-ready? board))

        (brainflow/start-stream! board :buffer-size 450000)
        (Thread/sleep 5000) ;Collect some data

        (let [data (brainflow/get-current-board-data board :num-samples 250)]
          (is (seq data))
          (is (pos? (count data))))

        (brainflow/stop-stream! board)
        (brainflow/release-session! board)))))

;Helper for file-based testing with simulated data
(deftest test-file-streaming
  (testing "File-based data streaming"
    (let [tmp-file (File/createTempFile "brainflow-test" ".csv")
          tmp-path (.getAbsolutePath tmp-file)
          params (doto (BrainFlowInputParams.)
                   (.set_serial_port ""))
          board (brainflow/create-board-shim BoardIds/SYNTHETIC_BOARD :params (.toJson params))]

      ;Setup streaming to file
      (brainflow/prepare-session! board)
      (brainflow/add-streamer! board (str "file://" tmp-path ":w"))
      (brainflow/start-stream! board :buffer-size 45000)

      ;Generate synthetic data
      (Thread/sleep 1000)
      (brainflow/insert-marker! board 5.0)
      (Thread/sleep 1000)

      ;Cleanup
      (brainflow/stop-stream! board)
      (brainflow/release-session! board)

      ;Verify file exists and has content
      (is (.exists tmp-file))
      (is (pos? (.length tmp-file)))

      ;Clean up temp file
      (.delete tmp-file))))

;Test utility functions
(deftest test-utility-functions
  (testing "get-version returns a string"
    (is (string? (brainflow/get-version))))

  (testing "sampling rate returns number"
    (is (number? (brainflow/get-sampling-rate test-board-id)))))

(deftest test-channel-information
  (let [board-id test-board-id]
    (doseq [channel-type brainflow/channel-types]
      (testing (str "Channel getter: " channel-type)
        (let [getter-fn (ns-resolve 'floj.brainflow.board-shim
                                    (symbol (str "get-" (name channel-type) "-channels")))
              result (getter-fn board-id)]
          (is (vector? result)
              (str "Expected a vector from " (name channel-type)))
          (is (every? number? result)
              (str "Expected all numeric values from " (name channel-type))))))))

#_(deftest test-get-eeg-names
  (testing "Getting EEG names"
    (let [result (brainflow/get-channel-data :eeg-names board-id)]
      (is (vector? result) "Should return a vector")
      (is (every? string? result) "Should contain strings"))))

#_(deftest test-get-package-num-channel
    (testing "Getting package number channel"
      (let [result (brainflow/get-channel-data :package-num board-id)]
        (is (integer? result) "Should return an integer"))))

(run-tests)