(ns floj-test.brainflow-test.agg-operations-test
  (:require [clojure.test :refer :all]
            [floj.brainflow.agg-operations :as agg])
  (:import [brainflow AggOperations]))

(deftest operations-map-test
  (testing "operations map contains all expected values"
    (is (= AggOperations/MEAN (get agg/operations :mean)))
    (is (= AggOperations/MEDIAN (get agg/operations :median)))
    (is (= AggOperations/EACH (get agg/operations :each)))))

(deftest code-to-keyword-map-test
  (testing "code->keyword map contains all expected values"
    (is (= :mean (get agg/code->keyword 0)))
    (is (= :median (get agg/code->keyword 1)))
    (is (= :each (get agg/code->keyword 2)))))

(deftest get-code-test
  (testing "get-code with keywords"
    (is (= 0 (agg/get-code :mean)))
    (is (= 1 (agg/get-code :median)))
    (is (= 2 (agg/get-code :each))))

  (testing "get-code with enum values"
    (is (= 0 (agg/get-code AggOperations/MEAN)))
    (is (= 1 (agg/get-code AggOperations/MEDIAN)))
    (is (= 2 (agg/get-code AggOperations/EACH))))

  (testing "get-code with invalid input"
    (is (thrown? IllegalArgumentException (agg/get-code "invalid")))))

(deftest from-code-test
  (testing "from-code returns correct enum values"
    (is (= AggOperations/MEAN (agg/from-code 0)))
    (is (= AggOperations/MEDIAN (agg/from-code 1)))
    (is (= AggOperations/EACH (agg/from-code 2)))))

(deftest string-from-code-test
  (testing "string-from-code returns correct string names"
    (is (= "MEAN" (agg/string-from-code 0)))
    (is (= "MEDIAN" (agg/string-from-code 1)))
    (is (= "EACH" (agg/string-from-code 2)))))

(deftest keyword-from-code-test
  (testing "keyword-from-code returns correct keywords"
    (is (= :mean (agg/keyword-from-code 0)))
    (is (= :median (agg/keyword-from-code 1)))
    (is (= :each (agg/keyword-from-code 2)))))

(deftest enum-value-test
  (testing "enum-value with keywords"
    (is (= AggOperations/MEAN (agg/enum-value :mean)))
    (is (= AggOperations/MEDIAN (agg/enum-value :median)))
    (is (= AggOperations/EACH (agg/enum-value :each))))

  (testing "enum-value with integer codes"
    (is (= AggOperations/MEAN (agg/enum-value 0)))
    (is (= AggOperations/MEDIAN (agg/enum-value 1)))
    (is (= AggOperations/EACH (agg/enum-value 2))))

  (testing "enum-value with enum values"
    (is (= AggOperations/MEAN (agg/enum-value AggOperations/MEAN)))
    (is (= AggOperations/MEDIAN (agg/enum-value AggOperations/MEDIAN)))
    (is (= AggOperations/EACH (agg/enum-value AggOperations/EACH))))

  (testing "enum-value with invalid input"
    (is (thrown? IllegalArgumentException (agg/enum-value "invalid")))))

(run-tests)