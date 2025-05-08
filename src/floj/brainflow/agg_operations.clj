(ns floj.brainflow.agg-operations
  "Clojure wrapper for BrainFlow's AggOperations enum"
  (:import [brainflow AggOperations]))

(def operations
  "Map of keywords to AggOperations enum values"
  {:mean AggOperations/MEAN
   :median AggOperations/MEDIAN
   :each AggOperations/EACH})

(def code->keyword
  "Map of integer codes to keywords"
  {0 :mean
   1 :median
   2 :each})

(defn get-code
  "Get the integer code for an aggregation operation.
   Can accept either a keyword from the operations map or an AggOperations enum directly."
  [operation]
  (cond
    (keyword? operation) (.get_code (get operations operation))
    (instance? AggOperations operation) (.get_code operation)
    :else (throw (IllegalArgumentException.
                  (str "Expected keyword or AggOperations enum, got: " operation)))))

(defn from-code
  "Get the AggOperations enum value from its integer code"
  [code]
  (AggOperations/from_code code))

(defn string-from-code
  "Get the string name of the AggOperations enum from its integer code"
  [code]
  (AggOperations/string_from_code code))

(defn keyword-from-code
  "Get the keyword representation of an AggOperations enum from its integer code"
  [code]
  (get code->keyword code))

(defn enum-value
  "Get the AggOperations enum value for the given operation.
   Can accept either a keyword from the operations map or an integer code."
  [operation]
  (cond
    (keyword? operation) (get operations operation)
    (integer? operation) (from-code operation)
    (instance? AggOperations operation) operation
    :else (throw (IllegalArgumentException.
                  (str "Expected keyword, integer, or AggOperations enum, got: " operation)))))