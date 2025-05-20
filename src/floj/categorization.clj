(ns floj.categorization
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [floj.io :as fio]))

(defn find-signature-recording-dirs
  "Find all recording directories for a specific signature type"
  [profile-name category signature-type]
  (try
    (let [signature-dir (str (fio/get-wave-lexicon-dir profile-name category) "/" signature-type)
          signature-file (io/file signature-dir)]
      (when (.exists signature-file)
        (->> (.listFiles signature-file)
             (filter #(.isDirectory %))
             (filter #(str/includes? (.getName %) (str signature-type "_")))
             (sort-by #(.lastModified %)))))
    (catch Exception e
      (println "Error finding recording directories for signature" signature-type ":" (.getMessage e))
      [])))

(defn get-signature-types
  "Get all signature types for a category"
  [profile-name category]
  (try
    (let [category-dir (fio/get-wave-lexicon-dir profile-name category)
          category-file (io/file category-dir)]
      (when (.exists category-file)
        (->> (.listFiles category-file)
             (filter #(.isDirectory %))
             ; Only include directories that don't have timestamps (not recording dirs)
             (filter #(not (str/includes? (.getName %) "_")))
             (map #(.getName %)))))
    (catch Exception e
      (println "Error finding signature types:" (.getMessage e))
      [])))

(defn get-category-stats
  "Get stats about a specific wave signature category"
  [profile-name category]
  (try
    (let [category-dir (fio/get-wave-lexicon-dir profile-name category)
          template-file (str category-dir "/signature.edn")
          has-template (.exists (io/file template-file))
          template (when has-template
                     (try (edn/read-string (slurp template-file))
                          (catch Exception _ nil)))
          instance-count (or (:instance-count template) 0)
          last-updated (:last-updated template)
          signatures (when (.exists (io/file category-dir))
                       (->> (.listFiles (io/file category-dir))
                            (filter #(.isDirectory %))
                            (count)))]
      {:category category
       :instance-count instance-count
       :signature-count signatures
       :has-template has-template
       :last-updated last-updated})
    (catch Exception e
      (println "Error getting category stats:" (.getMessage e))
      {:category category
       :instance-count 0
       :signature-count 0
       :has-template false
       :last-updated nil})))

(defn extract-metadata
  "Extract metadata from a signature directory"
  [signature-dir]
  (try
    (let [metadata-file (io/file signature-dir "recording_metadata.edn")]
      (when (.exists metadata-file)
        (let [metadata (edn/read-string (slurp metadata-file))]
          (assoc metadata
                 :directory (.getPath signature-dir)
                 :timestamp (.lastModified signature-dir)))))
    (catch Exception e
      (println "Error extracting metadata from" (.getPath signature-dir) ":" (.getMessage e))
      nil)))

(defn aggregate-band-powers
  "Aggregate band power data from multiple recordings"
  [recordings]
  (try
    (let [band-keys [:delta :theta :alpha :beta :gamma :thinking]

          ; Extract band powers from each recording
          all-band-powers (keep (fn [recording]
                                  (get-in recording [:calibration-index :band-powers]))
                                recordings)

          ; Calculate average for each band
          avg-band-powers (when (seq all-band-powers)
                            (reduce
                             (fn [result band-key]
                               (let [values (keep #(get % band-key) all-band-powers)
                                     avg (when (seq values)
                                           (/ (reduce + values) (count values)))]
                                 (if avg
                                   (assoc result band-key avg)
                                   result)))
                             {}
                             band-keys))

          ; Calculate standard deviation for each band
          std-dev-band-powers (when (seq all-band-powers)
                                (reduce
                                 (fn [result band-key]
                                   (let [values (keep #(get % band-key) all-band-powers)
                                         avg (get avg-band-powers band-key)]
                                     (if (and avg (seq values))
                                       (let [variance (/ (reduce + (map #(Math/pow (- % avg) 2) values))
                                                         (count values))
                                             std-dev (Math/sqrt variance)]
                                         (assoc result band-key std-dev))
                                       result)))
                                 {}
                                 band-keys))]

      {:average avg-band-powers
       :std-dev std-dev-band-powers
       :sample-count (count all-band-powers)})
    (catch Exception e
      (println "Error aggregating band powers:" (.getMessage e))
      {:error (.getMessage e)})))

(defn aggregate-calibration-factors
  "Aggregate calibration factors from multiple recordings"
  [recordings]
  (try
    (let [factor-keys [:delta :theta :alpha :beta :gamma :thinking]

          ; Extract calibration factors from each recording
          all-factors (keep (fn [recording]
                              (get-in recording [:calibration-index :calibration-factors]))
                            recordings)

          ; Calculate average for each factor
          avg-factors (when (seq all-factors)
                        (reduce
                         (fn [result factor-key]
                           (let [values (keep #(get % factor-key) all-factors)
                                 avg (when (seq values)
                                       (/ (reduce + values) (count values)))]
                             (if avg
                               (assoc result factor-key avg)
                               result)))
                         {}
                         factor-keys))]

      {:average avg-factors
       :sample-count (count all-factors)})
    (catch Exception e
      (println "Error aggregating calibration factors:" (.getMessage e))
      {:error (.getMessage e)})))

(defn aggregate-band-distribution
  "Aggregate band distribution data from multiple recordings"
  [recordings]
  (try
    (let [dist-keys [:delta :theta :alpha :beta :gamma :thinking]

          ; Extract band distribution from each recording
          all-distributions (keep (fn [recording]
                                    (get-in recording [:calibration-index :band-distribution]))
                                  recordings)

          ; Calculate average for each distribution value
          avg-distribution (when (seq all-distributions)
                             (reduce
                              (fn [result dist-key]
                                (let [values (keep #(get % dist-key) all-distributions)
                                      avg (when (seq values)
                                            (/ (reduce + values) (count values)))]
                                  (if avg
                                    (assoc result dist-key avg)
                                    result)))
                              {}
                              dist-keys))]

      {:average avg-distribution
       :sample-count (count all-distributions)})
    (catch Exception e
      (println "Error aggregating band distribution:" (.getMessage e))
      {:error (.getMessage e)})))

(defn create-signature-summary
  "Create a summary of signatures for a category"
  [recordings category]
  (try
    (let [; Sort recordings by timestamp descending (newest first)
          sorted-recordings (sort-by #(get % :timestamp 0) > recordings)

          ; Get the 10 most recent recordings
          recent-recordings (take 10 sorted-recordings)

          ; Create summaries
          all-summary {:category category
                       :updated-at (java.util.Date.)
                       :recording-count (count recordings)
                       :band-powers (aggregate-band-powers recordings)
                       :calibration-factors (aggregate-calibration-factors recordings)
                       :band-distribution (aggregate-band-distribution recordings)
                       :device-types (frequencies (map :device-type recordings))
                       :channel-counts (frequencies (map :channel-count recordings))
                       :sampling-rates (frequencies (map :sampling-rate recordings))}

          recent-summary {:category category
                          :updated-at (java.util.Date.)
                          :recording-count (count recent-recordings)
                          :band-powers (aggregate-band-powers recent-recordings)
                          :calibration-factors (aggregate-calibration-factors recent-recordings)
                          :band-distribution (aggregate-band-distribution recent-recordings)
                          :device-types (frequencies (map :device-type recent-recordings))
                          :channel-counts (frequencies (map :channel-count recent-recordings))
                          :sampling-rates (frequencies (map :sampling-rate recent-recordings))}]

      {:all all-summary
       :recent recent-summary})
    (catch Exception e
      (println "Error creating signature summary:" (.getMessage e))
      {:error (.getMessage e)})))

(defn aggregate-signature-type!
  "Aggregate recordings for a specific signature type"
  [profile-name category signature-type]
  (try
    ; Find all recording directories for this signature type
    (let [recording-dirs (find-signature-recording-dirs profile-name category signature-type)]

      (println "Found" (count recording-dirs) "recordings for signature type:" signature-type)

      ; Extract metadata from each directory
      (let [recordings (keep extract-metadata recording-dirs)

            ; Create signature summary for this specific type
            summary (create-signature-summary recordings (str category "/" signature-type))

            ; Define signature directory and file path
            signature-dir (str (fio/get-wave-lexicon-dir profile-name category) "/" signature-type)
            signature-file (str signature-dir "/signature.edn")]

        ; Ensure the signature directory exists
        (fio/ensure-directory! signature-dir)

        ; Save signature summary to file
        (with-open [w (io/writer signature-file)]
          (binding [*out* w]
            (pp/pprint (:all summary))))

        (println "Successfully aggregated" (count recordings)
                 "recordings for signature type:" signature-type)
        summary))
    (catch Exception e
      (println "Error aggregating signature type:" (.getMessage e))
      (.printStackTrace e)
      {:error (.getMessage e)})))


(defn aggregate-category-signatures!
  "Aggregate all signatures for a category and save summary to file"
  [profile-name category]
  (try
    ; Find all signature types in this category
    (let [signature-types (get-signature-types profile-name category)]

      (println "Found" (count signature-types) "signature types for category:" category)

      ; Aggregate each signature type
      (doseq [signature-type signature-types]
        (println "Aggregating signature type:" signature-type)
        (aggregate-signature-type! profile-name category signature-type))

      ; Now create the overall category summaries
      (let [; Collect recordings from all signature types
            all-recordings (mapcat #(keep extract-metadata
                                          (find-signature-recording-dirs profile-name category %))
                                   signature-types)

            ; Create overall summary for the category
            overall-summary (create-signature-summary all-recordings category)

            ; Define output directory and files for overall summaries
            category-dir (fio/get-wave-lexicon-dir profile-name category)
            all-summary-file (io/file category-dir "aggregate_summary.edn")
            recent-summary-file (io/file category-dir "recent_summary.edn")]

        ; Save overall summaries to files
        (fio/ensure-directory! category-dir)

        (with-open [w (io/writer all-summary-file)]
          (binding [*out* w]
            (pp/pprint (:all overall-summary))))

        (with-open [w (io/writer recent-summary-file)]
          (binding [*out* w]
            (pp/pprint (:recent overall-summary))))

        (println "Successfully aggregated" (count all-recordings)
                 "signatures for category" category)

        ; Return the overall summary
        overall-summary))
    (catch Exception e
      (println "Error aggregating category signatures:" (.getMessage e))
      (.printStackTrace e)
      {:error (.getMessage e)})))

(defn aggregate-all-categories!
  "Aggregate signatures for all categories"
  [profile-name]
  (try
    (let [lexicon-dir (fio/get-wave-lexicon-dir profile-name)
          categories (->> (io/file lexicon-dir)
                          .listFiles
                          (filter #(.isDirectory %))
                          (map #(.getName %)))]

      (println "Processing" (count categories) "categories...")

      (doseq [category categories]
        (println "Aggregating category:" category)
        (aggregate-category-signatures! profile-name category))

      (println "Successfully aggregated all categories"))
    (catch Exception e
      (println "Error aggregating all categories:" (.getMessage e))
      {:error (.getMessage e)})))

(defn aggregate-after-recording!
  "Automatically aggregate data for a category after a new recording"
  [profile-name category signature]
  (try
    (println "Auto-aggregating data for category:" category)

    ; First, aggregate the specific signature type
    (println "Aggregating signature type:" signature)
    (aggregate-signature-type! profile-name category signature)

    ; Then use the existing aggregation function for the overall category
    (let [summary (aggregate-category-signatures! profile-name category)]

      ; Also update the category.edn file with stats
      (let [stats (get-category-stats profile-name category)
            category-dir (fio/get-wave-lexicon-dir profile-name category)
            signature-file (io/file category-dir "category.edn")]

        ; Merge the stats with the summary data
        (let [category-data {:stats stats
                             :summary summary
                             :updated-at (java.util.Date.)}]

          ; Save to signature-file
          (with-open [w (io/writer signature-file)]
            (binding [*out* w]
              (pp/pprint category-data)))

          (println "Updated signature-file for" category)
          category-data)))
    (catch Exception e
      (println "Error in auto-aggregation:" (.getMessage e))
      (.printStackTrace e)
      nil)))

(defn aggregate-all-categories-with-stats!
  "Manual aggregation all categories and update signature.edn files"
  [profile-name]
  (try
    (let [lexicon-dir (fio/get-wave-lexicon-dir profile-name)
          categories (->> (io/file lexicon-dir)
                          .listFiles
                          (filter #(.isDirectory %))
                          (map #(.getName %)))]

      (println "Processing" (count categories) "categories with stats...")

      (doseq [category categories]
        (println "Aggregating category with stats:" category)
        (aggregate-after-recording! profile-name category))

      (println "Successfully aggregated all categories with stats"))
    (catch Exception e
      (println "Error aggregating all categories with stats:" (.getMessage e))
      {:error (.getMessage e)})))

; Function to check if a category needs aggregation (useful for periodic checks)
(defn needs-aggregation?
  "Check if a category needs to be aggregated based on timestamps"
  [profile-name category]
  (try
    (let [category-dir (fio/get-wave-lexicon-dir profile-name category)
          signature-file (io/file category-dir "signature.edn")

          ; Get the last aggregation time from signature.edn if it exists
          last-aggregation (when (.exists signature-file)
                             (try
                               (:updated-at (edn/read-string (slurp signature-file)))
                               (catch Exception _ nil)))

          ; Find all signature directories
          signature-dirs (find-signature-recording-dirs profile-name category)

          ; Find the most recent recording
          most-recent (when (seq signature-dirs)
                        (apply max (map #(.lastModified %) signature-dirs)))]

      ; Need aggregation if no previous aggregation or new recordings exist
      (or (nil? last-aggregation)
          (and most-recent (> most-recent (.getTime last-aggregation)))))
    (catch Exception e
      (println "Error checking if category needs aggregation:" (.getMessage e))
      true))) ; Default to true if error, to be safe

; Helper function to ensure all categories are up to date
(defn ensure-all-categories-aggregated!
  "Check all categories and aggregate those that need updating"
  [profile-name]
  (try
    (let [lexicon-dir (fio/get-wave-lexicon-dir profile-name)
          categories (->> (io/file lexicon-dir)
                          .listFiles
                          (filter #(.isDirectory %))
                          (map #(.getName %)))]

      (doseq [category categories]
        (when (needs-aggregation? profile-name category)
          (println "Category" category "needs aggregation, updating...")
          (aggregate-after-recording! profile-name category)))

      (println "All categories are now up to date"))
    (catch Exception e
      (println "Error ensuring all categories are aggregated:" (.getMessage e))
      (.printStackTrace e))))