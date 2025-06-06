(ns floj.categorization
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [floj.io :as fio]
            [zprint.core :as zp]))

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
  "Extract metadata ensuring REAL extracted features are used"
  [recording-dir]
  (try
    (let [metadata-file (str recording-dir "/recording_metadata.edn")
          signature-file (str recording-dir "/signature_features.edn")]
      (when (.exists (clojure.java.io/file metadata-file))
        (let [metadata (edn/read-string (slurp metadata-file))

              signature-features (when (.exists (clojure.java.io/file signature-file))
                                   (try
                                     (edn/read-string (slurp signature-file))
                                     (catch Exception e
                                       (println "Error reading signature features:" (.getMessage e))
                                       nil)))]

          ; Extract raw band powers - prioritize calibration data for aggregation
          (let [real-band-powers (or
                                  ; First priority: calibration band powers (what frontend expects)
                                  (get-in metadata [:calibration-index :band-powers])
                                  ; Second priority: from signature_features.edn calibration context
                                  (get-in signature-features [:calibration-context :band-powers])
                                  ; Third priority: from signature_features.edn signature features
                                  (get-in signature-features [:signature-features :band-powers])
                                  ; Fourth priority: from metadata extracted powers
                                  (when-let [meta-powers (:extracted-band-powers metadata)]
                                    (when (not= meta-powers {:delta 0.2, :theta 0.15, :alpha 0.25, :beta 0.3, :gamma 0.1})
                                      meta-powers)))

                ; Extract band distribution - try multiple sources
                band-distribution (or
                                   ; First try calibration index
                                   (get-in metadata [:calibration-index :band-distribution])
                                   ; Then try signature features calibration context
                                   (get-in signature-features [:calibration-context :band-distribution])
                                   ; Then try to calculate from band powers if we have them
                                   (when real-band-powers
                                     (let [total-power (reduce + (vals real-band-powers))]
                                       (when (> total-power 0)
                                         (into {} (for [[band power] real-band-powers]
                                                    [band (/ power total-power)]))))))]

            (when-not real-band-powers
              (println "WARNING: No real band powers found for" recording-dir))

            ; Build the complete metadata with calibration data
            (cond-> metadata
              ; Use calibration band powers for aggregation
              real-band-powers (assoc :extracted-band-powers real-band-powers)

              ; Include calibration factors for frontend
              true (assoc :calibration-factors
                          (or (get-in metadata [:calibration-index :calibration-factors])
                              (get-in signature-features [:calibration-context :calibration-factors])))

              ; Include band distribution - use calculated or existing
              band-distribution (assoc :band-distribution band-distribution)

              ; Golden tensor reference
              signature-features (assoc :golden-tensor-reference
                                        (or (:golden-tensor signature-features)
                                            (get-in signature-features [:calibration-context :golden-tensor])
                                            (get-in signature-features [:golden-tensor-full :spectral :frequency-domain])
                                            (get-in metadata [:calibration-index :golden-tensor :spectral :frequency-domain])
                                            (get-in metadata [:calibration-index :golden-tensor])))

              ; Triangulation data
              signature-features (assoc :triangulation-data
                                        (:triangulation-data signature-features)))))))
    (catch Exception e
      (println "Error extracting metadata from" recording-dir ":" (.getMessage e))
      nil)))
(defn aggregate-band-powers
  "Aggregate band power data from multiple recordings - using calibration data"
  [recordings]
  (try
    (let [band-keys [:delta :theta :alpha :beta :gamma :thinking]

          ; Extract band powers from calibration data (what frontend expects)
          all-band-powers (keep (fn [recording]
                                  (or (get-in recording [:calibration-index :band-powers])
                                      (:extracted-band-powers recording)))
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
                              (or (get-in recording [:calibration-index :calibration-factors])
                                  (:calibration-factors recording)))
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
                                    (or (get-in recording [:calibration-index :band-distribution])
                                        (:band-distribution recording)))
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

(defn aggregate-channel-counts
  "Aggregate channel count data from multiple recordings"
  [recordings]
  (try
    (let [channel-counts (keep (fn [recording]
                                 (or (:channel-count recording)
                                     (:channels recording)
                                     (get-in recording [:metadata :channel-count])
                                     4))
                               recordings)]
      (frequencies channel-counts))
    (catch Exception e
      (println "Error aggregating channel counts:" (.getMessage e))
      {})))

(defn extract-calibration-data-from-metadata
  "Extract all calibration-related data from recording metadata"
  [metadata]
  (let [calibration-index (get metadata :calibration-index)]
    {:band-powers (get calibration-index :band-powers)
     :calibration-factors (get calibration-index :calibration-factors)
     :golden-tensor (get calibration-index :golden-tensor)
     :band-distribution (get calibration-index :band-distribution)}))

(defn create-signature-summary
  "Create signature summary using calibration data (frontend format)"
  [recordings signature-name]
  (try
    (when (seq recordings)
      (let [; Filter recordings to use for aggregation
            valid-recordings (filter #(get-in % [:metadata :include-in-aggregation] true) recordings)

            _ (println "Creating summary from" (count valid-recordings) "recordings")

            ; Use calibration data for aggregation (what frontend expects)
            band-powers-stats (aggregate-band-powers valid-recordings)
            calibration-factors-stats (aggregate-calibration-factors valid-recordings)
            band-distribution-stats (aggregate-band-distribution valid-recordings)

            ; Aggregate metadata
            device-types (frequencies (keep #(or (:device-type %)
                                                 (get-in % [:metadata :device-type])
                                                 "GANGLION_BOARD") valid-recordings))
            sampling-rates (frequencies (keep #(or (:sampling-rate %)
                                                   (get-in % [:metadata :sampling-rate])
                                                   200) valid-recordings))
            channel-counts (aggregate-channel-counts valid-recordings)

            ; Create summary in exact frontend format
            summary {:updated-at (java.util.Date.)
                     :category signature-name
                     :recording-count (count valid-recordings)
                     :device-types device-types
                     :sampling-rates sampling-rates
                     :channel-counts channel-counts
                     :band-powers band-powers-stats
                     :calibration-factors calibration-factors-stats
                     :band-distribution band-distribution-stats}

            ; Create recent summary (last n recordings)
            recent-recordings (take-last 10 valid-recordings)
            recent-band-powers (aggregate-band-powers recent-recordings)
            recent-calibration-factors (aggregate-calibration-factors recent-recordings)
            recent-band-distribution (aggregate-band-distribution recent-recordings)
            recent-device-types (frequencies (keep #(or (:device-type %)
                                                        (get-in % [:metadata :device-type])
                                                        "GANGLION_BOARD") recent-recordings))
            recent-sampling-rates (frequencies (keep #(or (:sampling-rate %)
                                                          (get-in % [:metadata :sampling-rate])
                                                          200) recent-recordings))
            recent-channel-counts (aggregate-channel-counts recent-recordings)

            recent-summary {:updated-at (java.util.Date.)
                            :category signature-name
                            :recording-count (count recent-recordings)
                            :device-types recent-device-types
                            :sampling-rates recent-sampling-rates
                            :channel-counts recent-channel-counts
                            :band-powers recent-band-powers
                            :calibration-factors recent-calibration-factors
                            :band-distribution recent-band-distribution}]
        {:all summary
         :recent recent-summary}))

    (catch Exception e
      (println "Error creating signature summary:" (.getMessage e))
      (.printStackTrace e)
      {:error (.getMessage e)})))



(defn aggregate-signature-type!
  "Aggregate signature recordings using calibration data"
  [profile-name category signature-type]
  (try
    (let [recording-dirs (find-signature-recording-dirs profile-name category signature-type)]
      (println "Found" (count recording-dirs) "recordings for signature type:" signature-type)

      ; Extract metadata from each recording directory
      (let [recordings (keep extract-metadata recording-dirs)
            _ (println "Extracted metadata from" (count recordings) "recordings")

            ; Create signature summary using calibration data
            summary (create-signature-summary recordings (str category "/" signature-type))

            ; Define signature directory and file path
            signature-dir (str (fio/get-wave-lexicon-dir profile-name category) "/" signature-type)
            signature-file (str signature-dir "/signature.edn")]

        ; Ensure the signature directory exists
        (fio/ensure-directory! signature-dir)

        ; Save signature summary to file in frontend format
        (with-open [w (io/writer signature-file)]
          (binding [*print-length* nil *print-level* nil]
            (clojure.pprint/pprint (:all summary) w)))

        (println "Successfully aggregated" (count recording-dirs)
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

        (when (:all overall-summary)
          (with-open [w (io/writer all-summary-file)]
            (binding [*out* w]
              (pp/pprint (:all overall-summary)))))

        (when (:recent overall-summary)
          (with-open [w (io/writer recent-summary-file)]
            (binding [*out* w]
              (pp/pprint (:recent overall-summary)))))

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
            category-file (io/file category-dir "category.edn")]

        ; Merge the stats with the summary data
        (let [category-data {:stats stats
                             :summary summary
                             :updated-at (java.util.Date.)}]

          ; Save to category-file
          (with-open [w (io/writer category-file)]
            (binding [*out* w]
              (pp/pprint category-data)))

          (println "Updated category.edn for" category)
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
  [profile-name signature]
  (try
    (let [lexicon-dir (fio/get-wave-lexicon-dir profile-name)
          categories (->> (io/file lexicon-dir)
                          .listFiles
                          (filter #(.isDirectory %))
                          (map #(.getName %)))]

      (doseq [category categories]
        (when (needs-aggregation? profile-name category)
          (println "Category" category "needs aggregation, updating...")
          (aggregate-after-recording! profile-name category signature)))

      (println "All categories are now up to date"))
    (catch Exception e
      (println "Error ensuring all categories are aggregated:" (.getMessage e))
      (.printStackTrace e))))