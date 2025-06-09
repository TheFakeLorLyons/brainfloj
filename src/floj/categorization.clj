(ns floj.categorization
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [floj.io :as fio]
            [zprint.core :as zp]))

(def category
  {:metadata {:version "1"
              :category-name nil
              :refinement "light"
              :last-updated nil}

   ; Statistical foundation (existing stats)
   :statistical-foundation {:stats nil :summary nil}

   ; Adaptive thresholding using triangulation data
   :adaptive-thresholds
   {:dynamic-thresholds {:up 0.02 :down 0.02}        ; Adaptive based on separation quality
    :confidence-multipliers {:up 3.0 :down 3.0}      ; Boosted by triangulation strength
    :separation-bonus 0.5                            ; Reward for clear separation
    :temporal-stability-bonus 0.4                    ; Stable triangulation patterns
    :momentum-boost 0.25                             ; Trend amplification
    :triangulation-bonus 0.3}                        ; Leverage triangulation data

   ; Feature weighting incorporating signature_features.edn data
   :feature-weights
   {:golden-tensor 0.35             ; From calibration-context
    :triangulation-data 0.25        ; From triangulation-data section  
    :band-powers 0.20               ; Traditional band powers
    :calibration-factors 0.15       ; From calibration-context
    :spectral-features 0.05}        ; From power-distribution

   ; Performance tracking via triangulation metrics
   :performance-tracking
   {:triangulation-quality 0.0      ; Average triangulation strength
    :separation-score 0.0           ; How well up/down separate
    :stability-score 0.0            ; Consistency across recordings
    :confidence-momentum {:up 0.15 :down 0.12}
    :adaptation-rate 0.18
    :recording-count 0}

   ; Signature quality assessment
   :signature-quality
   {:discriminability-score 0.0     ; How distinguishable signatures are
    :stability-score 0.0            ; How consistent they are
    :triangulation-confidence 0.0   ; Average triangulation confidence
    :overall-grade :needs-training}

   ; Real-time processing configuration tailored for frontend
   :realtime-config
   {:confidence-threshold 0.02      ; Dynamic threshold
    :boost-factor 3.0               ; Confidence multiplier
    :feature-weights {:golden-tensor 0.35 :triangulation-data 0.25
                      :band-powers 0.20 :calibration-factors 0.15}
    :stability-bonus 0.4
    :separation-bonus 0.5
    :triangulation-bonus 0.3}})


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
  "Enhanced metadata extraction to capture all signature_features.edn data"
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

          ; Extract data from signature_features.edn
          (let [real-band-powers (or
                                  (get-in metadata [:calibration-index :band-powers])
                                  (get-in signature-features [:calibration-context :band-powers])
                                  (get-in signature-features [:signature-features :band-powers])
                                  (when-let [meta-powers (:extracted-band-powers metadata)]
                                    (when (not= meta-powers {:delta 0.2, :theta 0.15, :alpha 0.25, :beta 0.3, :gamma 0.1})
                                      meta-powers)))

                band-distribution (or
                                   (get-in metadata [:calibration-index :band-distribution])
                                   (get-in signature-features [:calibration-context :band-distribution])
                                   (get-in signature-features [:signature-features :power-distribution])
                                   (when real-band-powers
                                     (let [total-power (reduce + (vals real-band-powers))]
                                       (when (> total-power 0)
                                         (into {} (for [[band power] real-band-powers]
                                                    [band (/ power total-power)]))))))

                ; Extract triangulation data with proper structure
                triangulation-data (get-in signature-features [:signature-features :triangulation-data])
                calibration-comparison (get-in signature-features [:signature-features :calibration-comparison])

                ; Extract pattern recognition data
                pattern-recognition (get-in signature-features [:signature-features :triangulation-data :pattern-recognition])

                ; Extract channel statistics
                channel-stats (get-in signature-features [:signature-features :channel-statistics])

                ; Extract signature strength and confidence
                signature-strength (get-in signature-features [:signature-features :signature-strength])

                ; Extract golden tensor ratios
                golden-tensor-ratios (get-in triangulation-data [:golden-tensor-ratios])
                calibration-ratios (get-in triangulation-data [:calibration-ratios])
                signature-ratios (get-in triangulation-data [:signature-ratios])

                ; Extract triangulated strength metrics
                triangulated-strength (get-in triangulation-data [:triangulated-strength])]

            ; Build enhanced metadata with all available data
            (cond-> metadata
              real-band-powers (assoc :extracted-band-powers real-band-powers)
              band-distribution (assoc :band-distribution band-distribution)

              ; Store triangulation data at root level for easy access
              triangulation-data (assoc :triangulation-data triangulation-data)
              calibration-comparison (assoc :calibration-comparison calibration-comparison)

              ; Store individual components for backward compatibility (will get rid of this soon)
              pattern-recognition (assoc :pattern-recognition pattern-recognition)
              channel-stats (assoc :channel-statistics channel-stats)
              signature-strength (assoc :signature-strength signature-strength)
              golden-tensor-ratios (assoc :golden-tensor-ratios golden-tensor-ratios)
              calibration-ratios (assoc :calibration-ratios calibration-ratios)
              signature-ratios (assoc :signature-ratios signature-ratios)
              triangulated-strength (assoc :triangulated-strength triangulated-strength)

              ; Enhanced calibration factors
              true (assoc :calibration-factors
                          (or (get-in metadata [:calibration-index :calibration-factors])
                              (get-in signature-features [:calibration-context :calibration-factors])))

              ; Enhanced golden tensor reference
              signature-features (assoc :golden-tensor-reference
                                        (or (:golden-tensor signature-features)
                                            (get-in signature-features [:calibration-context :golden-tensor])
                                            (get-in signature-features [:golden-tensor-full :spectral :frequency-domain])
                                            (get-in metadata [:calibration-index :golden-tensor :spectral :frequency-domain])
                                            (get-in metadata [:calibration-index :golden-tensor])))

              ; Store the full signature features for reference
              signature-features (assoc :signature-features signature-features))))))
    (catch Exception e
      (println "Error extracting metadata from" recording-dir ":" (.getMessage e))
      nil)))


(defn aggregate-band-powers
  "Streamlined band power aggregation using metadata"
  [recordings]
  (let [band-keys [:delta :theta :alpha :beta :gamma :thinking]
        all-band-powers (keep :extracted-band-powers recordings)]

    (when (seq all-band-powers)
      {:average (into {} (for [band band-keys]
                           [band (let [values (keep #(get % band) all-band-powers)]
                                   (when (seq values) (/ (reduce + values) (count values))))]))
       :std-dev (into {} (for [band band-keys]
                           [band (let [values (keep #(get % band) all-band-powers)
                                       avg (when (seq values) (/ (reduce + values) (count values)))]
                                   (when (and avg (seq values))
                                     (Math/sqrt (/ (reduce + (map #(* (- % avg) (- % avg)) values)) (count values)))))]))
       :sample-count (count all-band-powers)})))

(defn aggregate-calibration-factors
  "Streamlined calibration factor aggregation"
  [recordings]
  (let [factor-keys [:delta :theta :alpha :beta :gamma :thinking]
        all-factors (keep :calibration-factors recordings)]

    (when (seq all-factors)
      {:average (into {} (for [factor factor-keys]
                           [factor (let [values (keep #(get % factor) all-factors)]
                                     (when (seq values) (/ (reduce + values) (count values))))]))
       :sample-count (count all-factors)})))

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

(defn calculate-metrics
  "Calculate intelligence metrics from metadata"
  [recordings]
  (try
    (let [recordings-with-features (filter #(or (get % :triangulation-data)
                                                (get % :signature-strength)
                                                (get-in % [:signature-features :signature-features])) recordings)

          ; Extract triangulation strengths - check multiple locations
          triangulation-strengths (keep #(or (get-in % [:triangulated-strength :vs-golden-tensor])
                                             (get-in % [:triangulation-data :triangulated-strength :vs-golden-tensor])
                                             (get-in % [:signature-features :signature-features :triangulation-data :triangulated-strength :vs-golden-tensor]))
                                        recordings-with-features)

          ; Calculate separation quality by comparing signature strengths
          signature-strengths (keep #(or (get % :signature-strength)
                                         (get-in % [:signature-features :signature-features :signature-strength]))
                                    recordings-with-features)

          ; Stability from band power consistency
          band-power-stabilities (for [recording recordings-with-features]
                                   (let [bp (get recording :extracted-band-powers)]
                                     (when bp
                                       (let [values (vals bp)
                                             mean (/ (reduce + values) (count values))
                                             variance (/ (reduce + (map #(* (- % mean) (- % mean)) values)) (count values))]
                                         (if (> mean 0) (/ (Math/sqrt variance) mean) 1.0)))))]

      {:triangulation-quality (if (seq triangulation-strengths)
                                (/ (reduce + triangulation-strengths) (count triangulation-strengths))
                                0.0)
       :separation-score (if (seq signature-strengths)
                           (let [max-strength (apply max signature-strengths)
                                 min-strength (apply min signature-strengths)]
                             (if (> max-strength 0) (/ (- max-strength min-strength) max-strength) 0.0))
                           0.0)
       :stability-score (if (seq (remove nil? band-power-stabilities))
                          (- 1.0 (/ (reduce + (remove nil? band-power-stabilities))
                                    (count (remove nil? band-power-stabilities))))
                          0.5)
       :feature-recordings-count (count recordings-with-features)})
    (catch Exception e
      (println "Error calculating intelligence metrics:" (.getMessage e))
      {:triangulation-quality 0.0 :separation-score 0.0 :stability-score 0.5 :feature-recordings-count 0})))

(defn aggregate-triangulation-data
  "Aggregate triangulation data in signature_features.edn"
  [recordings]
  (try
    (let [triangulation-data (keep #(or (get % :triangulation-data)
                                        (get-in % [:signature-features :signature-features :triangulation-data]))
                                   recordings)]
      (when (seq triangulation-data)
        (let [golden-tensor-ratios (keep :golden-tensor-ratios triangulation-data)
              calibration-ratios (keep :calibration-ratios triangulation-data)
              signature-ratios (keep :signature-ratios triangulation-data)
              triangulated-strengths (keep :triangulated-strength triangulation-data)
              pattern-recognitions (keep :pattern-recognition triangulation-data)

              bands [:delta :theta :alpha :beta :thinking :gamma]]

          {:golden-tensor-ratios
           (when (seq golden-tensor-ratios)
             (into {} (for [band bands]
                        [band (let [values (keep #(get % band) golden-tensor-ratios)]
                                (when (seq values)
                                  {:average (/ (reduce + values) (count values))
                                   :std-dev (let [avg (/ (reduce + values) (count values))
                                                  variance (/ (reduce + (map #(* (- % avg) (- % avg)) values)) (count values))]
                                              (Math/sqrt variance))
                                   :min (apply min values)
                                   :max (apply max values)
                                   :sample-count (count values)}))])))

           :calibration-ratios
           (when (seq calibration-ratios)
             (into {} (for [band bands]
                        [band (let [values (keep #(get % band) calibration-ratios)]
                                (when (seq values)
                                  {:average (/ (reduce + values) (count values))
                                   :std-dev (let [avg (/ (reduce + values) (count values))
                                                  variance (/ (reduce + (map #(* (- % avg) (- % avg)) values)) (count values))]
                                              (Math/sqrt variance))
                                   :min (apply min values)
                                   :max (apply max values)
                                   :sample-count (count values)}))])))

           :signature-ratios
           (when (seq signature-ratios)
             (into {} (for [band bands]
                        [band (let [values (keep #(get % band) signature-ratios)]
                                (when (seq values)
                                  {:average (/ (reduce + values) (count values))
                                   :std-dev (let [avg (/ (reduce + values) (count values))
                                                  variance (/ (reduce + (map #(* (- % avg) (- % avg)) values)) (count values))]
                                              (Math/sqrt variance))
                                   :min (apply min values)
                                   :max (apply max values)
                                   :sample-count (count values)}))])))

           :triangulated-strength
           (when (seq triangulated-strengths)
             (let [#_#_live-intensities (keep :live-intensity triangulated-strengths)
                   vs-golden-tensor (keep :vs-golden-tensor triangulated-strengths)
                   vs-calibration (keep :vs-calibration triangulated-strengths)
                   vs-signature (keep :vs-signature triangulated-strengths)]
               {:live-intensity {:average (when (seq live-intensities) (/ (reduce + live-intensities) (count live-intensities)))
                                     #_#_:std-dev (when (seq live-intensities)
                                                (let [avg (/ (reduce + live-intensities) (count live-intensities))
                                                      variance (/ (reduce + (map #(* (- % avg) (- % avg)) live-intensities)) (count live-intensities))]
                                                  (Math/sqrt variance)))}
                :vs-golden-tensor {:average (when (seq vs-golden-tensor) (/ (reduce + vs-golden-tensor) (count vs-golden-tensor)))
                                   #_#_:std-dev (when (seq vs-golden-tensor)
                                              (let [avg (/ (reduce + vs-golden-tensor) (count vs-golden-tensor))
                                                    variance (/ (reduce + (map #(* (- % avg) (- % avg)) vs-golden-tensor)) (count vs-golden-tensor))]
                                                (Math/sqrt variance)))}
                :vs-calibration {:average (when (seq vs-calibration) (/ (reduce + vs-calibration) (count vs-calibration)))
                                 :std-dev (when (seq vs-calibration)
                                            (let [avg (/ (reduce + vs-calibration) (count vs-calibration))
                                                  variance (/ (reduce + (map #(* (- % avg) (- % avg)) vs-calibration)) (count vs-calibration))]
                                              (Math/sqrt variance)))}
                :vs-signature {:average (when (seq vs-signature) (/ (reduce + vs-signature) (count vs-signature)))
                               :std-dev (when (seq vs-signature)
                                          (let [avg (/ (reduce + vs-signature) (count vs-signature))
                                                variance (/ (reduce + (map #(* (- % avg) (- % avg)) vs-signature)) (count vs-signature))]
                                            (Math/sqrt variance)))}}))

           :pattern-recognition
           (when (seq pattern-recognitions)
             (let [existing-signatures (map :has-existing-signature pattern-recognitions)
                   signature-confidences (keep :signature-confidence pattern-recognitions)
                   novelty-scores (keep :novelty-score pattern-recognitions)]
               {:has-existing-signature-ratio (if (seq existing-signatures)
                                                (/ (count (filter true? existing-signatures)) (count existing-signatures))
                                                0.0)
                :signature-confidence (when (seq signature-confidences)
                                        {:average-similarity (let [similarities (keep :average-similarity signature-confidences)]
                                                               (when (seq similarities) (/ (reduce + similarities) (count similarities))))
                                         :pattern-variance (let [variances (keep :pattern-variance signature-confidences)]
                                                             (when (seq variances) (/ (reduce + variances) (count variances))))})
                :novelty-score {:average (when (seq novelty-scores) (/ (reduce + novelty-scores) (count novelty-scores)))
                                :std-dev (when (seq novelty-scores)
                                           (let [avg (/ (reduce + novelty-scores) (count novelty-scores))
                                                 variance (/ (reduce + (map #(* (- % avg) (- % avg)) novelty-scores)) (count novelty-scores))]
                                             (Math/sqrt variance)))}}))

           :sample-count (count triangulation-data)})))
    (catch Exception e
      (println "Error aggregating triangulation data:" (.getMessage e))
      {:error (.getMessage e)})))

(defn aggregate-features
  "Aggregate features from signature_features.edn files"
  [recordings]
  (try
    (let [recordings-with-features (filter #(get-in % [:signature-features :signature-features]) recordings)

          ; Aggregate triangulation data
          triangulation-stats
          (when (seq recordings-with-features)
            (let [triangulation-data (keep #(get-in % [:signature-features :triangulation-data]) recordings-with-features)
                  golden-tensor-ratios (keep :golden-tensor-ratios triangulation-data)
                  calibration-ratios (keep :calibration-ratios triangulation-data)]

              {:golden-tensor-ratios
               (when (seq golden-tensor-ratios)
                 (let [bands [:delta :theta :alpha :beta :thinking :gamma]]
                   (into {} (for [band bands]
                              [band (let [values (keep #(get % band) golden-tensor-ratios)]
                                      (when (seq values)
                                        {:average (/ (reduce + values) (count values))
                                         :std-dev (let [avg (/ (reduce + values) (count values))
                                                        variance (/ (reduce + (map #(* (- % avg) (- % avg)) values)) (count values))]
                                                    (Math/sqrt variance))}))]))))

               :calibration-ratios
               (when (seq calibration-ratios)
                 (let [bands [:delta :theta :alpha :beta :thinking :gamma]]
                   (into {} (for [band bands]
                              [band (let [values (keep #(get % band) calibration-ratios)]
                                      (when (seq values)
                                        {:average (/ (reduce + values) (count values))
                                         :std-dev (let [avg (/ (reduce + values) (count values))
                                                        variance (/ (reduce + (map #(* (- % avg) (- % avg)) values)) (count values))]
                                                    (Math/sqrt variance))}))]))))

               :sample-count (count triangulation-data)}))

          ; Aggregate signature strengths
          signature-strength-stats
          (let [strengths (keep #(get-in % [:signature-features :signature-features :signature-strength]) recordings-with-features)]
            (when (seq strengths)
              {:average (/ (reduce + strengths) (count strengths))
               :std-dev (let [avg (/ (reduce + strengths) (count strengths))
                              variance (/ (reduce + (map #(* (- % avg) (- % avg)) strengths)) (count strengths))]
                          (Math/sqrt variance))
               :min (apply min strengths)
               :max (apply max strengths)
               :sample-count (count strengths)}))]

      {:triangulation-stats triangulation-stats
       :signature-strength-stats signature-strength-stats
       :feature-sample-count (count recordings-with-features)})
    (catch Exception e
      (println "Error aggregating features:" (.getMessage e))
      {:feature-sample-count 0})))

(defn aggregate-triangulation-data
  "Aggregate triangulation data across recordings"
  [recordings]
  (try
    (let [triangulation-data (keep :triangulation-data recordings)]
      (when (seq triangulation-data)
        (let [golden-tensor-ratios (keep :golden-tensor-ratios triangulation-data)
              calibration-ratios (keep :calibration-ratios triangulation-data)
              signature-ratios (keep :signature-ratios triangulation-data)
              triangulated-strengths (keep :triangulated-strength triangulation-data)
              pattern-recognitions (keep :pattern-recognition triangulation-data)

              bands [:delta :theta :alpha :beta :thinking :gamma]]

          {:golden-tensor-ratios
           (when (seq golden-tensor-ratios)
             (into {} (for [band bands]
                        [band (let [values (keep #(get % band) golden-tensor-ratios)]
                                (when (seq values)
                                  {:average (/ (reduce + values) (count values))
                                   :std-dev (let [avg (/ (reduce + values) (count values))
                                                  variance (/ (reduce + (map #(* (- % avg) (- % avg)) values)) (count values))]
                                              (Math/sqrt variance))
                                   :min (apply min values)
                                   :max (apply max values)
                                   :sample-count (count values)}))])))

           :calibration-ratios
           (when (seq calibration-ratios)
             (into {} (for [band bands]
                        [band (let [values (keep #(get % band) calibration-ratios)]
                                (when (seq values)
                                  {:average (/ (reduce + values) (count values))
                                   :std-dev (let [avg (/ (reduce + values) (count values))
                                                  variance (/ (reduce + (map #(* (- % avg) (- % avg)) values)) (count values))]
                                              (Math/sqrt variance))
                                   :min (apply min values)
                                   :max (apply max values)
                                   :sample-count (count values)}))])))

           :signature-ratios
           (when (seq signature-ratios)
             (into {} (for [band bands]
                        [band (let [values (keep #(get % band) signature-ratios)]
                                (when (seq values)
                                  {:average (/ (reduce + values) (count values))
                                   :std-dev (let [avg (/ (reduce + values) (count values))
                                                  variance (/ (reduce + (map #(* (- % avg) (- % avg)) values)) (count values))]
                                              (Math/sqrt variance))
                                   :min (apply min values)
                                   :max (apply max values)
                                   :sample-count (count values)}))])))

           :triangulated-strength
           (when (seq triangulated-strengths)
             (let [live-intensities (keep :live-intensity triangulated-strengths)
                   vs-golden-tensor (keep :vs-golden-tensor triangulated-strengths)
                   vs-calibration (keep :vs-calibration triangulated-strengths)
                   vs-signature (keep :vs-signature triangulated-strengths)]
               {:live-intensity {:average (when (seq live-intensities) (/ (reduce + live-intensities) (count live-intensities)))
                                 :std-dev (when (seq live-intensities)
                                            (let [avg (/ (reduce + live-intensities) (count live-intensities))
                                                  variance (/ (reduce + (map #(* (- % avg) (- % avg)) live-intensities)) (count live-intensities))]
                                              (Math/sqrt variance)))}
                :vs-golden-tensor {:average (when (seq vs-golden-tensor) (/ (reduce + vs-golden-tensor) (count vs-golden-tensor)))
                                   :std-dev (when (seq vs-golden-tensor)
                                              (let [avg (/ (reduce + vs-golden-tensor) (count vs-golden-tensor))
                                                    variance (/ (reduce + (map #(* (- % avg) (- % avg)) vs-golden-tensor)) (count vs-golden-tensor))]
                                                (Math/sqrt variance)))}
                :vs-calibration {:average (when (seq vs-calibration) (/ (reduce + vs-calibration) (count vs-calibration)))
                                 :std-dev (when (seq vs-calibration)
                                            (let [avg (/ (reduce + vs-calibration) (count vs-calibration))
                                                  variance (/ (reduce + (map #(* (- % avg) (- % avg)) vs-calibration)) (count vs-calibration))]
                                              (Math/sqrt variance)))}
                :vs-signature {:average (when (seq vs-signature) (/ (reduce + vs-signature) (count vs-signature)))
                               :std-dev (when (seq vs-signature)
                                          (let [avg (/ (reduce + vs-signature) (count vs-signature))
                                                variance (/ (reduce + (map #(* (- % avg) (- % avg)) vs-signature)) (count vs-signature))]
                                            (Math/sqrt variance)))}}))

           :pattern-recognition
           (when (seq pattern-recognitions)
             (let [existing-signatures (map :has-existing-signature pattern-recognitions)
                   signature-confidences (keep :signature-confidence pattern-recognitions)
                   novelty-scores (keep :novelty-score pattern-recognitions)]
               {:has-existing-signature-ratio (if (seq existing-signatures)
                                                (/ (count (filter true? existing-signatures)) (count existing-signatures))
                                                0.0)
                :signature-confidence (when (seq signature-confidences)
                                        {:average-similarity (let [similarities (keep :average-similarity signature-confidences)]
                                                               (when (seq similarities) (/ (reduce + similarities) (count similarities))))
                                         :pattern-variance (let [variances (keep :pattern-variance signature-confidences)]
                                                             (when (seq variances) (/ (reduce + variances) (count variances))))})
                :novelty-score {:average (when (seq novelty-scores) (/ (reduce + novelty-scores) (count novelty-scores)))
                                :std-dev (when (seq novelty-scores)
                                           (let [avg (/ (reduce + novelty-scores) (count novelty-scores))
                                                 variance (/ (reduce + (map #(* (- % avg) (- % avg)) novelty-scores)) (count novelty-scores))]
                                             (Math/sqrt variance)))}}))

           :sample-count (count triangulation-data)})))
    (catch Exception e
      (println "Error aggregating triangulation data:" (.getMessage e))
      {:error (.getMessage e)})))
(defn create-signature-summary
  "Create signature summary with enhanced intelligence metrics - FIXED VERSION"
  [recordings signature-name]
  (try
    (when (seq recordings)
      (let [processed-recordings (if (string? (first recordings))
                                   (keep extract-metadata recordings)
                                   recordings)
            valid-recordings (filter #(get-in % [:metadata :include-in-aggregation] true) processed-recordings)
            _ (println "Creating summary from" (count valid-recordings) "recordings")

            band-powers-stats (aggregate-band-powers valid-recordings)
            calibration-factors-stats (aggregate-calibration-factors valid-recordings)
            band-distribution-stats (aggregate-band-distribution valid-recordings)

            device-types (frequencies (keep #(or (:device-type %)
                                                 (get-in % [:metadata :device-type])
                                                 "GANGLION_BOARD") valid-recordings))
            sampling-rates (frequencies (keep #(or (:sampling-rate %)
                                                   (get-in % [:metadata :sampling-rate])
                                                   200) valid-recordings))
            channel-counts (aggregate-channel-counts valid-recordings)

            ; Aggregations using fixed functions
            triangulation-stats (aggregate-triangulation-data valid-recordings)

            ; Intelligence metrics calculation
            intelligence-metrics (calculate-metrics valid-recordings)

            ; Create the summary
            summary {:updated-at (java.util.Date.)
                     :category signature-name
                     :recording-count (count valid-recordings)
                     :device-types device-types
                     :sampling-rates sampling-rates
                     :channel-counts channel-counts
                     :band-powers band-powers-stats
                     :calibration-factors calibration-factors-stats
                     :band-distribution band-distribution-stats
                     :intelligence-metrics intelligence-metrics
                     :triangulation-analysis triangulation-stats

                     :signature-quality
                     {:discriminability-score (:separation-score intelligence-metrics)
                      :stability-score (:stability-score intelligence-metrics)
                      :triangulation-confidence (:triangulation-quality intelligence-metrics)
                      :overall-grade (cond
                                       (> (:separation-score intelligence-metrics) 0.8) :excellent
                                       (> (:separation-score intelligence-metrics) 0.6) :good
                                       (> (:separation-score intelligence-metrics) 0.4) :moderate
                                       (< (:separation-score intelligence-metrics) 0.3) :poor
                                       :else :needs-training)}}

            recent-recordings (take-last 10 valid-recordings)
            recent-summary (when (seq recent-recordings)
                             (let [recent-band-powers (aggregate-band-powers recent-recordings)
                                   recent-calibration-factors (aggregate-calibration-factors recent-recordings)
                                   recent-band-distribution (aggregate-band-distribution recent-recordings)
                                   recent-triangulation-stats (aggregate-triangulation-data recent-recordings)
                                   recent-intelligence-metrics (calculate-metrics recent-recordings)]

                               {:updated-at (java.util.Date.)
                                :category signature-name
                                :recording-count (count recent-recordings)

                                :device-types (frequencies (keep #(or (:device-type %)
                                                                      (get-in % [:metadata :device-type])
                                                                      "GANGLION_BOARD") recent-recordings))
                                :sampling-rates (frequencies (keep #(or (:sampling-rate %)
                                                                        (get-in % [:metadata :sampling-rate])
                                                                        200) recent-recordings))
                                :channel-counts (aggregate-channel-counts recent-recordings)
                                :band-powers recent-band-powers
                                :calibration-factors recent-calibration-factors
                                :band-distribution recent-band-distribution
                                :intelligence-metrics recent-intelligence-metrics
                                :triangulation-analysis recent-triangulation-stats

                                :signature-quality
                                {:discriminability-score (:separation-score recent-intelligence-metrics)
                                 :stability-score (:stability-score recent-intelligence-metrics)
                                 :triangulation-confidence (:triangulation-quality recent-intelligence-metrics)
                                 :overall-grade (cond
                                                  (> (:separation-score recent-intelligence-metrics) 0.8) :excellent
                                                  (> (:separation-score recent-intelligence-metrics) 0.6) :good
                                                  (> (:separation-score recent-intelligence-metrics) 0.4) :moderate
                                                  (< (:separation-score recent-intelligence-metrics) 0.3) :poor
                                                  :else :needs-training)}}))]

        ; Return both all and recent summaries
        {:all summary
         :recent recent-summary}))
    (catch Exception e
      (println "Error creating signature summary:" (.getMessage e))
      (.printStackTrace e)
      {:error (.getMessage e)})))

(defn create-category-intelligence
  "Comprehensive category intelligence that preserves all functionality"
  [profile-name category-name signature-summaries existing-category-data]
  (try
    (let [; Extract intelligence data from signature summaries (supporting both data paths)
          up-intelligence (or (get-in signature-summaries [:up :intelligence-metrics])
                              (get-in signature-summaries [:up :all :intelligence-metrics]))
          down-intelligence (or (get-in signature-summaries [:down :intelligence-metrics])
                                (get-in signature-summaries [:down :all :intelligence-metrics]))

          ; Calculate comprehensive metrics with fallbacks
          overall-separation (max (or (:separation-quality up-intelligence)
                                      (:separation-score up-intelligence) 0.3)
                                  (or (:separation-quality down-intelligence)
                                      (:separation-score down-intelligence) 0.3))
          overall-stability (max (or (:stability-score up-intelligence) 0.5)
                                 (or (:stability-score down-intelligence) 0.5))
          overall-triangulation (max (or (:triangulation-quality up-intelligence) 0.0)
                                     (or (:triangulation-quality down-intelligence) 0.0))

          ; Performance metrics from existing data
          recording-count (or (get-in existing-category-data [:summary :all :recording-count])
                              (+ (get-in signature-summaries [:up :all :recording-count] 0)
                                 (get-in signature-summaries [:down :all :recording-count] 0))
                              0)
          performance-factor (if (> recording-count 5) 0.8 0.6)

          ; Calculate dynamic thresholds (preserving both approaches)
          base-threshold (if (> overall-separation 0.6) 0.02 0.05)
          quality-adjustment (* (- 1.0 overall-separation) 0.03)
          stability-adjustment (* (- 1.0 overall-stability) 0.02)
          performance-adjustment (if (> performance-factor 0.8) -0.01 0.01)

          final-threshold (max 0.005 (+ base-threshold quality-adjustment
                                        stability-adjustment performance-adjustment))

          ; Confidence boost calculation
          confidence-boost (+ 2.0 (* overall-separation 2.0))

          category {:metadata {:version "1"
                               :category-name category-name
                               :refinement "light"
                               :last-updated (java.util.Date.)}

                    ; Statistical foundation (preserving both approaches)
                    :statistical-foundation (merge (:stats existing-category-data)
                                                   (:summary existing-category-data)
                                                   {:stats existing-category-data
                                                    :signature-summaries signature-summaries})

                    ; Thresholding
                    :adaptive-thresholds
                    {:dynamic-thresholds {:up final-threshold :down final-threshold}
                     :confidence-multipliers {:up confidence-boost :down confidence-boost}
                     :separation-bonus (* overall-separation 0.5)
                     :temporal-stability-bonus (* overall-stability 0.4)
                     :triangulation-bonus (* overall-triangulation 0.3)
                     :momentum-boost 0.25}

                    ; Feature weighting based on triangulation data
                    :feature-weights
                    {:golden-tensor 0.35
                     :triangulation-data 0.25
                     :band-powers 0.20
                     :calibration-factors 0.15
                     :spectral-features 0.05}

                    ; Comprehensive performance tracking
                    :performance-tracking
                    {:recording-count recording-count
                     :triangulation-quality overall-triangulation
                     :separation-score overall-separation
                     :stability-score overall-stability
                     :confidence-momentum {:up 0.15 :down 0.12}
                     :adaptation-rate 0.18}

                    ; Quality assessment
                    :signature-quality
                    {:discriminability-score overall-separation
                     :stability-score overall-stability
                     :triangulation-confidence overall-triangulation
                     :overall-grade (cond
                                      (> overall-separation 0.8) :excellent
                                      (> overall-separation 0.6) :good
                                      (> overall-separation 0.4) :moderate
                                      (< overall-separation 0.3) :poor
                                      :else :needs-training)}

                    ; Real-time processing config (optimized for frontend)
                    :realtime-config
                    {:confidence-threshold final-threshold
                     :boost-factor confidence-boost
                     :feature-weights {:golden-tensor 0.35 :triangulation-data 0.25
                                       :band-powers 0.20 :calibration-factors 0.15}
                     :stability-bonus (* overall-stability 0.4)
                     :separation-bonus (* overall-separation 0.5)
                     :triangulation-bonus (* overall-triangulation 0.3)}

                    ; Integration capabilities
                    :calibration-integration
                    {:uses-triangulation-data true
                     :uses-calibration-factors true
                     :uses-golden-tensor true
                     :compatible-with-existing-flow true}}]

      category)
    (catch Exception e
      (println "Error creating category intelligence:" (.getMessage e))
      (or existing-category-data {:error "Failed to create intelligence"}))))

(defn aggregate-signature-type!
  "Enhanced version that includes intelligence analysis"
  [profile-name category signature-type]
  (try
    (let [recording-dirs (find-signature-recording-dirs profile-name category signature-type)]
      (println "Found" (count recording-dirs) "recordings for signature type:" signature-type)

      (let [recordings (keep extract-metadata recording-dirs)
            _ (println "Extracted metadata from" (count recordings) "recordings")

            ; Create intelligent signature summary
            summary (create-signature-summary recordings (str category "/" signature-type))

            ; Define paths
            signature-dir (str (fio/get-wave-lexicon-dir profile-name category) "/" signature-type)
            signature-file (str signature-dir "/signature.edn")]

        ; Ensure directory exists
        (fio/ensure-directory! signature-dir)

        ; Save signature summary
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

      ; Create the overall category summaries
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
  "Automatically aggregate data for a category after a new recording with intelligence enhancement"
  [profile-name category signature]
  (try
    (println "Auto-aggregating with intelligence for category:" category)

    ; First, aggregate the specific signature type (PRESERVED original logic)
    (println "Aggregating signature type:" signature)
    (let [signature-summary (if (resolve 'aggregate-signature-type!)
                              ; Use enhanced version if available
                              (aggregate-signature-type! profile-name category signature)
                              ; Fall back to original version
                              (aggregate-signature-type! profile-name category signature))

          ; Get the other signature type for complete analysis
          other-signature (if (= signature "up") "down" "up")
          other-summary (if (resolve 'aggregate-signature-type!)
                          ; Use enhanced version if available
                          (aggregate-signature-type! profile-name category other-signature)
                          ; Fall back to original version
                          (aggregate-signature-type! profile-name category other-signature))

          ; Use the existing aggregation function for the overall category
          base-category-data (aggregate-category-signatures! profile-name category)

          ; Also update the category.edn file with stats
          stats (get-category-stats profile-name category)
          category-dir (fio/get-wave-lexicon-dir profile-name category)
          category-file (io/file category-dir "category.edn")]

      ; Create signature summaries map for intelligence processing
      (let [signature-summaries {(keyword signature) signature-summary
                                 (keyword other-signature) other-summary}

            ; Merge the stats with the summary data
            category-data-base {:stats stats
                                :summary base-category-data
                                :updated-at (java.util.Date.)}

            ; Intelligence layer if create-category-intelligence is available
            final-category-data (if (resolve 'create-category-intelligence)
                                  (merge category-data-base
                                         (create-category-intelligence
                                          profile-name category signature-summaries category-data-base))
                                  category-data-base)]

        ; Save to category-file
        (with-open [w (io/writer category-file)]
          (binding [*out* w]
            (pp/pprint final-category-data)))

        (println "Updated category.edn for" category "with"
                 (if (resolve 'create-category-intelligence) "intelligence" "standard aggregation"))
        final-category-data))

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

(defn upgrade-existing-categories!
  "Upgrade all existing categories to include intelligence layer"
  [profile-name]
  (try
    (let [lexicon-dir (fio/get-wave-lexicon-dir profile-name)
          categories (->> (io/file lexicon-dir)
                          .listFiles
                          (filter #(.isDirectory %))
                          (map #(.getName %)))]

      (println "Upgrading" (count categories) "categories to intelligence system...")

      (doseq [category categories]
        (println "Upgrading category:" category)
        ; Use "up" as signature parameter (it will process both up and down)
        (aggregate-after-recording! profile-name category "up"))

      (println "Successfully upgraded all categories to intelligence system"))
    (catch Exception e
      (println "Error upgrading categories:" (.getMessage e))
      (.printStackTrace e))))

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