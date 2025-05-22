(ns brain-pong.signature
  (:require [hyperfiddle.electric3 :as e]
            #?(:clj [floj.io :as fio])
            #?(:clj [floj.state :as state])
            #?(:clj [floj.wave-refraction :as refraction])
            #?(:clj [floj.brainflow.data-filter :as data-filter])))

#?(:clj
   (defn load-wave-signatures-from-dir
     "Load signature files from the specified category directory"
     [dir-path]
     (try
       (println "Loading wave signatures from" dir-path)
       (if-not (fio/dir-exists? dir-path)
         (do
           (println "Directory does not exist:" dir-path)
           [])
         (let [signature-file (str dir-path "/signature.edn")]
           (if (fio/file-exists? signature-file)
             ; If signature.edn exists, load it as the primary source
             (do
               (println "Found signature.edn file at" signature-file)
               [(fio/read-edn-file signature-file)])
             ; Otherwise, look for individual signature directories
             (let [; Look for directories that match the pattern category_timestamp
                   category-name (last (clojure.string/split dir-path #"/"))
                   sig-dirs (filter #(.startsWith % category-name)
                                    (fio/list-subdirectories dir-path))
                   signatures (reduce (fn [acc subdir]
                                        (let [sig-file (str dir-path "/" subdir "/signature_features.edn")]
                                          (if (fio/file-exists? sig-file)
                                            (conj acc (fio/read-edn-file sig-file))
                                            acc)))
                                      []
                                      sig-dirs)]
               (println "Loaded" (count signatures) "individual signatures")
               signatures))))
       (catch Exception e
         (println "Error loading wave signatures:" (.getMessage e))
         []))))

#?(:clj
   (defn compute-time-domain-stats
     "Compute basic time domain statistics for EEG data using BrainFlow's native implementation.
      raw-data is a sequence of samples, where each sample is a vector of channel values."
     [raw-data]
     (try
       ; Use BrainFlow's data-filter implementation for time domain stats
       (data-filter/compute-time-domain-stats raw-data)
       (catch Exception e
         (println "Error computing time domain stats:" (.getMessage e))
         {:channel-stats []}))))

#?(:clj (defn compute-frequency-bands
          "Compute frequency band powers for EEG data using BrainFlow's FFT capabilities"
          [raw-data sampling-rate]
          (when (seq raw-data)
            (try
              ; Use BrainFlow's real band power calculation
              (data-filter/calculate-band-powers raw-data sampling-rate)
              (catch Exception e
                (println "Error in compute-frequency-bands:" (.getMessage e))
                {:delta 0.0, :theta 0.0, :alpha 0.0, :beta 0.0, :gamma 0.0, :thinking 0.0})))))

#?(:clj
   (defn compute-wavelet-features
     "Compute wavelet features for EEG data using BrainFlow's wavelet transform.
      raw-data is a sequence of samples, where each sample is a vector of channel values.
      sampling-rate is the number of samples per second."
     [raw-data sampling-rate]
     (try
       (if (empty? raw-data)
         {:coefficients [], :energies {}}
         ; Use BrainFlow's native wavelet implementation
         (data-filter/compute-wavelet-features raw-data sampling-rate))
       (catch Exception e
         (println "Error computing wavelet features:" (.getMessage e))
         {:coefficients [], :energies {}}))))

#?(:clj
   (defn compute-time-domain-similarity
     "Compare time domain features between current data and signature"
     [current-features signature-features]
     (try
       (if (or (nil? current-features) (nil? signature-features))
         0.0  ; Return zero similarity if either is nil
         (let [current-stats (:channel-stats current-features)
               signature-stats (:channel-stats signature-features)

               ; Ensure we're comparing the same number of channels
               min-channels (min (count current-stats) (count signature-stats))

               ; For each channel, compute similarity between stats
               channel-similarities
               (for [i (range min-channels)]
                 (let [current-chan (nth current-stats i)
                       signature-chan (nth signature-stats i)

                       ; Compare means with tolerance for variance
                       mean-diff (Math/abs (- (:mean current-chan) (:mean signature-chan)))
                       mean-norm (/ mean-diff (max 1.0 (+ (:std-dev current-chan) (:std-dev signature-chan))))
                       mean-sim (Math/exp (- (* 2.0 mean-norm)))

                       ; Compare variance/std-dev
                       var-ratio (/ (min (:variance current-chan) (:variance signature-chan))
                                    (max 0.001 (max (:variance current-chan) (:variance signature-chan))))
                       var-sim (Math/sqrt var-ratio)

                       ; Compare ranges
                       range-ratio (/ (min (:range current-chan) (:range signature-chan))
                                      (max 0.001 (max (:range current-chan) (:range signature-chan))))
                       range-sim (Math/sqrt range-ratio)

                       ; Weighted combination of similarities
                       combined-sim (/ (+ (* 0.5 mean-sim) (* 0.3 var-sim) (* 0.2 range-sim))
                                       1.0)]
                   combined-sim))]

           ; Average similarities across channels
           (/ (apply + channel-similarities) (max 1 (count channel-similarities)))))
       (catch Exception e
         (println "Error computing time domain similarity:" (.getMessage e))
         0.0))))


#?(:clj
   (defn compute-frequency-similarity
     "Compare frequency domain features between current data and signature"
     [current-freq signature-freq]
     (try
       (if (or (nil? current-freq) (nil? signature-freq))
         0.0  ; Return zero similarity if either is nil
         (let [; Compare each frequency band
               band-keys [:delta :theta :alpha :beta :gamma :thinking]

               ; Compute similarity for each band
               band-similarities
               (for [band band-keys
                     :let [current-val (get current-freq band 0.0)
                           signature-val (get signature-freq band 0.0)]]
                 (if (and (number? current-val) (number? signature-val)
                          (pos? current-val) (pos? signature-val))
                   (let [; Ratio-based similarity (closer to 1.0 is better)
                         ratio (/ (min current-val signature-val)
                                  (max 0.001 (max current-val signature-val)))

                         ; Define band weights (customize based on importance)
                         band-weight (case band
                                       :alpha 0.25    ; Alpha often important for mental states
                                       :beta 0.25     ; Beta important for active thinking
                                       :thinking 0.2  ; Custom "thinking" band
                                       :theta 0.15    ; Moderate importance
                                       :delta 0.1     ; Less important (often contains noise)
                                       :gamma 0.05)]  ; Gamma can be noisy in many consumer EEGs

                     ; Apply weight and return similarity for this band
                     (* ratio band-weight))
                   0.0)) ; Skip bands with invalid values

               ; Sum weights of valid bands
               total-weight (reduce + (for [band band-keys
                                            :let [current-val (get current-freq band 0.0)
                                                  signature-val (get signature-freq band 0.0)]
                                            :when (and (number? current-val) (number? signature-val)
                                                       (pos? current-val) (pos? signature-val))]
                                        (case band
                                          :alpha 0.25
                                          :beta 0.25
                                          :thinking 0.2
                                          :theta 0.15
                                          :delta 0.1
                                          :gamma 0.05)))]

           ; Normalize by total weight and return
           (if (pos? total-weight)
             (/ (reduce + band-similarities) total-weight)
             0.0)))
       (catch Exception e
         (println "Error computing frequency similarity:" (.getMessage e))
         0.0))))

#?(:clj
   (defn compute-wavelet-similarity
     "Compare wavelet features between current data and signature"
     [current-wavelet signature-wavelet]
     (try
       (if (or (nil? current-wavelet) (nil? signature-wavelet))
         0.0
         (let [current-energies (:energies current-wavelet)    ; Compare energies in different bands
               signature-energies (:energies signature-wavelet)

               ; Get the energy bands to compare
               energy-bands [:delta :theta :alpha :beta :gamma]

               ; Calculate similarity for each energy band
               band-similarities
               (for [band energy-bands
                     :let [current-energy (get current-energies band 0.0)
                           signature-energy (get signature-energies band 0.0)]
                     :when (and (pos? current-energy) (pos? signature-energy))]
                 (let [ratio (/ (min current-energy signature-energy) ; Ratio-based similarity
                                (max 0.001 (max current-energy signature-energy)))]
                   ratio))

               ; Calculate average similarity across bands
               avg-similarity (if (seq band-similarities)
                                (/ (apply + band-similarities) (count band-similarities))
                                0.0)]

           avg-similarity))
       (catch Exception e
         (println "Error computing wavelet similarity:" (.getMessage e))
         0.0))))

#?(:clj
   (defn compute-spatial-similarity
     "Compare spatial correlation features between current data and signature"
     [current-corr signature-corr]
     (try
       (if (or (nil? current-corr) (nil? signature-corr))
         0.0
         ; Compute correlation matrix similarity
         (let [; Flatten matrices into vectors for comparison (if they're not already)
               current-vec (if (vector? current-corr) current-corr (into [] (flatten current-corr)))
               signature-vec (if (vector? signature-corr) signature-corr (into [] (flatten signature-corr)))

               ; Ensure we're comparing the same number of elements
               min-elements (min (count current-vec) (count signature-vec))

               ; Calculate element-wise differences
               element-diffs (for [i (range min-elements)
                                   :let [curr-val (nth current-vec i)
                                         sig-val (nth signature-vec i)]]
                               (Math/abs (- curr-val sig-val)))

               ; Average difference
               avg-diff (if (pos? (count element-diffs))
                          (/ (reduce + element-diffs) (count element-diffs))
                          1.0)

               ; Convert to similarity score (0.0-1.0)
               similarity (- 1.0 (min 1.0 avg-diff))]

           similarity))
       (catch Exception e
         (println "Error computing spatial similarity:" (.getMessage e))
         0.0))))

#?(:clj
   (defn gradient-field
     "Compute a gradient field matrix from normalized multi-channel EEG data.
   Each off-diagonal element contains the vector direction and magnitude of the difference between two channels.
   Diagonal elements are zero-vectors with zero magnitude."
     [normalized-channels]
     (let [channel-count (count normalized-channels)
           channel-length (count (first normalized-channels))
        ;; Pairwise differences
           ch-diffs (for [i (range channel-count)
                          j (range channel-count)
                          :when (not= i j)]
                      (let [ch-i (nth normalized-channels i)
                            ch-j (nth normalized-channels j)
                            diffs (mapv #(- %1 %2) ch-i ch-j)]
                        {:i i :j j :diffs diffs}))
        ;; Default direction if missing
           default-dir (fn []
                         (let [random-dir (mapv (fn [_] (- (rand) 0.5))
                                                (range (max 1 (dec channel-length))))]
                           (refraction/normalize-vector random-dir)))]
       (try
         (vec
          (for [i (range channel-count)]
            (vec
             (for [j (range channel-count)]
               (if (= i j)
                 {:direction [(default-dir)]
                  :magnitude 0.0}
                 (let [pair-diffs (first (filter #(and (= (:i %) i) (= (:j %) j)) ch-diffs))
                       diffs (if pair-diffs
                               (:diffs pair-diffs)
                               (mapv (fn [_] (- (rand) 0.5))
                                     (range (dec channel-length))))
                       processed-diffs (if (< (count diffs) 2)
                                         (conj diffs (rand))
                                         diffs)
                       direction (refraction/normalize-vector processed-diffs)
                       magnitude (Math/sqrt (reduce + (map #(* % %) processed-diffs)))]
                   {:direction [direction]
                    :magnitude (max 0.01 magnitude)}))))))
         (catch Exception e
           (println "Error in gradient-field:" (.getMessage e))
           nil)))))

#?(:clj
   (defn extract-signature-features
     "Extract features from raw EEG data for signature matching"
     [raw-data sampling-rate]
     (try
       (println "Extracting features from raw EEG data...")
       (let [channel-count (count (first raw-data))
             ; Create time-domain features
             time-domain-stats (compute-time-domain-stats raw-data) ;these aren't real

             ; Create frequency-domain features
             freq-domain (compute-frequency-bands raw-data sampling-rate) ;these aren't real

             ; Extract wavelet features
             wavelet-features (compute-wavelet-features raw-data sampling-rate) ;these aren't real

             ; Compute correlation matrix between channels
             correlation-matrix (refraction/compute-covariance-matrix raw-data)

             ; Use PCA to find principal components
             pca-result (refraction/principal-component-analysis raw-data)

             ; Compute gradient field for topographic analysis
             gradient-field (gradient-field raw-data correlation-matrix)]

         ; Return complete feature set
         {:time-domain time-domain-stats
          :frequency-domain freq-domain
          :wavelet wavelet-features
          :spatial {:correlation correlation-matrix
                    :pca pca-result}
          :gradient gradient-field})
       (catch Exception e
         (println "Error extracting signature features:" (.getMessage e))
         nil))))

#?(:clj
   (defn compute-coherence-similarity
     "Calculate similarity between coherence patterns"
     [current-coherence template-coherence]
     (try
       (if (or (empty? current-coherence) (empty? template-coherence))
         0.5 ; Default value if no coherence data

         (let [; Create maps for easier lookup
               current-map (reduce (fn [acc item]
                                     (assoc acc (:channels item) (:correlation item)))
                                   {}
                                   current-coherence)

               template-map (reduce (fn [acc item]
                                      (assoc acc (:channels item) (:correlation item)))
                                    {}
                                    template-coherence)

               ; Find common channel pairs
               all-pairs (into #{} (concat (keys current-map) (keys template-map)))

               ; Calculate differences
               differences (for [pair all-pairs
                                 :let [current-val (get current-map pair 0.0)
                                       template-val (get template-map pair 0.0)
                                       diff (Math/abs (- current-val template-val))]]
                             diff)

               ; Average difference
               avg-diff (if (pos? (count differences))
                          (/ (reduce + differences) (count differences))
                          1.0)

               ; Convert to similarity
               similarity (/ 1.0 (+ 1.0 avg-diff))]

           similarity))
       (catch Exception e
         (println "Error calculating coherence similarity:" (.getMessage e))
         0.5))))

#?(:clj
   (defn calculate-band-power-similarity
     "Calculate similarity between band power distributions"
     [current-bands template-bands]
     (try
       (let [bands [:delta :theta :alpha :beta :gamma]

             ; Calculate Euclidean distance
             squared-diffs (for [band bands
                                 :let [current-val (get current-bands band 0.0)
                                       template-val (get template-bands band 0.0)
                                       diff (- current-val template-val)]]
                             (* diff diff))

             distance (Math/sqrt (reduce + squared-diffs))

             ; Convert to similarity (0-1 range)
             similarity (/ 1.0 (+ 1.0 distance))]

         similarity)
       (catch Exception e
         (println "Error calculating band power similarity:" (.getMessage e))
         0.5))))

#?(:clj
   (defn calculate-signature-similarity
     "Calculate comprehensive similarity between current EEG features and stored signature"
     [current-features signature]
     (try
       ; Calculate similarity scores for each feature domain
       (let [; Time domain similarity
             time-similarity (compute-time-domain-similarity
                              (:time-domain current-features)
                              (:time-domain signature))

             ; Frequency domain similarity
             freq-similarity (compute-frequency-similarity
                              (:frequency-domain current-features)
                              (:frequency-domain signature))

             ; Wavelet similarity
             wavelet-similarity (compute-wavelet-similarity
                                 (:wavelet current-features)
                                 (:wavelet signature))

             ; Spatial similarity
             spatial-similarity (compute-spatial-similarity
                                 (get-in current-features [:spatial :correlation])
                                 (get-in signature [:spatial :correlation]))

             ; Coherence similarity (if available)
             coherence-similarity (if (and (get-in current-features [:spatial :coherence])
                                           (get-in signature [:spatial :coherence]))
                                    (compute-coherence-similarity
                                     (get-in current-features [:spatial :coherence])
                                     (get-in signature [:spatial :coherence]))
                                    0.5) ; Default if not available

             ; Weight factors - customize based on what works best for your signals
             time-weight 0.15
             freq-weight 0.35
             wavelet-weight 0.25
             spatial-weight 0.15
             coherence-weight 0.10

             ; Calculate weighted average similarity
             weighted-similarity (/ (+ (* time-similarity time-weight)
                                       (* freq-similarity freq-weight)
                                       (* wavelet-similarity wavelet-weight)
                                       (* spatial-similarity spatial-weight)
                                       (* coherence-similarity coherence-weight))
                                    (+ time-weight freq-weight wavelet-weight
                                       spatial-weight coherence-weight))]

         ; Return normalized similarity score (0.0 to 1.0)
         (max 0.0 (min 1.0 weighted-similarity)))
       (catch Exception e
         (println "Error calculating signature similarity:" (.getMessage e))
         0.0))))


#?(:clj
   (defn calculate-feature-distance
     "Calculate distance between two feature sets (alternative to similarity)"
     [features1 features2]
     (try
       (let [; Calculate band power distance
             band-dist (let [bp1 (get features1 :frequency-domain)
                             bp2 (get features2 :frequency-domain)
                             keys (keys bp1)]
                         (if (and bp1 bp2 (seq keys))
                           (/ (reduce + (for [k keys
                                              :let [v1 (get bp1 k 0)
                                                    v2 (get bp2 k 0)]]
                                          (Math/pow (- v1 v2) 2)))
                              (count keys))
                           1.0))

             ; Calculate temporal pattern distance
             temp-dist (let [tp1 (get-in features1 [:time-domain :channel-stats])
                             tp2 (get-in features2 [:time-domain :channel-stats])]
                         (if (and tp1 tp2 (= (count tp1) (count tp2)))
                           (/ (reduce + (for [i (range (count tp1))
                                              :let [t1 (nth tp1 i)
                                                    t2 (nth tp2 i)]]
                                          (+ (Math/pow (- (:mean t1) (:mean t2)) 2)
                                             (Math/pow (- (:std-dev t1) (:std-dev t2)) 2))))
                              (count tp1))
                           1.0))

             ; Calculate wavelet distance
             wavelet-dist (let [w1 (get-in features1 [:wavelet :energies])
                                w2 (get-in features2 [:wavelet :energies])]
                            (if (and w1 w2)
                              (let [bands [:delta :theta :alpha :beta :gamma]
                                    band-diffs (for [band bands
                                                     :let [v1 (get w1 band 0)
                                                           v2 (get w2 band 0)]]
                                                 (Math/pow (- v1 v2) 2))]
                                (/ (reduce + band-diffs) (count bands)))
                              1.0))

             ; Calculate spatial distance
             spatial-dist (let [s1 (get-in features1 [:spatial :correlation])
                                s2 (get-in features2 [:spatial :correlation])]
                            (if (and s1 s2)
                              (- 1.0 (compute-spatial-similarity s1 s2))
                              1.0))

             ; Weighted total distance
             total-dist (+ (* 0.35 band-dist)      ; Frequency domain most important
                           (* 0.25 wavelet-dist)    ; Wavelet features next
                           (* 0.25 temp-dist)       ; Temporal patterns
                           (* 0.15 spatial-dist))   ; Spatial patterns

             ; Convert to similarity score (0-1)
             similarity (max 0.0 (min 1.0 (- 1.0 (/ total-dist 4.0))))]
         similarity)
       (catch Exception e
         (println "Error calculating feature distance:" (.getMessage e))
         0.0))))

#?(:clj
   (defn extract-signature-features
     "Extract comprehensive features from raw EEG data for signature matching"
     [raw-data sampling-rate]
     (try
       (println "Extracting features from raw EEG data...")
       (when (and raw-data (seq raw-data))
         (let [; Create time-domain features
               time-domain-stats (compute-time-domain-stats raw-data)

               ; Create frequency-domain features
               freq-domain (compute-frequency-bands raw-data sampling-rate)

               ; Extract wavelet features
               wavelet-features (compute-wavelet-features raw-data sampling-rate)

               ; Compute spatial features
               channel-count (count (first raw-data))

               ; Only compute correlation if we have multiple channels
               correlation-matrix (when (> channel-count 1)
                                    (try
                                      ; Use direct computation if refraction namespace not available
                                      (let [data-matrix (into-array (map double-array raw-data))
                                            corr-matrix (make-array Double/TYPE channel-count channel-count)]
                                        ; Simple correlation calculation
                                        (dotimes [i channel-count]
                                          (dotimes [j channel-count]
                                            (let [ch1 (map #(aget % i) data-matrix)
                                                  ch2 (map #(aget % j) data-matrix)
                                                  mean1 (/ (reduce + ch1) (count ch1))
                                                  mean2 (/ (reduce + ch2) (count ch2))
                                                  var1 (reduce + (map #(Math/pow (- % mean1) 2) ch1))
                                                  var2 (reduce + (map #(Math/pow (- % mean2) 2) ch2))
                                                  cov (reduce + (map #(* (- %1 mean1) (- %2 mean2)) ch1 ch2))
                                                  denom (Math/sqrt (* var1 var2))]
                                              (aset corr-matrix i j (if (pos? denom)
                                                                      (/ cov denom)
                                                                      0.0)))))
                                        corr-matrix)
                                      (catch Exception e
                                        (println "Error computing correlation matrix:" (.getMessage e))
                                        nil)))]

           ; Return complete feature set
           {:time-domain time-domain-stats
            :frequency-domain freq-domain
            :wavelet wavelet-features
            :spatial {:correlation correlation-matrix}}))
       (catch Exception e
         (println "Error extracting signature features:" (.getMessage e))
         nil))))

#?(:clj
   (defn match-brain-activity-server []
     (try
       (println "Matching brain activity against recorded signatures...")
       (let [current-eeg-data @state/eeg-data ; Get current EEG data from the state

             ; Skip if we don't have enough data
             _ (when (< (count current-eeg-data) 50)
                 (throw (Exception. "Not enough EEG data for matching")))

             ; Get last 1 second of data (or whatever sampling rate)
             sampling-rate ((:sampling-rate @state/state))
             last-n-samples (min (count current-eeg-data) sampling-rate)
             recent-data (take-last last-n-samples current-eeg-data)

             ; Extract features from current data
             current-features (extract-signature-features recent-data sampling-rate)

             ; Get active profile for matching
             profile-name (or (:name ((:get-active-profile @state/state))) "default")

             ; Load "up" signatures
             up-dir (fio/get-wave-lexicon-dir profile-name "pong/up")
             up-signatures (load-wave-signatures-from-dir up-dir)

             ; Load "down" signatures
             down-dir (fio/get-wave-lexicon-dir profile-name "pong/down")
             down-signatures (load-wave-signatures-from-dir down-dir)

             ; Calculate best match scores for each category
             up-scores (map #(calculate-signature-similarity current-features %) up-signatures)
             down-scores (map #(calculate-signature-similarity current-features %) down-signatures)

             ; Get highest score for each category
             best-up-score (if (seq up-scores) (apply max up-scores) 0.0)
             best-down-score (if (seq down-scores) (apply max down-scores) 0.0)

             ; Apply golden tensor calibration to enhance signal matching
             golden-tensor (get-in ((:get-active-profile @state/state)) [:golden-tensor])

             ; Apply golden tensor calibration if available
             [calibrated-up calibrated-down]
             (if golden-tensor
               (let [; Apply spectral calibration from golden tensor
                     spectral-calibration (get-in golden-tensor [:spectral :frequency-domain])

                     ; Get calibration factors
                     calibration-factors (try
                                           (-> (str (fio/get-wave-lexicon-dir profile-name "pong") "/category.edn")
                                               (fio/read-edn-file)
                                               (get-in [:summary :all :calibration-factors :average]))
                                           (catch Exception _ nil))

                     ; Apply calibration to scores
                     calibrated-up (if (and spectral-calibration calibration-factors)
                                     (* best-up-score
                                        (/ (+ (:alpha calibration-factors)
                                              (:beta calibration-factors))
                                           2.0))
                                     best-up-score)

                     calibrated-down (if (and spectral-calibration calibration-factors)
                                       (* best-down-score
                                          (/ (+ (:alpha calibration-factors)
                                                (:beta calibration-factors))
                                             2.0))
                                       best-down-score)]

                 ; Return calibrated values clamped between 0.0-1.0
                 [(max 0.0 (min 1.0 calibrated-up))
                  (max 0.0 (min 1.0 calibrated-down))])

               ; If no golden tensor, return original scores
               [best-up-score best-down-score])

             ; Create confidence map with calibrated values
             confidence-data {:up calibrated-up
                              :down calibrated-down}]

         (println "Confidence scores:" confidence-data)
         confidence-data)
       (catch Exception e
         (println "Error matching brain activity:" (.getMessage e))
         {:up 0.0 :down 0.0}))))