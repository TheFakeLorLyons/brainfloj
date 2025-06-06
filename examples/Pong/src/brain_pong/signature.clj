(ns brain-pong.signature
  (:require [floj.api :as api]
            [floj.calibration :as calibration]
            [floj.io :as fio]
            [floj.wave-lexicon :as lexi]
            [floj.state :as state]
            [floj.wave-refraction :as refraction]
            [floj.brainflow.data-filter :as data-filter]))

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
      [])))

(defn compute-time-domain-stats
  "Compute basic time domain statistics for EEG data using BrainFlow's native implementation.
      raw-data is a sequence of samples, where each sample is a vector of channel values."
  [raw-data]
  (try
       ; Use BrainFlow's data-filter implementation for time domain stats
    (data-filter/compute-time-domain-stats raw-data)
    (catch Exception e
      (println "Error computing time domain stats:" (.getMessage e))
      {:channel-stats []})))

(defn compute-frequency-bands
  "Compute frequency band powers for EEG data using BrainFlow's FFT capabilities"
  [raw-data sampling-rate]
  (when (seq raw-data)
    (try
              ; Use BrainFlow's real band power calculation
      (data-filter/calculate-band-powers raw-data sampling-rate)
      (catch Exception e
        (println "Error in compute-frequency-bands:" (.getMessage e))
        {:delta 0.0, :theta 0.0, :alpha 0.0, :beta 0.0, :gamma 0.0, :thinking 0.0}))))

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
      {:coefficients [], :energies {}})))

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
      0.0)))

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
      0.0)))

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
      0.0)))

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
      0.0)))

(defn gradient-field
  "Compute a gradient field matrix from normalized multi-channel EEG data.
   Each off-diagonal element contains the vector direction and magnitude of the difference between two channels.
   Diagonal elements are zero-vectors with zero magnitude."
  [normalized-channels]
  (let [channel-count (count normalized-channels)
        channel-length (count (first normalized-channels))
        ; Pairwise differences
        ch-diffs (for [i (range channel-count)
                       j (range channel-count)
                       :when (not= i j)]
                   (let [ch-i (nth normalized-channels i)
                         ch-j (nth normalized-channels j)
                         diffs (mapv #(- %1 %2) ch-i ch-j)]
                     {:i i :j j :diffs diffs}))
        ; Default direction if missing
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
        nil))))


(defn calculate-simple-band-powers
  "Calculate proper band powers using BrainFlow's FFT implementation"
  [channel-data sampling-rate]
  (try
    (if (empty? channel-data)
      {:delta 0.0 :theta 0.0 :alpha 0.0 :beta 0.0 :gamma 0.0 :thinking 0.0}
      (let [; Process each channel separately
            channel-powers (for [channel channel-data]
                             (let [; Ensure we have a flat vector of numbers
                                   flat-channel (if (sequential? channel)
                                                  (vec (flatten channel))
                                                  [channel])

                                   ; Perform FFT using BrainFlow
                                   [real-parts imag-parts] (data-filter/perform-fft flat-channel)

                                   ; Calculate power spectral density
                                   psd (mapv (fn [r i] (+ (* r r) (* i i)))
                                             real-parts imag-parts)

                                   ; Calculate frequency bins
                                   n-samples (count flat-channel)
                                   freq-resolution (/ sampling-rate n-samples)
                                   frequencies (mapv #(* % freq-resolution) (range (count psd)))

                                   ; Helper function to sum power in frequency band
                                   band-power (fn [[min-freq max-freq]]
                                                (reduce +
                                                        (map second
                                                             (filter (fn [[freq power]]
                                                                       (and (>= freq min-freq)
                                                                            (< freq max-freq)))
                                                                     (map vector frequencies psd)))))

                                   ; Calculate power for each band
                                   delta-power (band-power [1.0 4.0])
                                   theta-power (band-power [4.0 8.0])
                                   alpha-power (band-power [8.0 13.0])
                                   beta-power (band-power [13.0 30.0])
                                   gamma-power (band-power [30.0 50.0])
                                   thinking-power (band-power [8.0 30.0])]

                               {:delta delta-power
                                :theta theta-power
                                :alpha alpha-power
                                :beta beta-power
                                :gamma gamma-power
                                :thinking thinking-power}))

            ; Average across all channels
            total-channels (count channel-powers)
            averaged-powers (if (pos? total-channels)
                              {:delta (/ (reduce + (map :delta channel-powers)) total-channels)
                               :theta (/ (reduce + (map :theta channel-powers)) total-channels)
                               :alpha (/ (reduce + (map :alpha channel-powers)) total-channels)
                               :beta (/ (reduce + (map :beta channel-powers)) total-channels)
                               :gamma (/ (reduce + (map :gamma channel-powers)) total-channels)
                               :thinking (/ (reduce + (map :thinking channel-powers)) total-channels)}
                              {:delta 0.0 :theta 0.0 :alpha 0.0 :beta 0.0 :gamma 0.0 :thinking 0.0})

            ; Normalize to prevent extremely large values
            total-power (reduce + (vals averaged-powers))
            normalized-powers (if (pos? total-power)
                                (into {} (map (fn [[k v]] [k (/ v total-power)]) averaged-powers))
                                averaged-powers)]

        normalized-powers))
    (catch Exception e
      (println "Error calculating FFT band powers:" (.getMessage e))
      {:delta 0.0 :theta 0.0 :alpha 0.0 :beta 0.0 :gamma 0.0 :thinking 0.0})))

(defn extract-features-from-eeg-data
  "Extract features using calibration module's robust band power extraction"
  [eeg-data sampling-rate]
  (try
    (println "=== ROBUST FEATURE EXTRACTION (using calibration module) ===")
    (println "Input data type:" (type eeg-data))
    (println "Sample count:" (count eeg-data))

    (when (seq eeg-data)
      (let [; Extract EEG arrays from the data structure
            eeg-samples (mapv :eeg eeg-data)
            ; Flatten all samples into one continuous dataset
            all-samples (vec (apply concat eeg-samples))

            ; Debug info
            _ (println "Total samples extracted:" (count all-samples))

            ; Filter out any invalid samples
            valid-samples (filterv #(and (sequential? %)
                                         (every? number? %)
                                         (= (count %) 5)) ; Ensure 5 channels
                                   all-samples)

            _ (println "Valid samples after filtering:" (count valid-samples))
            _ (when (seq valid-samples)
                (println "First valid sample:" (first valid-samples)))

            ; Check minimum sample requirement
            _ (when (< (count valid-samples) 8)
                (throw (Exception. (str "Not enough samples for reliable analysis: " (count valid-samples)))))

            ; Transpose to get [channels][samples] format for calibration module
            channels (when (seq valid-samples)
                       (let [n-channels 5] ; We know we have 5 channels
                         (println "Processing" n-channels "channels with" (count valid-samples) "samples each")
                         (for [ch (range n-channels)]
                           (mapv #(nth % ch 0.0) valid-samples))))

            ; Use calibration module's band power extraction
            band-powers (when channels
                          (calibration/extract-band-powers channels sampling-rate))]

        (println "Final band powers:" band-powers)
        (if (and band-powers
                 (map? band-powers)
                 (pos? (reduce + (vals band-powers))))
          {:band-powers band-powers
           :timestamp (System/currentTimeMillis)
           :sample-count (count valid-samples)
           :channels (count channels)}
          (throw (Exception. "Band power extraction failed - all powers are zero")))))
    (catch Exception e
      (println "Error in feature extraction:" (.getMessage e))
      (.printStackTrace e)
      nil)))


(defn create-signature-from-eeg-data
  "Create a signature using calibration module's band power extraction"
  [eeg-data sampling-rate]
  (try
    (let [features (extract-features-from-eeg-data eeg-data sampling-rate)]
      (when features
        {:band-powers (:band-powers features)
         :timestamp (:timestamp features)
         :sample-count (:sample-count features)
         :created-at (java.util.Date.)
         :sampling-rate sampling-rate}))
    (catch Exception e
      (println "Error creating signature:" (.getMessage e))
      nil)))

(defn calculate-band-power-similarity-fixed
  "Calculate similarity between current and signature band powers"
  [current-bands signature-bands]
  (try
    (let [bands [:delta :theta :alpha :beta :gamma :thinking]

          ; Extract band values, handling nested structure in signature
          current-values (if (map? current-bands)
                           current-bands
                           {:delta 0.0 :theta 0.0 :alpha 0.0 :beta 0.0 :gamma 0.0 :thinking 0.0})

          signature-values (if (and (map? signature-bands) (contains? signature-bands :average))
                             (:average signature-bands)  ; Use average from signature
                             signature-bands)

          ; Calculate similarity for each band
          band-similarities (for [band bands
                                  :let [current-val (get current-values band 0.0)
                                        signature-val (get signature-values band 0.0)]]
                              (if (and (pos? current-val) (pos? signature-val))
                                ; Use ratio-based similarity
                                (let [ratio (/ (min current-val signature-val)
                                               (max current-val signature-val))
                                      ; Apply log scaling for large differences
                                      log-sim (Math/exp (- (Math/abs (Math/log (/ current-val signature-val)))))]
                                  (max ratio log-sim))
                                0.1)) ; Small default if either is zero

          ; Weighted average (emphasize important bands)
          weights {:delta 0.1 :theta 0.15 :alpha 0.2 :beta 0.2 :gamma 0.2 :thinking 0.15}
          weighted-sum (reduce + (map #(* %1 (get weights %2 0.1)) band-similarities bands))
          weight-total (reduce + (vals weights))]

      (/ weighted-sum weight-total))

    (catch Exception e
      (println "Error calculating band power similarity:" (.getMessage e))
      0.0)))

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
      0.5)))

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
      0.5)))

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
      0.0)))

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
          total-dist (+ (* 0.35 band-dist)       ; Frequency domain most important
                        (* 0.25 wavelet-dist)    ; Wavelet features next
                        (* 0.25 temp-dist)       ; Temporal patterns
                        (* 0.15 spatial-dist))   ; Spatial patterns

             ; Convert to similarity score (0-1)
          similarity (max 0.0 (min 1.0 (- 1.0 (/ total-dist 4.0))))]
      similarity)
    (catch Exception e
      (println "Error calculating feature distance:" (.getMessage e))
      0.0)))

(defn start-eeg-streaming-server []
  (try
    (println "Starting EEG streaming for category:")

    (let [_ (println "***GAME: Starting category recording at" (System/currentTimeMillis))
          recording-started? (lexi/start-category-recording! "pong")
          _ (println "***GAME: after Starting category recording at" (System/currentTimeMillis))]
      
      (if recording-started?
        (do
          (reset! state/recording? true)
          (println "EEG streaming started successfully")
          {:success true})
        (do
          (println "Failed to start EEG streaming")
          {:success false :error "Failed to start recording"})))
    (catch Exception e
      (println "Error starting EEG streaming:" (.getMessage e))
      {:success false :error (.getMessage e)})))

(defn stop-eeg-streaming-server []
  (try
    (println "Stopping EEG streaming")
(println "***GAME: STOPPED category recording at" (System/currentTimeMillis))
    (lexi/stop-category-recording! "pong")
    (reset! state/recording? false)
    (println "EEG streaming stopped")
    {:success true}
    (catch Exception e
      (println "Error stopping EEG streaming:" (.getMessage e))
      {:success false :error (.getMessage e)})))

(defn calculate-feature-similarity [features1 features2]
  "Calculate similarity between two feature sets using simple correlation"
  (try
    (if (or (nil? features1) (nil? features2))
      0.0
      (let [td1 (:time-domain features1)
            td2 (:time-domain features2)]
        (if (and td1 td2 (= (count td1) (count td2)))
          ; Simple correlation based on mean and std values
          (let [correlations
                (for [i (range (count td1))]
                  (let [ch1 (nth td1 i)
                        ch2 (nth td2 i)
                        mean-diff (Math/abs (- (:mean ch1) (:mean ch2)))
                        std-diff (Math/abs (- (:std ch1) (:std ch2)))
                        ; Normalize differences to 0-1 range (inverse similarity)
                        mean-sim (max 0.0 (- 1.0 (min 1.0 (/ mean-diff 50.0))))
                        std-sim (max 0.0 (- 1.0 (min 1.0 (/ std-diff 20.0))))]
                    (/ (+ mean-sim std-sim) 2.0)))]
            ; Average correlation across all channels
            (/ (reduce + correlations) (count correlations)))
          0.0)))
    (catch Exception e
      (println "Error calculating similarity:" (.getMessage e))
      0.0)))

(defn get-recent-data
  "Get recent EEG data within specified time window - ensures fresh data"
  [seconds]
  (let [current-time (System/currentTimeMillis)
        cutoff-time (- current-time (* seconds 1000))
        eeg-data @state/eeg-data
        ;; Add a small buffer to ensure we don't get stale data
        fresh-cutoff (- current-time (* 1.5 1000))] ; 500ms buffer

    (println "=== DEBUG GET-RECENT-DATA ===")
    (println "Current time:" current-time)
    (println "Cutoff time:" cutoff-time)
    (println "Fresh cutoff:" fresh-cutoff)
    (println "Total EEG data points:" (count eeg-data))

    (when (seq eeg-data)
      (let [recent-timestamps (map :timestamp (take-last 10 eeg-data))]
        (println "Recent 10 timestamps:" recent-timestamps)
        (println "Most recent timestamp:" (last recent-timestamps))
        (println "Time since most recent:" (- current-time (last recent-timestamps)))))

    (when (seq eeg-data)
      (println "=== MATCH TIMESTAMP:" (select-keys (first eeg-data) [:timestamp]) "===")
      (println "EEG data hash:" (hash eeg-data))
      (println "First sample timestamp:" (:timestamp (first eeg-data)))
      (println "Last sample timestamp:" (:timestamp (last eeg-data))))

    ; Filter for recent data and ensure it's actually fresh
    (when eeg-data
      (let [recent (filterv #(> (:timestamp %) cutoff-time) eeg-data)
            ;; Additional check: ensure we have data newer than 500ms
            fresh-recent (filterv #(> (:timestamp %) fresh-cutoff) recent)]
        (println "Recent data points found:" (count recent))
        (println "Fresh recent data points:" (count fresh-recent))

        ;; Only return data if we have fresh samples
        (if (seq fresh-recent)
          fresh-recent
          (do
            (println "WARNING: No fresh data found, data may be stale")
            ;; Return empty vector to force "not enough data" error
            []))))))

(defn get-recent-data-with-even-samples
  "Get recent EEG data ensuring even number of samples for FFT"
  [seconds]
  (let [current-time (System/currentTimeMillis)
        cutoff-time (- current-time (* seconds 1000))
        eeg-data @state/eeg-data
        fresh-cutoff (- current-time (* 1.0 1000))] ;; Reduced buffer to 1 second

    (println "=== DEBUG GET-RECENT-DATA-EVEN ===")
    (println "Current time:" current-time)
    (println "Cutoff time:" cutoff-time)
    (println "Total EEG data points:" (count eeg-data))

    (when eeg-data
      (let [recent (filterv #(> (:timestamp %) cutoff-time) eeg-data)
            fresh-recent (filterv #(> (:timestamp %) fresh-cutoff) recent)
            ;; Ensure even number of samples
            sample-count (count fresh-recent)
            even-count (if (even? sample-count) sample-count (dec sample-count))
            even-samples (take even-count fresh-recent)]

        (println "Recent data points found:" (count recent))
        (println "Fresh recent data points:" sample-count)
        (println "Even sample count:" even-count)

        (if (>= even-count 8) ;; Minimum 8 samples for reliable FFT
          (vec even-samples)
          (do
            (println "WARNING: Not enough even samples for FFT")
            []))))))

(defn get-smoothed-features
  "Get smoothed features with better time window management and even samples"
  [total-seconds window-count sampling-rate]
  (let [window-size-seconds (/ total-seconds window-count)
        current-time (System/currentTimeMillis)

        ; Create time windows going backwards from current time
        windows (for [i (range window-count)
                      :let [end-offset (* i window-size-seconds)
                            start-offset (+ end-offset window-size-seconds)
                            end-time (- current-time (* end-offset 1000))
                            start-time (- current-time (* start-offset 1000))
                            eeg-data @state/eeg-data
                            window-data (when eeg-data
                                          (filterv #(and (>= (:timestamp %) start-time)
                                                         (<= (:timestamp %) end-time))
                                                   eeg-data))
                            ; Ensure even sample count for each window
                            sample-count (count window-data)
                            even-count (if (even? sample-count) sample-count (dec sample-count))
                            even-window-data (take even-count window-data)]]
                  (when (>= even-count 6) even-window-data)) ;; Minimum samples per window

        valid-windows (filter some? windows)]

    (println "=== IMPROVED SMOOTHED FEATURES ===")
    (println "Total windows attempted:" window-count)
    (println "Valid windows found:" (count valid-windows))
    (println "Window sizes:" (map count valid-windows))

    (when (>= (count valid-windows) 2)  ; Need at least 2 windows
      (let [feature-sets (->> valid-windows
                              (map #(extract-features-from-eeg-data % sampling-rate))
                              (filter some?))

            ; Average the band powers across windows with weights (more recent = higher weight)
            averaged-features (when (seq feature-sets)
                                (let [band-power-sets (map :band-powers feature-sets)
                                      all-keys (set (mapcat keys band-power-sets))
                                      weights (map #(Math/pow 0.8 %) (range (count band-power-sets))) ; Exponential decay
                                      total-weight (reduce + weights)

                                      averaged-bands (into {}
                                                           (map (fn [key]
                                                                  [key (/ (reduce +
                                                                                  (map-indexed
                                                                                   (fn [idx bp-set]
                                                                                     (* (nth weights idx)
                                                                                        (get bp-set key 0)))
                                                                                   band-power-sets))
                                                                          total-weight)])
                                                                all-keys))]
                                  {:band-powers averaged-bands}))]

        (println "Feature sets extracted:" (count feature-sets))
        (println "Averaged band powers:" (:band-powers averaged-features))
        averaged-features))))

(defn continue-with-matching
  "Continue matching process with extracted features"
  [current-features is-smoothed?]
  (let [profile-name (or (:name ((:get-active-profile @state/state))) "default")
        up-dir (fio/get-wave-lexicon-dir profile-name "pong/up")
        up-signatures (load-wave-signatures-from-dir up-dir)
        down-dir (fio/get-wave-lexicon-dir profile-name "pong/down")
        down-signatures (load-wave-signatures-from-dir down-dir)]

    (println "=== SIGNATURE MATCHING ===")
    (println "Profile name:" profile-name)
    (println "Features smoothed:" is-smoothed?)
    (println "Up signatures loaded:" (count up-signatures))
    (println "Down signatures loaded:" (count down-signatures))

    (if (and (seq up-signatures) (seq down-signatures))
      (let [current-bands (:band-powers current-features)

            up-scores (map #(calculate-band-power-similarity-fixed
                             current-bands
                             (:band-powers %))
                           up-signatures)
            down-scores (map #(calculate-band-power-similarity-fixed
                               current-bands
                               (:band-powers %))
                             down-signatures)

            best-up-score (if (seq up-scores) (apply max up-scores) 0.0)
            best-down-score (if (seq down-scores) (apply max down-scores) 0.0)]

        (println "Current band powers:" current-bands)
        (println "Best up score:" best-up-score)
        (println "Best down score:" best-down-score)
        (println "Confidence gap:" (Math/abs (- best-up-score best-down-score)))

        {:up best-up-score :down best-down-score})

      (do
        (println "No trained patterns found")
        {:up 0.0 :down 0.0 :error "No trained patterns found"}))))


(defn match-brain-activity
  "Match current brain activity using calibration-based feature extraction with smoothing"
  []
  (try
    (let [recording? @state/recording?
          eeg-data @state/eeg-data
          current-time (System/currentTimeMillis)]

      (println "=== BRAIN ACTIVITY MATCHING START ===")
      (println "Current time:" current-time)
      (println "Recording active:" recording?)

      (cond
        (not recording?)
        (do
          (println "EEG recording not active")
          {:up 0.0 :down 0.0 :error "EEG recording not started"})

        (nil? eeg-data)
        (do
          (println "EEG data stream not available")
          {:up 0.0 :down 0.0 :error "EEG data stream not ready"})

        (empty? eeg-data)
        (do
          (println "No EEG data collected")
          {:up 0.0 :down 0.0 :error "No EEG data collected yet"})

        :else
        (let [sampling-rate (api/get-current-sample-rate)
              ; Try smoothed features first, fall back to regular if not enough data
              smoothed-features (get-smoothed-features 3.0 3 sampling-rate)  ; 3 seconds, 3 windows
              current-features (or smoothed-features
                                   (let [recent-data (get-recent-data 2)]
                                     (when (>= (count recent-data) 4)
                                       (extract-features-from-eeg-data recent-data sampling-rate))))]

          (println "=== FEATURE EXTRACTION STRATEGY ===")
          (println "Smoothed features available:" (some? smoothed-features))
          (println "Using smoothed approach:" (some? smoothed-features))

          (if (nil? current-features)
            (do
              (println "Feature extraction failed - trying fallback approach")
              (let [recent-data (get-recent-data 2)]
                (println "Fallback: Recent data count:" (count recent-data))
                (if (< (count recent-data) 2)  ; Lower requirement for fallback
                  {:up 0.0 :down 0.0 :error "Not enough recent data for analysis"}

                  ; Simple fallback feature extraction
                  (let [fallback-features (extract-features-from-eeg-data recent-data sampling-rate)]
                    (if fallback-features
                      (continue-with-matching fallback-features false)  ; false = not smoothed
                      {:up 0.0 :down 0.0 :error "All feature extraction methods failed"})))))

            ; Continue with successful feature extraction
            (continue-with-matching current-features (some? smoothed-features))))))

    (catch Exception e
      (println "ERROR in match-brain-activity:" (.getMessage e))
      (.printStackTrace e)
      {:up 0.0 :down 0.0 :error (.getMessage e)})))