(ns brain-pong.signature
  "This namespace is a work in progress, and contains lots of currently unimplemented code.
   It is intended to be the interface between the rest of brainfloj and applications built
   upon it. Currently it is set up for comparisons between live signals and pre-recorded 
   wave-signatures/categories saved via the brainfloj CLI. In the future will feature more 
   robust signature/category feature comparisons, and more live update functionality. 
   I have noted in the docstrings which functions are currently being used; and eventually 
   I plan on incorporating all of the functions included."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [floj.api :as api]
            [floj.calibration :as calibration]
            [floj.io :as fio]
            [floj.wave-lexicon :as lexi]
            [floj.state :as state]
            [floj.wave-refraction :as refraction]
            [floj.brainflow.data-filter :as data-filter])
  (:import [java.util.concurrent.locks ReentrantLock]
           [java.util.concurrent TimeUnit]))

(def category-state
  "Reference structure: {category-name {:lock ReentrantLock
                                        :last-update timestamp
                                        :failures count
                                        :buffer data}}"
  (atom {}))

(def live-signature-buffer (atom {}))

(defn get-or-create-category-state
  "Get or create category state with all needed components.
   
   Called in 'get-category-lock' and 'should-attempt-category-update?' below."
  [category]
  (let [current-state @category-state]
    (if (contains? current-state category)
      (get current-state category)
      (let [new-state {:lock (ReentrantLock.)
                       :last-update 0
                       :failures 0
                       :buffer {}}]
        (swap! category-state assoc category new-state)
        new-state))))

(defn get-category-lock
  "Get the ReentrantLock for a specific category.
   
   Called in 'update-category-thread-safe!' below."
  [category]
  (:lock (get-or-create-category-state category)))

(defn should-attempt-category-update?
  "Circuit breaker per category with recovery mechanism.
   
   -NOT CALLED IN THIS NAMESPACE-
   Called in 'start-signature-enhancement-background!' in the Pong example."
  [category]
  (let [category-info (get-or-create-category-state category)
        failures (:failures category-info)
        last-update (:last-update category-info)
        current-time (System/currentTimeMillis)
        time-since-last (- current-time last-update)
        min-interval 3000
        ; Recovery logic - reset failures after 60 seconds
        recovery-interval 60000
        should-reset-failures? (and (>= failures 5)
                                    (> time-since-last recovery-interval))]

    ; Reset failures if enough time has passed
    (when should-reset-failures?
      (println "Resetting circuit breaker for" category "after" time-since-last "ms")
      (swap! category-state update-in [category :failures] (constantly 0)))

    ; Re-check failures after potential reset
    (let [updated-failures (:failures (get-or-create-category-state category))]
      (println "  Circuit breaker check for" category ":")
      (println "  Failures:" updated-failures "/5")
      (println "  Time since last:" time-since-last "ms (min:" min-interval "ms)")
      (println "  Circuit breaker open?" (>= updated-failures 5))
      (println "  Time check passed?" (> time-since-last min-interval))

      (let [result (and (< updated-failures 5)
                        (> time-since-last min-interval))]
        (println "  Should attempt update?" result)
        result))))

(defn record-category-update-result
  "Track success/failure per category in consolidated state.
   
   Called in 'update-category-thread-safe!' below."
  [category success?]
  (let [current-time (System/currentTimeMillis)]
    (if success?
      (do
        (println "✅ Recording success for category:" category)
        (swap! category-state update category
               #(-> %
                    (assoc :failures 0)
                    (assoc :last-successful-update current-time))))
      (do
        (println "❌ Recording failure for category:" category)
        (swap! category-state update-in [category :failures] (fnil inc 0))))

    ; Log the current state
    (let [updated-state (get @category-state category)]
      (println "  Updated state - Failures:" (:failures updated-state)
               "Last update:" (:last-update updated-state)))))

(defn should-capture-live-signature?
  "Currently unimplemented...
   
   Determine if current confidence is high enough to capture a live signature."
  [confidence-data game-context category-intelligence]
  (let [max-confidence (max (:up confidence-data) (:down confidence-data))
        confidence-gap (Math/abs (- (:up confidence-data) (:down confidence-data)))

        ; Use existing intelligence metrics for decision
        separation-score (get-in category-intelligence [:summary :all :intelligence-metrics :separation-score] 0.5)

        ; Higher separation = more confident system = higher threshold for enhancement
        enhancement-threshold (if (> separation-score 0.7) 0.75 0.6)
        gap-threshold (if (> separation-score 0.7) 0.2 0.15)]

    (and (> max-confidence enhancement-threshold)
         (> confidence-gap gap-threshold)
         (get game-context :ball-position-validates-intent false))))

(defn should-enhance-signature?
  "Currently unimplemented, 
      but called in 'capture-live-enhancement!' below.
   
   Determine if current confidence is high enough to enhance signature live."
  [confidence-data game-context category-intelligence]
  (let [max-confidence (max (:up confidence-data) (:down confidence-data))
        confidence-gap (Math/abs (- (:up confidence-data) (:down confidence-data)))

        ; Use existing intelligence metrics
        separation-score (get-in category-intelligence [:summary :all :intelligence-metrics :separation-score] 0.5)

        ; Higher separation = more confident system = higher threshold for enhancement
        enhancement-threshold (if (> separation-score 0.7) 0.75 0.6)
        gap-threshold (if (> separation-score 0.7) 0.2 0.15)]

    (and (> max-confidence enhancement-threshold)
         (> confidence-gap gap-threshold)
         ; Add game context validation if available
         (get game-context :ball-position-validates-intent true))))

(defn enhance-category-live!
  "Currently unimplemented, 
      but called in 'capture-live-enhancement!' below.
   
   Enhance category.edn with live high-confidence data using your existing structure."
  [category direction samples]
  (try
    (let [category-file (lexi/load-category category)

          ; Calculate enhancement metrics
          avg-quality (/ (reduce + (map :quality-score samples)) (count samples))
          current-separation (get-in category-file [:summary :all :intelligence-metrics :separation-score] 0.5)

          ; Apply modest enhancement (don't over-boost)
          separation-boost (min 0.1 (* 0.05 (- avg-quality 0.6)))
          enhanced-separation (min 0.95 (+ current-separation separation-boost))

          ; Update stability score based on live performance
          current-stability (get-in category-file [:summary :all :intelligence-metrics :stability-score] 0.0)
          stability-boost (min 0.05 (* 0.02 (count samples)))
          enhanced-stability (min 0.95 (+ current-stability stability-boost))

          ; Create enhanced category
          enhanced-category (update-in category-file [:summary :all :intelligence-metrics]
                                       merge
                                       {:separation-score enhanced-separation
                                        :stability-score enhanced-stability
                                        :live-enhanced true
                                        :last-enhancement (System/currentTimeMillis)
                                        :enhancement-samples (count samples)})]

      ; Save enhanced category
      (with-open [w (io/writer category-file)]
        (binding [*print-length* nil *print-level* nil]
          (pprint/pprint enhanced-category w)))

      ; Clear processed samples
      (swap! live-signature-buffer update-in [category direction]
             #(drop 5 %))

      (println "✨ Enhanced category with live data - new separation:" enhanced-separation))

    (catch Exception e
      (println "Error enhancing category with live data:" (.getMessage e)))))

(defn capture-live-enhancement!
  "Currently unimplemented, 
      but meant to be called in 'Process-live-training-integration!',
      in bci-integration, in the Pong example.

   Capture high-confidence sample for live signature enhancement."
  [category confidence-data eeg-sample game-context]
  (let [timestamp (System/currentTimeMillis)
        category-intelligence (lexi/load-category category)]
    (when (and category-intelligence
               (should-enhance-signature? confidence-data game-context category-intelligence))
      (let [dominant-direction (if (> (:up confidence-data) (:down confidence-data)) "up" "down")
            enhancement-sample {:timestamp timestamp
                                :confidence confidence-data
                                :eeg-sample eeg-sample
                                :game-context game-context
                                :quality-score (max (:up confidence-data) (:down confidence-data))}]

        (swap! live-signature-buffer assoc-in [category dominant-direction]
               (conj (get-in @live-signature-buffer [category dominant-direction] [])
                     enhancement-sample))

        (let [samples (get-in @live-signature-buffer [category dominant-direction])]
          (when (>= (count samples) 5)
            (enhance-category-live! category dominant-direction samples)))))))

(defn load-wave-signatures-from-dir
  "Load signature files from the specified category directory.
   
   Called in 'continue-with-matching' below."
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
  "Currently unimplemented...
   
   Compute basic time domain statistics for EEG data using BrainFlow's native implementation.
      raw-data is a sequence of samples, where each sample is a vector of channel values."
  [raw-data]
  (try
    ; Use BrainFlow's data-filter implementation for time domain stats
    (data-filter/compute-time-domain-stats raw-data)
    (catch Exception e
      (println "Error computing time domain stats:" (.getMessage e))
      {:channel-stats []})))

(defn compute-frequency-bands
  "Currently unimplemented...
   
   Compute frequency band powers for EEG data using BrainFlow's FFT capabilities"
  [raw-data sampling-rate]
  (when (seq raw-data)
    (try
      ; Use BrainFlow's native band power calculation
      (data-filter/calculate-band-powers raw-data sampling-rate)
      (catch Exception e
        (println "Error in compute-frequency-bands:" (.getMessage e))
        {:delta 0.0, :theta 0.0, :alpha 0.0, :beta 0.0, :gamma 0.0, :thinking 0.0}))))

(defn compute-wavelet-features
  "Currently unimplemented...
   
   Compute wavelet features for EEG data using BrainFlow's wavelet transform.
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
  "Currently unimplemented,
      but called in 'calculate-signature-similarity' below.
   
   Compare time domain features between current data and signature."
  [current-features signature-features]
  (try
    (if (or (nil? current-features) (nil? signature-features))
      0.0  ; Return zero similarity if either is nil
      (let [current-stats (:channel-stats current-features)
            signature-stats (:channel-stats signature-features)

            ; Ensure comparison of the same number of channels
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
  "Currently unimplemented, 
      but called in 'calculate-signature-similarity' below.
   
   Compare frequency domain features between current data and signature."
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
  "Currently unimplemented, 
      but called in 'calculate-signature-similarity' below.
   
   Compare wavelet features between current data and signature."
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
  "Currently unimplemented, 
      but called in 'calculate-signature-similarity' below.
   
   Compare spatial correlation features between current data and signature"
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

(defn compute-coherence-similarity
  "Currently unimplemented, 
      but called in 'calculate-signature-similarity' below.
   
   Calculate similarity between coherence patterns"
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

(defn gradient-field
  "Currently unimplemented...
   
   Compute a gradient field matrix from normalized multi-channel EEG data.
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

(defn normalize-band-powers
  "Currently unimplemented,
      but called in 'extract-comprehensive-features' below.
   
   Normalize band powers to create distribution ratios"
  [band-powers]
  (let [total-power (apply + (vals band-powers))]
    (if (> total-power 0)
      (into {} (map (fn [[k v]] [k (/ v total-power)]) band-powers))
      band-powers)))

(defn extract-features-from-eeg-data
  "Extract features using calibration module's robust band power extraction.
   
   Called in 'get-smoothed-features' below, and the pong example directly."
  [eeg-data sampling-rate]
  (try
    ;(println "=== EEG FEATURE EXTRACTION ===")
    ;(println "Input data type:" (type eeg-data))
    ;(println "Sample count:" (count eeg-data))

    (when (seq eeg-data)
      (let [; Extract EEG arrays from the data structure
            eeg-samples (mapv :eeg eeg-data)
            ; Flatten all samples into one continuous dataset
            all-samples (vec (apply concat eeg-samples))

            ; Filter out any invalid samples
            valid-samples (filterv #(and (sequential? %)
                                         (every? number? %)
                                         (= (count %) 5)) ; Ensure 5 channels
                                   all-samples)

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

(defn extract-comprehensive-features
  "Currently unimplemented...
   
   Extract features that align with category intelligence structure"
  [eeg-data sampling-rate]
  (let [basic-features (extract-features-from-eeg-data eeg-data sampling-rate)]
    (when basic-features
      (merge basic-features
             {:band-distribution (normalize-band-powers (:band-powers basic-features))
              :triangulation-ready true
              :feature-timestamp (System/currentTimeMillis)}))))

(defn calculate-band-power-similarity
  "Calculate similarity between current and signature band powers.
   
   Called in 'continue-with-matching' below."
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

(defn calculate-signature-similarity
  "Currently unimplemented...
   
   Calculate comprehensive similarity between current EEG features and stored signature"
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
  "Currently unimplemented...
   
   Calculate distance between two feature sets."
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

(defn calculate-golden-tensor-affinity
  "Calculate similarity to golden tensor patterns.
   
   Called in 'calculate-triangulation-score' below."
  [current-bands golden-ratios]
  (try
    (let [common-bands (filter #(and (contains? current-bands %)
                                     (contains? golden-ratios %))
                               [:delta :theta :alpha :beta :gamma :thinking])
          affinities (map (fn [band]
                            (let [current-val (get current-bands band 0)
                                  golden-avg (get-in golden-ratios [band :average] 0)
                                  golden-std (get-in golden-ratios [band :std-dev] 1.0)]
                              ; Normalize and calculate affinity
                              (if (> golden-std 0)
                                (max 0 (- 1.0 (/ (Math/abs (- current-val golden-avg)) golden-std)))
                                0.5)))
                          common-bands)]
      (if (seq affinities)
        (/ (apply + affinities) (count affinities))
        0.0))
    (catch Exception e
      (println "Error calculating golden tensor affinity:" (.getMessage e))
      0.0)))

(defn calculate-ratio-similarity
  "Compare current band distribution to signature ratios.
   
   Called in 'calculate-triangulation-score' below."
  [current-bands reference-ratios]
  (try
    (let [common-bands (filter #(and (contains? current-bands %)
                                     (contains? reference-ratios %))
                               [:delta :theta :alpha :beta :gamma :thinking])
          similarities (map (fn [band]
                              (let [current-val (get current-bands band 0)
                                    ref-avg (get-in reference-ratios [band :average] 0)
                                    ref-std (get-in reference-ratios [band :std-dev] 0.1)]
                                ; Gaussian-like similarity with std deviation consideration
                                (Math/exp (- (/ (Math/pow (- current-val ref-avg) 2)
                                                (* 2 (Math/pow ref-std 2)))))))
                            common-bands)]
      (if (seq similarities)
        (/ (apply + similarities) (count similarities))
        0.0))
    (catch Exception e
      (println "Error calculating ratio similarity:" (.getMessage e))
      0.0)))

(defn calculate-calibration-alignment
  "Calculate alignment with calibration factors.
   
   Called in 'calculate-triangulation-score' below."
  [current-bands calibration-factors]
  (try
    (let [common-bands (filter #(and (contains? current-bands %)
                                     (contains? calibration-factors %))
                               [:delta :theta :alpha :beta :gamma :thinking])
          alignments (map (fn [band]
                            (let [current-val (get current-bands band 0)
                                  cal-factor (get calibration-factors band 1.0)]
                              ; Simple alignment metric
                              (min 1.0 (if (> cal-factor 0)
                                         (/ (min current-val cal-factor)
                                            (max current-val cal-factor))
                                         0.5))))
                          common-bands)]
      (if (seq alignments)
        (/ (apply + alignments) (count alignments))
        0.5))
    (catch Exception e
      (println "Error calculating calibration alignment:" (.getMessage e))
      0.5)))

(defn calculate-confidence
  "Currently unimplemented...
   
   Calculate confidence using category intelligence metrics"
  [scores category-intelligence]
  (try
    (let [intelligence-metrics (get-in category-intelligence [:summary :all :intelligence-metrics])
          separation-score (or (:separation-score intelligence-metrics) 0.5)
          triangulation-quality (or (:triangulation-quality intelligence-metrics) 1.0)
          stability-score (or (:stability-score intelligence-metrics) 0.0)

          ; Dynamic threshold based on intelligence
          base-threshold (if (> separation-score 0.6) 0.02 0.05)
          quality-boost (* triangulation-quality 0.3)
          stability-adjustment (if (> stability-score 0) 0.1 -0.05)

          dynamic-threshold (max 0.01 (+ base-threshold quality-boost stability-adjustment))

          ; Enhanced confidence calculation
          up-score (:up scores)
          down-score (:down scores)
          confidence-gap (Math/abs (- up-score down-score))

          ; Boost confidence when there's clear separation
          separation-multiplier (if (> confidence-gap dynamic-threshold)
                                  (+ 1.0 (* separation-score 0.5))
                                  1.0)

          up (* up-score separation-multiplier)
          down (* down-score separation-multiplier)]

      ;(println "Enhanced confidence calculation:")
      ;(println "  Separation score:" separation-score)
      ;(println "  Triangulation quality:" triangulation-quality)
      ;(println "  Dynamic threshold:" dynamic-threshold)
      ;(println "  Confidence gap:" confidence-gap)
      ;(println "  Separation multiplier:" separation-multiplier)
      ;(println "  Enhanced scores - Up:" up "Down:" down)

      {:up up
       :down down
       :confidence-gap confidence-gap
       :dynamic-threshold dynamic-threshold
       :intelligence-grade (get-in category-intelligence [:summary :all :signature-quality :overall-grade])})
    (catch Exception e
      (println "Error in confidence calculation:" (.getMessage e))
      scores)))

(defn calculate-confidence-boost
  "Currently unimplemented, but called in 'update-signature-with-live-data!' below.
      -> 'update-signature-with-live-data!' is not yet implemented...
   
   Calculate how much to boost confidence based on live samples"
  [samples existing-signature]
  (let [avg-live-confidence (/ (reduce + (map :quality-score samples)) (count samples))
        existing-confidence (get-in existing-signature [:confidence-metrics :avg-confidence] 0.3)
        boost-factor (min 0.15 (* 0.1 (- avg-live-confidence existing-confidence)))]
    (max 0.0 boost-factor)))

(defn update-summary-with-live-data
  "Currently unimplemented, but called in 'update-category-from-live-data!' below.
      -> 'update-category-from-live-data!' is not yet implemented...

   Update category summary with live intelligence data"
  [existing-summary live-intelligence]
  (let [current-all (get existing-summary :all {})
        current-metrics (get current-all :intelligence-metrics {})
        updated-metrics (merge current-metrics live-intelligence)]

    (assoc existing-summary
           :all (assoc current-all :intelligence-metrics updated-metrics)
           :live-updated (System/currentTimeMillis))))

(defn calculate-live-metrics
  "Currently unimplemented, but called in 'update-category-from-live-data!' below.
      -> 'update-category-from-live-data!' is not yet implemented...

   Calculate bci performance metrics from live data."
  [up-signature down-signature live-samples]
  (try
    (let [; Extract existing metrics
          up-confidence (get-in up-signature [:confidence-metrics :avg-confidence] 0.3)
          down-confidence (get-in down-signature [:confidence-metrics :avg-confidence] 0.3)

          ; Calculate live metrics
          live-confidences (map :quality-score live-samples)
          avg-live-confidence (if (seq live-confidences)
                                (/ (reduce + live-confidences) (count live-confidences))
                                0.3)

          ; Enhanced separation based on live performance
          confidence-gap (Math/abs (- up-confidence down-confidence))
          live-separation-boost (min 0.1 (* 0.05 (- avg-live-confidence 0.5)))
          enhanced-separation (min 0.95 (+ confidence-gap live-separation-boost))

          ; Stability based on consistency of live samples
          confidence-variance (if (> (count live-confidences) 1)
                                (let [mean avg-live-confidence
                                      squared-diffs (map #(Math/pow (- % mean) 2) live-confidences)]
                                  (/ (reduce + squared-diffs) (count live-confidences)))
                                0.1)
          stability-score (max 0.0 (- 1.0 (* confidence-variance 2)))

          ; Quality score based on live performance
          triangulation-quality (min 1.0 (+ 0.5 (* avg-live-confidence 0.5)))]

      {:separation-score enhanced-separation
       :stability-score stability-score
       :triangulation-quality triangulation-quality
       :live-enhanced true
       :live-sample-count (count live-samples)
       :avg-live-confidence avg-live-confidence})

    (catch Exception e
      (println "Error calculating live intelligence metrics:" (.getMessage e))
      {:separation-score 0.5
       :stability-score 0.0
       :triangulation-quality 1.0
       :live-enhanced false})))

(defn update-category!
  "Update category.edn with new performance data and intelligence metrics.
   
   Called in 'perform-category-update-with-timeout' below."
  [category confidence-data eeg-sample game-context recent-performance]
  (try
    ; Add safety checks at the beginning
    (when (or (nil? confidence-data)
              (not (and (contains? confidence-data :up)
                        (contains? confidence-data :down))))
      (throw (Exception. "Invalid confidence data provided")))

    ; Add file locking to prevent concurrent writes
    (let [profile-name (:name ((:get-active-profile @state/state)))
          lexicon-path (fio/get-wave-lexicon-dir profile-name "")
          category-dir (str lexicon-path "/" category)
          category-path (str category-dir "/category.edn")
          lock-path (str category-path ".lock")]

      ; File lock mechanism
      (when (.exists (java.io.File. lock-path))
        (println "Category file is locked, skipping update")
        {:success false :error "File locked"})

      ; Create lock file
      (try
        (spit lock-path (str (System/currentTimeMillis)))

        (let [; Load existing category data with better error handling
              existing-data (try
                              (if (.exists (java.io.File. category-path))
                                (edn/read-string (slurp category-path))
                                ; Default structure matching your original format
                                {:stats {:category category
                                         :instance-count 0
                                         :signature-count 0
                                         :has-template false
                                         :last-updated nil}
                                 :summary {:all {:intelligence-metrics {}
                                                 :signature-quality {}
                                                 :recording-count 0}}
                                 :performance-history []
                                 :updated-at (java.util.Date.)})
                              (catch Exception e
                                (println "Error reading existing category data, creating new:" (.getMessage e))
                                {:stats {:category category
                                         :instance-count 0
                                         :signature-count 0
                                         :has-template false
                                         :last-updated nil}
                                 :summary {:all {:intelligence-metrics {}
                                                 :signature-quality {}
                                                 :recording-count 0}}
                                 :performance-history []
                                 :updated-at (java.util.Date.)}))

              ; Calculate new performance metrics
              current-time (System/currentTimeMillis)
              current-date (java.util.Date.)
              performance-entry {:timestamp current-time
                                 :confidence confidence-data
                                 :game-context game-context
                                 :recent-performance recent-performance
                                 :eeg-sample-available (not (nil? eeg-sample))}

              ; Update performance history (keep last 100 entries)
              updated-history (take-last 100
                                         (conj (or (:performance-history existing-data) [])
                                               performance-entry))

              ; Calculate updated intelligence metrics with null checks
              avg-confidence (let [up (or (:up confidence-data) 0.0)
                                   down (or (:down confidence-data) 0.0)]
                               (/ (+ up down) 2))
              confidence-gap (let [up (or (:up confidence-data) 0.0)
                                   down (or (:down confidence-data) 0.0)]
                               (Math/abs (- up down)))

              ; Update intelligence metrics with running averages
              old-metrics (get-in existing-data [:summary :all :intelligence-metrics] {})
              old-count (get old-metrics :sample-count 0)
              new-count (inc old-count)

              ; Running average calculations with safety checks
              old-separation (get old-metrics :separation-score 0.5)
              new-separation (if (> new-count 0)
                               (/ (+ (* old-separation old-count) confidence-gap) new-count)
                               confidence-gap)

              old-stability (get old-metrics :stability-score 0.0)
              stability-contribution (if (> avg-confidence 0.1) 0.8 0.2)
              new-stability (if (> new-count 0)
                              (/ (+ (* old-stability old-count) stability-contribution) new-count)
                              stability-contribution)

              ; Update triangulation quality based on consistency
              old-triangulation (get old-metrics :triangulation-quality 0.5)
              triangulation-contribution (if (> confidence-gap 0.05) 0.9 0.3)
              new-triangulation (if (> new-count 0)
                                  (/ (+ (* old-triangulation old-count) triangulation-contribution) new-count)
                                  triangulation-contribution)

              ; Calculate dynamic threshold based on separation score
              ; This is what updates the front-end threshold!
              dynamic-threshold (cond
                                  (> new-separation 0.8) 0.9   ; High separation = high threshold
                                  (> new-separation 0.6) 0.75  ; Good separation = medium-high threshold
                                  (> new-separation 0.4) 0.6   ; Moderate separation = medium threshold
                                  (> new-separation 0.2) 0.45  ; Low separation = lower threshold
                                  :else 0.3)                   ; Poor separation = low threshold

              ; Preserve existing structure and add intelligence updates
              updated-data (-> existing-data
                               ; Update stats with current timestamp
                               (assoc-in [:stats :last-updated] current-date)
                               ; Update summary timestamps
                               (assoc-in [:summary :all :updated-at] current-date)
                               (assoc :updated-at current-date)
                               ; Add performance history
                               (assoc :performance-history updated-history)
                               ; Update intelligence metrics
                               (assoc-in [:summary :all :recording-count] new-count)
                               (assoc-in [:summary :all :intelligence-metrics :separation-score] new-separation)
                               (assoc-in [:summary :all :intelligence-metrics :stability-score] new-stability)
                               (assoc-in [:summary :all :intelligence-metrics :triangulation-quality] new-triangulation)
                               (assoc-in [:summary :all :intelligence-metrics :sample-count] new-count)
                               ; Update dynamic threshold
                               (assoc-in [:summary :all :dynamic-threshold] dynamic-threshold)
                               ; Update signature quality
                               (assoc-in [:summary :all :signature-quality :overall-grade]
                                         (cond
                                           (> new-separation 0.8) :excellent
                                           (> new-separation 0.6) :good
                                           (> new-separation 0.4) :moderate
                                           :else :poor)))]

          ; Ensure directory exists
          (.mkdirs (java.io.File. category-dir))

          ; Write updated data with proper error handling and file operations
          (let [temp-path (str category-path ".tmp")
                temp-file (java.io.File. temp-path)
                target-file (java.io.File. category-path)]

            (println "Writing to temp file:" temp-path)

            ; Write to temp file with proper error handling
            (try
              (with-open [w (io/writer temp-file)]
                (binding [*out* w]
                  (pprint/pprint updated-data)))

              (println "Successfully wrote temp file, size:" (.length temp-file) "bytes")

              ; Use proper file operations for Windows
              (cond
                ; If target doesn't exist, simple rename
                (not (.exists target-file))
                (do
                  (println "Target file doesn't exist, renaming temp file")
                  (if (.renameTo temp-file target-file)
                    (println "✅ Successfully renamed temp file to target")
                    (do
                      (println "❌ Rename failed, trying copy approach")
                      ; Copy and delete otherwise
                      (io/copy temp-file target-file)
                      (.delete temp-file)
                      (println "✅ Successfully copied temp file and deleted original"))))

                ; If target exists, be more careful on Windows
                :else
                (do
                  (println "Target file exists, using Windows-safe replacement")
                  (let [backup-path (str category-path ".backup")
                        backup-file (java.io.File. backup-path)]

                    ; Create backup of original
                    (when (.exists target-file)
                      (println "Creating backup of original file")
                      (io/copy target-file backup-file))

                    ; Try to delete original (Windows requirement)
                    (if (.delete target-file)
                      (do
                        (println "Deleted original file, renaming temp")
                        (if (.renameTo temp-file target-file)
                          (do
                            (println "✅ Successfully replaced file")
                            ; Clean up backup on success
                            (.delete backup-file))
                          (do
                            (println "❌ Rename failed even after delete, restoring backup")
                            ; Restore backup if rename failed
                            (.renameTo backup-file target-file)
                            (.delete temp-file)
                            (throw (Exception. "Could not rename temp file even after deleting original")))))
                      (do
                        (println "❌ Could not delete original file, using copy approach")
                        ; If we can't delete original, force copy over it
                        (io/copy temp-file target-file)
                        (.delete temp-file)
                        (.delete backup-file) ; Clean up backup
                        (println "✅ Successfully force-copied over original file"))))))

              ; Verify the final file exists and has content
              (if (.exists target-file)
                (do
                  (println "✅ Final verification: target file exists, size:" (.length target-file) "bytes")
                  ; Clean up any remaining temp files
                  (when (.exists temp-file)
                    (.delete temp-file))
                  (when (.exists (java.io.File. (str category-path ".backup")))
                    (.delete (java.io.File. (str category-path ".backup")))))
                (throw (Exception. "Target file does not exist after write operation!")))

              (catch Exception e
                (println "❌ Error during file write operation:" (.getMessage e))
                ; Clean up temp file on error
                (when (.exists temp-file)
                  (.delete temp-file))
                (throw e))))

          (println "Updated category intelligence:")
          (println "  Separation score:" (format "%.3f" new-separation))
          (println "  Stability score:" (format "%.3f" new-stability))
          (println "  Triangulation quality:" (format "%.3f" new-triangulation))
          (println "  Dynamic threshold:" (format "%.3f" dynamic-threshold))

          {:success true
           :updated-at current-time
           :metrics {:separation-score new-separation
                     :stability-score new-stability
                     :triangulation-quality new-triangulation
                     :dynamic-threshold dynamic-threshold
                     :sample-count new-count}})

        (finally
          ; Always remove the lock file
          (.delete (java.io.File. lock-path)))))

    (catch Exception e
      (println "Error updating category intelligence:" (.getMessage e))
      (.printStackTrace e)
      {:success false :error (.getMessage e)})))

(defn perform-category-update-with-timeout
  "Perform the actual category update with internal timeout.
   
   Called in 'update-category-thread-safe!' below."
  [category confidence-data eeg-sample game-context recent-performance timeout-ms]
  (let [start-time (System/currentTimeMillis)
        deadline (+ start-time timeout-ms)]
    (try
      (println "Starting category update for:" category)

      ; Check timeout helper function
      (letfn [(check-timeout! []
                (when (> (System/currentTimeMillis) deadline)
                  (throw (Exception. "Operation timeout exceeded"))))]

        ; Validate inputs
        (when (or (nil? confidence-data)
                  (nil? (:up confidence-data))
                  (nil? (:down confidence-data)))
          (throw (Exception. "Invalid confidence data")))

        (check-timeout!)

        ; Load existing category intelligence
        (let [category-intelligence (try
                                      (println "Loading category intelligence...")
                                      (lexi/load-category category)
                                      (catch Exception e
                                        (println "Could not load existing category:" (.getMessage e))
                                        nil))]

          (check-timeout!)

          ; Prepare the update data
          (let [update-data {:confidence-data confidence-data
                             :eeg-sample eeg-sample
                             :game-context game-context
                             :recent-performance recent-performance
                             :timestamp (System/currentTimeMillis)
                             :category category}]

            (check-timeout!)

            ; Call your actual category update function
            (println "Performing file update for category:" category)

            (let [success? (try
                             ;; This calls your existing update-category! function
                             (let [result (update-category! category confidence-data eeg-sample
                                                            game-context recent-performance)]
                               (check-timeout!)
                               (:success result))

                             (catch Exception e
                               (println "❌ File update error:" (.getMessage e))
                               false))]

              (if success?
                {:success true :updated-at (System/currentTimeMillis)}
                {:success false :error "file-update-failed"})))))

      (catch Exception e
        (let [elapsed (- (System/currentTimeMillis) start-time)]
          (println "❌ Category update error after" elapsed "ms:" (.getMessage e))
          {:success false :error (.getMessage e) :elapsed-ms elapsed})))))

(defn update-category-thread-safe!
  "Main category update function, per-category ReentrantLock with timeout.
   
   This is called in apps built upon brainfloj to enhance the category.edn files
   with live streamed information when appropriate.
      Currently called in the Pong example in 'start-signature-enhancement-background!'
      and 'match-brain-activity-server'."
  [category confidence-data eeg-sample game-context recent-performance]
  (println "Starting thread-safe update for category:" category)

  (let [category-lock (get-category-lock category)
        current-time (System/currentTimeMillis)]

    (println "  Lock object:" (str category-lock))
    (println "  Current time:" current-time)
    (println "  Thread interrupted status:" (.isInterrupted (Thread/currentThread)))

    ; Try to acquire lock with timeout
    (try
      (println "Attempting to acquire lock...")

      ; Clear interrupted status before attempting lock
      (Thread/interrupted) ; This clears the interrupted flag

      (if (.tryLock category-lock 5 TimeUnit/SECONDS) ; Reduced timeout to 5 seconds
        (try
          (println "Acquired lock for category:" category)

          ;; Perform the actual update with internal timeout
          (println "Starting file update process...")
          (let [update-result (perform-category-update-with-timeout
                               category confidence-data eeg-sample
                               game-context recent-performance 4000)] ; Reduced to 4 seconds

            (println "File update result:" update-result)

            ;; Record the result for circuit breaker
            (record-category-update-result category (:success update-result))

            (if (:success update-result)
              (println "Category update completed for:" category)
              (println "Category update failed for:" category (:error update-result)))

            update-result)

          (catch Exception e
            (println "Exception during category update:" (.getMessage e))
            (.printStackTrace e)
            (record-category-update-result category false)
            {:success false :error (.getMessage e)})

          (finally
            (.unlock category-lock)
            (println "RELEASED LOCK for category:" category)))

        (do
          (println "  Could not acquire lock for category:" category "within 5 seconds")
          (println "  Lock state - isLocked:" (.isLocked category-lock))
          (println "  Lock state - hasQueuedThreads:" (.hasQueuedThreads category-lock))
          (println "  Lock state - holdCount:" (.getHoldCount category-lock))
          (record-category-update-result category false)
          {:success false :error "lock-timeout"}))

      (catch InterruptedException e
        (println "  Thread interrupted while waiting for lock:" category)
        (println "  Thread name:" (.getName (Thread/currentThread)))
        (println "  Thread state:" (.getState (Thread/currentThread)))
        (println "  Interrupted status:" (.isInterrupted (Thread/currentThread)))

        ; Clear the interrupted status and try a non-blocking approach
        (Thread/interrupted)

        ; Try immediate lock without waiting
        (if (.tryLock category-lock)
          (try
            (println "Acquired lock immediately after interrupt for:" category)
            (let [update-result (perform-category-update-with-timeout
                                 category confidence-data eeg-sample
                                 game-context recent-performance 2000)] ; Very short timeout
              (record-category-update-result category (:success update-result))
              update-result)
            (finally
              (.unlock category-lock)
              (println "Released lock after interrupt recovery for:" category)))
          (do
            (println "Could not acquire lock even after interrupt, giving up")
            (record-category-update-result category false)
            {:success false :error "interrupted-no-recovery"})))

      (catch Exception e
        (println "Unexpected error in update-category-thread-safe!:" (.getMessage e))
        (.printStackTrace e)
        (record-category-update-result category false)
        {:success false :error (.getMessage e)}))))

(defn update-category-from-live-data!
  "Currently unimplemented, but called in 'update-signature-with-live-data!' below.
       -> 'update-signature-with-live-data!' is itself unimplemented thus far

   Update category.edn based on live signature updates"
  [category]
  (try
    (let [existing-category (lexi/load-category category)
          ; Load updated signature data
          up-signature (lexi/load-signature-data category "up")
          down-signature (lexi/load-signature-data  category "down")
          ; Calculate new bci performance metrics with live data
          live-metrics (calculate-live-metrics up-signature down-signature)
          ; Update category with live intelligence
          updated-category (merge existing-category
                                  {:live-intelligence live-metrics
                                   :last-live-update (System/currentTimeMillis)
                                   :summary (update-summary-with-live-data
                                             (:summary existing-category)
                                             live-metrics)})]
      ; Save updated category
      (with-open [w (io/writer existing-category)]
        (binding [*print-length* nil *print-level* nil]
          (pprint/pprint updated-category w)))
      (println "Updated category.edn with live intelligence data"))
    (catch Exception e
      (println "Error updating category with live data:" (.getMessage e)))))

(defn calculate-direction-confidence
  "Currently unimplemented...
   
   Calculate confidence for a specific direction with direction-specific data"
  [current-features category-intelligence direction]
  (try
    (let [direction-key (keyword direction)
          current-bands (:band-powers current-features)

          ; Try to get direction-specific data first, then fall back to :all
          direction-data (get-in category-intelligence [:summary direction-key])
          fallback-data (get-in category-intelligence [:summary :all])

          ; Use direction-specific data if available, otherwise use :all
          reference-data (or direction-data fallback-data)

          ; Extract reference data
          reference-bands (get-in reference-data [:band-powers :average])
          reference-std (get-in reference-data [:band-powers :std-dev])

          ; Add direction-specific weighting to differentiate up vs down
          direction-weight (case direction
                             "up" {:delta 1.2, :theta 0.8, :alpha 1.1, :beta 1.0, :gamma 0.9, :thinking 1.1}
                             "down" {:delta 0.8, :theta 1.2, :alpha 0.9, :beta 1.0, :gamma 1.1, :thinking 0.9}
                             {:delta 1.0, :theta 1.0, :alpha 1.0, :beta 1.0, :gamma 1.0, :thinking 1.0})

          ; Calculate weighted similarity
          similarity-score (if (and current-bands reference-bands)
                             (let [bands [:delta :theta :alpha :beta :gamma :thinking]
                                   similarities (for [band bands]
                                                  (let [current-val (get current-bands band 0.0)
                                                        ref-val (get reference-bands band 0.0)
                                                        ref-std (get reference-std band 1.0)
                                                        weight (get direction-weight band 1.0)]
                                                    (if (and (> ref-val 0) (> ref-std 0))
                                                      (* weight (Math/exp (- (/ (Math/pow (- current-val ref-val) 2)
                                                                                (* 2 (Math/pow ref-std 2))))))
                                                      0.0)))]
                               (/ (reduce + similarities) (count similarities)))
                             0.0)

          ; Get calibration factors with direction-specific adjustment
          calibration-factors (get-in reference-data [:calibration-factors :average])
          calibration-boost (if calibration-factors
                              (let [boost-sum (reduce + (for [band [:delta :theta :alpha :beta :gamma :thinking]]
                                                          (let [weight (get direction-weight band 1.0)]
                                                            (* weight
                                                               (get current-bands band 0.0)
                                                               (get calibration-factors band 1.0)))))]
                                (/ boost-sum 6.0))
                              0.0)

          ; Calculate final confidence with direction bias
          base-confidence (* similarity-score 0.7)
          calibration-confidence (* calibration-boost 0.3)
          final-confidence (+ base-confidence calibration-confidence)]

      ;(println (str "Direction " direction " confidence calculation:"))
      ;(println "  Similarity score:" (format "%.4f" similarity-score))
      ;(println "  Calibration boost:" (format "%.4f" calibration-boost))
      ;(println "  Final confidence:" (format "%.4f" final-confidence))
      ;(println "  Using direction-specific data:" (some? direction-data))

      (max 0.0 final-confidence))
    (catch Exception e
      (println (str "Error calculating confidence for " direction ": " (.getMessage e)))
      0.0)))

(defn calculate-triangulation-score
  "Currently unimplemented,
      but called in 'process-live-sample' below, which is also yet to be implemented.
   
   Calculate similarity using triangulation analysis from category intelligence."
  [current-features category-intelligence direction]
  (try
    (let [triangulation-data (get-in category-intelligence [:summary :all :triangulation-analysis])
          current-bands (:band-distribution current-features)
          reference-ratios (get-in triangulation-data [:signature-ratios])
          golden-ratios (get-in triangulation-data [:golden-tensor-ratios])
          calibration-factors (get-in category-intelligence [:summary :all :calibration-factors :average])]

      (when (and triangulation-data current-bands reference-ratios)
        (let [; Multi-dimensional similarity scoring
              signature-similarity (calculate-ratio-similarity current-bands reference-ratios)
              golden-tensor-affinity (calculate-golden-tensor-affinity current-bands golden-ratios)
              calibration-alignment (calculate-calibration-alignment current-bands calibration-factors)

              ; Weighted combination based on category intelligence
              triangulation-quality (get-in triangulation-data [:triangulated-strength :vs-golden-tensor :average] 1.0)
              separation-score (get-in category-intelligence [:summary :all :intelligence-metrics :separation-score] 0.5)

              ; Dynamic weighting based on intelligence metrics
              signature-weight (if (> separation-score 0.6) 0.4 0.5)
              golden-weight (min 0.4 (* triangulation-quality 0.3))
              calibration-weight (- 1.0 signature-weight golden-weight)

              combined-score (+ (* signature-similarity signature-weight)
                                (* golden-tensor-affinity golden-weight)
                                (* calibration-alignment calibration-weight))]

          (println (str "Triangulation score for " direction ":"))
          (println "  Signature similarity:" signature-similarity)
          (println "  Golden tensor affinity:" golden-tensor-affinity)
          (println "  Calibration alignment:" calibration-alignment)
          (println "  Combined score:" combined-score)

          combined-score)))
    (catch Exception e
      (println "Error in triangulation scoring:" (.getMessage e))
      0.0)))

   (defn calculate-recent-performance-server
     "Currently unimplemented...
      
      Server-side calculation of recent performance metrics"
     [profile-name category]
     (try
       (let [current-time (System/currentTimeMillis)
             recent-window 30000 ; 30 seconds
             
             ; Get action history from state (you'll need to track this server-side)
             action-history (get-in @state/state [:bci :action-history profile-name] [])
             
             ; Filter recent actions
             recent-actions (filter #(< (- current-time (:timestamp %)) recent-window) action-history)
             
             total-actions (count recent-actions)
             successful-actions (count (filter :successful recent-actions))
             assisted-actions (count (filter :assisted recent-actions))
             natural-actions (- successful-actions assisted-actions)
             
             success-rate (if (> total-actions 0) 
                           (/ successful-actions total-actions) 
                           0.5)
             
             ; Calculate additional server-side metrics
             confidence-history (get-in @state/state [:bci :confidence-history profile-name] [])
             recent-confidences (filter #(< (- current-time (:timestamp %)) recent-window) confidence-history)
             
             avg-confidence (if (seq recent-confidences)
                             (/ (reduce + (map :max-confidence recent-confidences)) 
                                (count recent-confidences))
                             0.3)]
         
         {:success-rate success-rate
          :total-actions total-actions
          :successful-actions successful-actions
          :assisted-actions assisted-actions
          :natural-actions natural-actions
          :avg-confidence avg-confidence
          :recent-sample-count (count recent-confidences)})
       
       (catch Exception e
         (println "Error calculating server performance:" (.getMessage e))
         {:success-rate 0.5 :total-actions 0 :successful-actions 0 
          :assisted-actions 0 :natural-actions 0 :avg-confidence 0.3})))

(defn process-live-sample
  "Currently unimplemented, but called in 'update-signature-with-live-data!' below.
      -> 'update-signature-with-live-data!' is not yet implemented...
   
   Process a live sample into signature format"
  [sample]
  (let [sampling-rate (api/get-current-sample-rate)]
    {:timestamp (:timestamp sample)
     :confidence-peak (:quality-score sample)
     :band-powers (calibration/extract-band-powers (:eeg-sample sample) sampling-rate)
     :triangulation-score (calculate-triangulation-score (:confidence sample))
     :game-validated true}))

(defn update-signature-with-live-data!
  "Currently unimplemented, but called in 'capture-live-signature-sample!' below.
   
   Update signature.edn with live high-confidence data"
  [category signature samples]
  (try
    (let [existing-signature (lexi/load-signature-data category signature)

          ; Process live samples into signature format
          processed-samples (map process-live-sample samples)

          ; Create enhanced signature data
          updated-signature (merge existing-signature
                                   {:live-updates {:samples processed-samples
                                                   :last-update (System/currentTimeMillis)
                                                   :update-count (inc (get-in existing-signature [:live-updates :update-count] 0))}
                                    :confidence-boost (calculate-confidence-boost samples existing-signature)})]
      ; Save updated signature
      (with-open [w (io/writer existing-signature)]
        (binding [*print-length* nil *print-level* nil]
          (pprint/pprint updated-signature w)))
      ; Trigger category update
      (future
        (Thread/sleep 100) ; Small delay to avoid race conditions
        (update-category-from-live-data! category))

      ; Clear processed samples from buffer
      (swap! live-signature-buffer update-in [category signature]
             #(drop 3 %))

      (println "Updated signature with live data:" signature "- new confidence boost available"))

    (catch Exception e
      (println "Error updating signature with live data:" (.getMessage e)))))

(defn capture-live-signature-sample!
  "Currently unimplemented...
   
   Capture a high-confidence sample during gameplay."
  [profile-name category signature-type confidence-data eeg-sample game-context]
  (let [timestamp (System/currentTimeMillis)
        sample-key (str category "/" signature-type "/" timestamp)

        ; Extract current calibration data (same as regular recording)
        current-calibration (get-in @state/state [:calibration])

        ; Create a mini-signature sample
        signature-sample {:timestamp timestamp
                          :confidence confidence-data
                          :eeg-sample eeg-sample
                          :calibration-index current-calibration
                          :game-context game-context
                          :signature-type signature-type
                          :category category
                          :quality-score (max (:up confidence-data) (:down confidence-data))}]

    ; Add to live buffer
    (swap! live-signature-buffer assoc-in [profile-name category signature-type]
           (conj (get-in @live-signature-buffer [profile-name category signature-type] [])
                 signature-sample))

    ; Trigger update if we have enough samples
    (let [samples (get-in @live-signature-buffer [profile-name category signature-type])]
      (when (>= (count samples) 3) ; Minimum samples for update
        (update-signature-with-live-data! profile-name category signature-type samples)))))

(defn start-eeg-streaming-server 
  "Begin streaming life eeg data directly into a specialized category recording.
   
   Meant to be called by applications built on brainfloj, and called in the Pong
      example in the 'AsyncBCIStateManager' in the components.cljc file."
  []
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

(defn stop-eeg-streaming-server
  "Complete streaming life eeg data directly into a specialized category recording.
     
   Meant to be called by applications built on brainfloj, and called in the Pong
      example in the 'AsyncBCIStateManager' in the components.cljc file."
  []
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

(defn calculate-feature-similarity 
  "Currently unimplemented...
   
   Calculate similarity between two feature sets using simple correlation"
  [features1 features2]
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
  "Get recent EEG data with improved validation.
   
   Called in applications built upon brainflow.
      Called in the Pong example in 'start-signature-enhancement-background!',
      'match-brain-activity-server', and 'start-eeg-ingestion!'."
  [seconds]
  (try
    (let [current-time (System/currentTimeMillis)
          eeg-data @state/eeg-data]

      ; Early validation
      (when (< (count eeg-data) 8)
        (println "WARNING: Insufficient total data points")
        (throw (Exception. "Insufficient data points for processing")))

      ; Debug the actual structure
      (when (seq eeg-data)
        (println "First data point type:" (type (first eeg-data)))
        (println "First data point:" (first eeg-data))
        (if (map? (first eeg-data))
          (println "Data contains maps with keys:" (keys (first eeg-data)))
          (println "Data contains vectors/other, length:" (count (first eeg-data)))))

      ; The data is already time-ordered, so take the last N samples
      (let [sample-count (max 8 (min 20 (count eeg-data))) ; Take 8-20 most recent samples
            recent-data (vec (take-last sample-count eeg-data))]

        (println "Taking" sample-count "most recent samples")
        (println "Recent data count:" (count recent-data))

        recent-data))
    (catch Exception e
      (println "Error in get-recent-data:" (.getMessage e))
      [])))

(defn get-recent-data-with-even-samples
  "Get recent EEG data ensuring even number of samples for FFT.
   
   Called in 'match-brain-activity' below."
  [seconds]
  (let [current-time (System/currentTimeMillis)
        cutoff-time (- current-time (* seconds 1000))
        eeg-data @state/eeg-data
        fresh-cutoff (- current-time (* 1.0 1000))]
    
    (when eeg-data
      (let [recent (filterv #(> (:timestamp %) cutoff-time) eeg-data)
            fresh-recent (filterv #(> (:timestamp %) fresh-cutoff) recent)
            ; Ensure even number of samples
            sample-count (count fresh-recent)
            even-count (if (even? sample-count) sample-count (dec sample-count))
            even-samples (take even-count fresh-recent)]

        (if (>= even-count 8) ; Minimum 8 samples for reliable FFT
          (vec even-samples)
          (do
            (println "WARNING: Not enough even samples for FFT")
            []))))))

(defn calculate-overall-confidence
  "Calculate overall confidence score from up/down scores.
   
   I don't call this here, but I am calling it in the Pong game in 'match-brain-activity-server'. 
      I may change that and implement it here in the future."
  [up-score down-score category-intelligence]
  (try
    (let [intelligence-metrics (get-in category-intelligence [:summary :all :intelligence-metrics])
          separation-score (or (:separation-score intelligence-metrics) 0.5)
          triangulation-quality (or (:triangulation-quality intelligence-metrics) 1.0)
          stability-score (or (:stability-score intelligence-metrics) 0.0)

          ; Calculate confidence based on the strength of the stronger signal
          max-score (max up-score down-score)
          min-score (min up-score down-score)
          signal-strength max-score

          ; Calculate separation (how different the two scores are)
          separation (Math/abs (- up-score down-score))

          ; Overall confidence combines signal strength and separation
          base-confidence (* signal-strength 0.6)
          separation-confidence (* separation 0.4)
          raw-confidence (+ base-confidence separation-confidence)

          ; Apply intelligence metrics weighting
          intelligence-weight (+ (* separation-score 0.3)
                                 (* (min triangulation-quality 1.0) 0.4)
                                 (* (max 0.0 stability-score) 0.3))

          final-confidence (* raw-confidence intelligence-weight)]

      (println "=== OVERALL CONFIDENCE CALCULATION ===")
      (println "Signal strength:" (format "%.4f" signal-strength))
      (println "Separation:" (format "%.4f" separation))
      (println "Raw confidence:" (format "%.4f" raw-confidence))
      (println "Intelligence weight:" (format "%.4f" intelligence-weight))
      (println "Final confidence:" (format "%.4f" final-confidence))

      (max 0.0 (min 1.0 final-confidence)))
    (catch Exception e
      (println "Error calculating overall confidence:" (.getMessage e))
      0.0)))


(defn get-smoothed-features
  "Provides more accurate live data by averaging recent data over a sliding window.
   
   Called in 'match-brain-activity' below."
  [total-seconds window-count sampling-rate]
  (try
    (let [eeg-data @state/eeg-data]

      ; Early return if not enough data
      (when (< (count eeg-data) (* window-count 4)) ; Reduced minimum per window
        (println "Insufficient data for smoothing. Need:" (* window-count 4) "Have:" (count eeg-data))
        (throw (Exception. "Insufficient data for smoothing")))

      ; Create windows by dividing recent data
      (let [data-per-window (max 4 (quot (count eeg-data) window-count))
            windows (for [i (range window-count)
                          :let [start-idx (* i data-per-window)
                                end-idx (min (count eeg-data) (+ start-idx data-per-window))
                                window-data (subvec (vec eeg-data) start-idx end-idx)]]
                      (when (>= (count window-data) 4) ; Minimum samples per window
                        window-data))

            valid-windows (filter some? windows)]

        ; Need at least 2 valid windows to proceed
        (when (< (count valid-windows) 2)
          (println "Not enough valid windows for smoothing")
          (throw (Exception. "Insufficient valid windows for smoothing")))

        (let [feature-sets (keep (fn [window-data]
                                   (try
                                     (extract-features-from-eeg-data window-data sampling-rate)
                                     (catch Exception e
                                       (println "Window feature extraction error:" (.getMessage e))
                                       nil)))
                                 valid-windows)]

          (when (< (count feature-sets) 2)
            (println "Not enough feature sets extracted")
            (throw (Exception. "Insufficient feature sets for smoothing")))

          (let [band-power-sets (map :band-powers feature-sets)
                all-keys (set (mapcat keys band-power-sets))
                weights (map #(Math/pow 0.8 %) (range (count band-power-sets)))
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
            {:band-powers averaged-bands
             :timestamp (System/currentTimeMillis)
             :window-count (count valid-windows)
             :total-samples (reduce + (map count valid-windows))}))))
    (catch Exception e
      (println "Error in smoothed features:" (.getMessage e))
      nil)))

(defn get-realtime-intelligence-status
  "Currently unimplemented...
   
   Provides 'metametrics' about the category bci performance over time."
  [category]
  (try
    (let [category-intelligence (lexi/load-category category)]
      (if category-intelligence
        (let [metrics (get-in category-intelligence [:summary :all :intelligence-metrics])
              quality (get-in category-intelligence [:summary :all :signature-quality])]
          {:available true
           :separation-score (:separation-score metrics 0.0)
           :triangulation-quality (:triangulation-quality metrics 0.0)
           :stability-score (:stability-score metrics 0.0)
           :overall-grade (:overall-grade quality :unknown)
           :recording-count (get-in category-intelligence [:summary :all :recording-count] 0)
           :last-updated (get category-intelligence :updated-at)})
        {:available false
         :message "Category intelligence not yet built"}))
    (catch Exception e
      {:available false
       :error (.getMessage e)})))

(defn continue-with-matching
  "Continue matching process with extracted features.
   
   Called in 'match-brain-activity' below."
  [current-features is-smoothed?]
  (let [profile-name (or (:name ((:get-active-profile @state/state))) "default")
        up-dir (fio/get-wave-lexicon-dir profile-name "pong/up")
        up-signatures (load-wave-signatures-from-dir up-dir)
        down-dir (fio/get-wave-lexicon-dir profile-name "pong/down")
        down-signatures (load-wave-signatures-from-dir down-dir)]

    (if (and (seq up-signatures) (seq down-signatures))
      (let [current-bands (:band-powers current-features)

            up-scores (map #(calculate-band-power-similarity
                             current-bands
                             (:band-powers %))
                           up-signatures)
            down-scores (map #(calculate-band-power-similarity
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

(defn apply-enhancement
  "Apply category intelligence enhancements to raw confidence scores.
   
   Called in 'match-brain-activity' below."
  [up-confidence down-confidence category-intelligence]
  (try
    (let [; Get intelligence metrics with safe fallbacks
          intelligence-metrics (get-in category-intelligence [:summary :all :intelligence-metrics])
          separation-score (or (:separation-score intelligence-metrics) 0.5)
          triangulation-quality (or (:triangulation-quality intelligence-metrics) 0.5)
          stability-score (or (:stability-score intelligence-metrics) 0.0)

          ; Calculate dynamic threshold
          base-threshold (if (> separation-score 0.6) 0.02 0.05)
          quality-adjustment (* (- 1.0 separation-score) 0.03)
          stability-adjustment (* (max 0.0 (- 1.0 stability-score)) 0.02)
          dynamic-threshold (max 0.005 (+ base-threshold quality-adjustment stability-adjustment))

          ; Calculate confidence gap
          confidence-gap (Math/abs (- up-confidence down-confidence))

          ; Apply separation multiplier
          separation-multiplier (if (> confidence-gap dynamic-threshold)
                                  (+ 1.0 (* separation-score 0.5))
                                  1.0)

          ; Calculate enhanced scores
          enhanced-up (* up-confidence separation-multiplier)
          enhanced-down (* down-confidence separation-multiplier)

          ; Add triangulation bonus
          triangulation-bonus (* triangulation-quality 0.3)
          final-up (+ enhanced-up triangulation-bonus)
          final-down (+ enhanced-down triangulation-bonus)

          ; Determine intelligence grade
          intelligence-grade (cond
                               (> separation-score 0.8) :excellent
                               (> separation-score 0.6) :good
                               (> separation-score 0.4) :moderate
                               :else :poor)]

      ;(println "Intelligence enhancement:")
      ;(println "  Separation score:" (format "%.3f" separation-score))
      ;(println "  Triangulation quality:" (format "%.3f" triangulation-quality))
      ;(println "  Dynamic threshold:" (format "%.3f" dynamic-threshold))
      ;(println "  Confidence gap:" (format "%.3f" confidence-gap))
      ;(println "  Separation multiplier:" (format "%.2f" separation-multiplier))
      ;(println "  Enhanced scores - Up:" (format "%.4f" final-up) "Down:" (format "%.4f" final-down))

      {:up final-up
       :down final-down
       :confidence-gap confidence-gap
       :dynamic-threshold dynamic-threshold
       :intelligence-grade intelligence-grade
       :separation-score separation-score
       :triangulation-quality triangulation-quality})
    (catch Exception e
      (println "Error in intelligence enhancement:" (.getMessage e))
      {:up up-confidence :down down-confidence :error (.getMessage e)})))

(defn match-brain-activity
  "Fixed brain activity matching with better error handling"
  []
  (try
    (let [recording? @state/recording?
          current-time (System/currentTimeMillis)
          eeg-data @state/eeg-data
          data-count (count eeg-data)]

      ;(println "=== BRAIN ACTIVITY MATCHING ===")
      (cond
        (not recording?)
        {:timestamp current-time
         :confidence 0.0
         :up 0.0
         :down 0.0
         :error "EEG recording not started"}

        (< data-count 8) ; Reduced from 20 to 8
        {:timestamp current-time
         :confidence 0.0
         :up 0.0
         :down 0.0
         :error (str "Insufficient data for matching (have " data-count ", need 8)")}

        :else
        (let [sampling-rate (api/get-current-sample-rate)
              profile-name (or (:name ((:get-active-profile @state/state))) "default")

              ; Try smoothed features first, with better error handling
              smoothed-features (try
                                  (get-smoothed-features 3.0 3 sampling-rate) ; Reduced requirements
                                  (catch Exception e
                                    (println "Smoothed features error:" (.getMessage e))
                                    nil))

              ; Fallback to recent data if smoothed fails
              fresh-data (or smoothed-features
                             (try
                               (let [recent-data (get-recent-data-with-even-samples 2)] ; Reduced window
                                 (when (>= (count recent-data) 8) ; Reduced minimum
                                   (extract-features-from-eeg-data recent-data sampling-rate)))
                               (catch Exception e
                                 (println "Recent data extraction error:" (.getMessage e))
                                 nil)))]

          (if (nil? fresh-data)
            {:timestamp current-time
             :confidence 0.0
             :up 0.0
             :down 0.0
             :error "No fresh EEG data available or feature extraction failed"}

            ; Continue with matching logic
            (let [current-features (if (and (map? fresh-data) (:band-powers fresh-data))
                                     fresh-data
                                     (try
                                       (extract-features-from-eeg-data fresh-data sampling-rate)
                                       (catch Exception e
                                         (println "Feature extraction error:" (.getMessage e))
                                         nil)))]

              (if (nil? current-features)
                {:timestamp current-time
                 :confidence 0.0
                 :up 0.0
                 :down 0.0
                 :error "Feature extraction failed"}

                ; Continue with category intelligence
                (let [category "pong"
                      category-intelligence (try
                                              (lexi/load-category category)
                                              (catch Exception e
                                                (println "Category loading error:" (.getMessage e))
                                                nil))]

                  (if (nil? category-intelligence)
                    {:timestamp current-time
                     :confidence 0.1
                     :up 0.05
                     :down 0.05
                     :error "Category intelligence not available - using defaults"}

                    ; Continue with your existing matching logic
                    (let [matching-result (try
                                            (continue-with-matching current-features (= fresh-data smoothed-features))
                                            (catch Exception e
                                              (println "Matching error:" (.getMessage e))
                                              {:up 0.0 :down 0.0 :error (.getMessage e)}))]

                      (try
                        (let [enhanced-result (apply-enhancement
                                               (:up matching-result)
                                               (:down matching-result)
                                               category-intelligence)]
                          (merge {:timestamp current-time}
                                 enhanced-result
                                 (when (or (nil? (:up matching-result))
                                           (nil? (:down matching-result)))
                                   (println "⚠️ WARNING: Nil confidence values detected in match-brain-activity!")
                                   (println "Result:" matching-result))
                                 (when (:error matching-result)
                                   {:matching-error (:error matching-result)})))
                        (catch Exception e
                          (println "Enhancement error:" (.getMessage e))
                          (merge {:timestamp current-time}
                                 matching-result))))))))))))
    (catch Exception e
      (println "ERROR in brain activity matching:" (.getMessage e))
      (.printStackTrace e)
      {:timestamp (System/currentTimeMillis)
       :confidence 0.0
       :up 0.0
       :down 0.0
       :error (.getMessage e)})))