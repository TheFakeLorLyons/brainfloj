(ns floj.calibration
  "This namespace handles on the fly calibration for recordings, and routinely updates the
   golden-tensor as users record more things."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [floj.io :as fio]
            [floj.state :as state]
            [floj.brainflow.data-filter :as filter]))

(def ^:dynamic *debug-calibration* false)
(def ^:const GOLDEN_TENSOR_BATCH_SIZE 10)
(def ^:const PROFILE_ROTATION_INTERVAL 10)
(def ^:const MAX_CALIBRATION_FILES 100)

(defn get-calibration-file-path
  "Get the path to a calibration file from a timestamp"
  [timestamp]
  (str (fio/get-recordings-dir) "/recording_" timestamp "/recording_metadata.edn"))

(defn load-calibration-by-timestamp
  "Load a specific calibration file by timestamp"
  [timestamp]
  (try
    (let [file-path (get-calibration-file-path timestamp)]
      (when (.exists (io/file file-path))
        (let [content (slurp file-path)
              data (edn/read-string content)]
          #_(println "Successfully loaded calibration for timestamp:" timestamp)
          data)))
    (catch Exception e
      (println "Error loading calibration file for timestamp" timestamp ":" (.getMessage e))
      (.printStackTrace e)
      nil)))

(defn load-recent-calibrations
  "Load the most recent N calibration timestamps from profile"
  [profile-name n]
  (try
    (let [profile ((:load-profile @state/state) profile-name)
          all-timestamps (get-in profile [:calibration-history :files] [])
          recent-timestamps (take-last (or n 10) all-timestamps)]
      (println "Loading" (count recent-timestamps) "recent calibrations")
      (filterv identity
               (mapv (fn [timestamp]
                       (try
                         (let [ts (if (number? timestamp) timestamp (Long/parseLong (str timestamp)))]
                           (load-calibration-by-timestamp ts))
                         (catch Exception e
                           (println "Error reading calibration file for timestamp:" timestamp ":" (.getMessage e))
                           nil)))
                     recent-timestamps)))
    (catch Exception e
      (println "Error loading recent calibrations:" (.getMessage e))
      (.printStackTrace e)
      [])))

(defn save-calibration-to-history!
  "Save calibration to profile history"
  [profile-name calibration-index]
  (try
    (when (and profile-name calibration-index)
      (when *debug-calibration*
        (println "\n===== DEBUGGING CALIBRATION SAVE ====="))
      (let [profile ((:load-profile @state/state) profile-name)
            timestamp (System/currentTimeMillis)
            ; Get the current recording directory from state
            recording-dir (get-in @state/recording-context [:lorfile-dir])
            ; Extract timestamp from recording directory path and validate it
            dir-timestamp (when recording-dir
                            (let [extracted (fio/extract-timestamp-from-recording-dir recording-dir)]
                              (when (and extracted (number? extracted) (> extracted 0))
                                extracted)))
            ; Use the extracted timestamp or current timestamp as fallback
            file-timestamp (or dir-timestamp timestamp)
            ; Get existing files vector or empty vector if it doesn't exist
            existing-files (get-in profile [:calibration-history :files] [])
            ; Check if this timestamp already exists in the files list to avoid duplicates
            timestamp-exists? (some #(= % file-timestamp) existing-files)
            ; Only update files if this is a new timestamp
            updated-files (if (and (not timestamp-exists?) (vector? existing-files))
                            (conj existing-files (long file-timestamp))
                            existing-files)
            ; Ensure it's properly sorted (oldest to newest)
            sorted-files (vec (sort updated-files))
            file-count (count sorted-files)
            updated-profile (-> profile
                                (assoc-in [:calibration-history :count] file-count)
                                (assoc-in [:calibration-history :last-update] timestamp)
                                (assoc-in [:calibration-history :files] sorted-files))]

        ; Show updated calibration for debugging/observation
        (when *debug-calibration*
          (println "Profile name:" profile-name)
          (println "Recording directory:" recording-dir)
          (println "Extracted timestamp:" dir-timestamp)
          (println "Using timestamp:" file-timestamp)
          (println "Timestamp already exists:" timestamp-exists?)
          (println "Previous files count:" (count existing-files))
          (println "Updated files count:" (count sorted-files))
          (println "Calibration count:" file-count)
          (println "Should rotate profile?" (zero? (mod file-count PROFILE_ROTATION_INTERVAL)))
          (println "Should consolidate?" (zero? (mod file-count MAX_CALIBRATION_FILES)))
          (println "========================================\n"))

        ; Save updated profile - will create new file based on rotation/consolidation logic
        ((:save-profile! @state/state) updated-profile)
        true))
    (catch Exception e
      (println "Error saving calibration to history:" (.getMessage e))
      (.printStackTrace e)
      false)))

(defn load-calibration-history
  "Load calibration history for a profile"
  []
  (try
    (let [profile ((:get-active-profile @state/state))
          calibration-timestamps (get-in profile [:calibration-history :files] [])
          recent-timestamps (take-last MAX_CALIBRATION_FILES calibration-timestamps)]
      (println "Found" (count recent-timestamps) "calibration timestamps to load")
      (filterv identity
               (mapv (fn [timestamp]
                       (try
                         (load-calibration-by-timestamp timestamp)
                         (catch Exception e
                           (println "Error loading calibration for timestamp" timestamp ":" (.getMessage e))
                           nil)))
                     recent-timestamps)))
    (catch Exception e
      (println "Error loading calibration history:" (.getMessage e))
      (.printStackTrace e)
      [])))

(defn compute-median-band-powers
  "Compute median band powers across multiple calibration files"
  [calibrations]
  (try
    (let [band-powers-list (map (fn [calib]
                                  (get-in calib [:calibration-index :band-powers]))
                                calibrations)]
      (if (empty? band-powers-list)
        nil
        (let [median-powers (into {}
                                  (for [band (keys filter/frequency-bands)
                                        :when (not= band :thinking)]
                                    (let [all-values (mapv #(get % band 0.0) band-powers-list)
                                          sorted-values (sort all-values)
                                          mid-idx (quot (count sorted-values) 2)
                                          median (if (odd? (count sorted-values))
                                                   (nth sorted-values mid-idx)
                                                   (/ (+ (nth sorted-values mid-idx)
                                                         (nth sorted-values (dec mid-idx)))
                                                      2.0))]
                                      [band median])))
              alpha-median (get median-powers :alpha 0)
              beta-median (get median-powers :beta 0)
              thinking-median (/ (+ alpha-median beta-median) 2)]
          (assoc median-powers :thinking thinking-median))))
    (catch Exception e
      (println "Error computing median band powers:" (.getMessage e))
      (.printStackTrace e)
      nil)))

(defn calculate-aggregate-distribution
  "Calculate an aggregate band distribution from multiple calibration indices"
  [calibrations]
  (try
    (let [valid-calibrations (filter #(and % (:band-distribution %)) calibrations)]
      (if (seq valid-calibrations)
        (let [; Extract all band distributions
              distributions (map :band-distribution valid-calibrations)
              ; Calculate weighted sum (newer = more weight)
              distribution-count (count distributions)
              weighted-distributions
              (map-indexed
               (fn [idx dist]
                 ;; Weight increases with index (more recent = higher weight)
                 (let [weight (/ (inc idx) (apply + (range 1 (inc distribution-count))))]
                   (reduce-kv (fn [m k v]
                                (assoc m k (* v weight)))
                              {}
                              dist)))
               distributions)
              ; Combine all weighted distributions
              combined (reduce
                        (fn [acc dist]
                          (merge-with + acc dist))
                        {:delta 0.0 :theta 0.0 :alpha 0.0 :beta 0.0 :thinking 0.2 :gamma 0.0}
                        weighted-distributions)
              ; Normalize to sum to 1.0
              total (apply + (vals combined))
              normalized (reduce-kv
                          (fn [m k v]
                            (assoc m k (/ v total)))
                          {}
                          combined)]
          (println "Calculated aggregate distribution:" normalized)
          normalized)
        (do
          (println "No valid calibrations, using default distribution")
          {:delta 0.2 :theta 0.15 :alpha 0.25 :beta 0.3 :thinking 0.2 :gamma 0.1})))
    (catch Exception e
      (println "Error calculating aggregate distribution:" (.getMessage e))
      {:delta 0.2 :theta 0.15 :alpha 0.25 :beta 0.3 :thinking 0.2 :gamma 0.1})))

(defn should-update-profile?
  "Determine if we should update the profile's golden tensor"
  [recording-counter]
  (zero? (mod recording-counter GOLDEN_TENSOR_BATCH_SIZE)))

(defn extract-band
  "Extract specific frequency band from FFT data"
  [fft-data freq-range]
  (try
    (let [sampling-rate 200
          [real-parts imag-parts] fft-data
          freq-resolution (/ sampling-rate (* 2 (count real-parts)))
          band-indices (reduce
                        (fn [indices i]
                          (let [freq (* i freq-resolution)]
                            (if (and (>= freq (first freq-range))
                                     (<= freq (second freq-range)))
                              (conj indices i)
                              indices)))
                        []
                        (range (count real-parts)))
          ; Extract real and imaginary parts from an individual band
          band-real (mapv #(nth real-parts %) band-indices)
          band-imag (mapv #(nth imag-parts %) band-indices)]
      [band-real band-imag])
    (catch Exception e
      (println "Error extracting frequency band:" (.getMessage e))
      [[] []])))

(defn extract-band-powers
  "Extract band powers from EEG data for each frequency band"
  [eeg-data sampling-rate]
  (try
    (let [bands (keys filter/frequency-bands)
          channel-count (count eeg-data)]

      (when (zero? channel-count)
        (throw (Exception. "No EEG channels found in data")))

      (when (< sampling-rate 100)
        (throw (Exception. (str "Sampling rate too low: " sampling-rate))))

      (let [band-powers (for [channel-idx (range channel-count)]
                          (let [channel-data (nth eeg-data channel-idx)
                                flattened (mapv double (flatten (flatten channel-data)))]
                            ; Create a map of band -> power for this channel
                            (into {}
                                  (for [band bands]
                                    (let [[low high] (get filter/frequency-bands band)
                                          power (filter/get-band-power
                                                 (double-array flattened)
                                                 sampling-rate
                                                 low
                                                 high)]
                                      [band power])))))
            ; Normalize band powers across channels
            normalized-powers (if (> channel-count 1)
                                (apply merge-with +
                                       (for [channel-powers band-powers]
                                         (let [sum (reduce + (vals channel-powers))]
                                           (if (pos? sum)
                                             (into {} (for [[band power] channel-powers]
                                                        [band (/ power sum)]))
                                             channel-powers))))
                                (first band-powers))]
        (println "Extracted normalized band powers:" normalized-powers)
        normalized-powers))
    (catch Exception e
      (println "Error extracting band powers:" (.getMessage e))
      (.printStackTrace e)
      {:delta 0.2 :theta 0.15 :alpha 0.25 :beta 0.3 :thinking 0.2 :gamma 0.1})))

(defn create-calibration-index
  "Create a calibration index from band powers and profile"
  [band-powers profile-calibration]
  (try
    (if-not band-powers
      (throw (Exception. "No band powers provided for calibration"))
      (let [golden-tensor (get-in profile-calibration [:golden-tensor :spectral :frequency-domain]
                                  {:delta 0.2 :theta 0.15 :alpha 0.25 :beta 0.3 :thinking 0.2 :gamma 0.1})
            calibration-factors (into {}
                                      (for [band (keys filter/frequency-bands)]
                                        (let [current (get band-powers band 0.0001)
                                              target (get golden-tensor band 0.2)
                                              factor (max 0.1 (min 10.0 (/ target current)))]
                                          [band factor])))]
        {:band-powers band-powers
         :golden-tensor golden-tensor
         :calibration-factors calibration-factors
         :created-at (java.util.Date.)}))
    (catch Exception e
      (println "Error creating calibration index:" (.getMessage e))
      (.printStackTrace e)
      nil)))

(defn extract-dominant-frequency
  "Find the dominant frequency in a specific band range"
  [psd freq-range]
  (try
    (let [freqs (:frequencies psd)
          amps (:amplitudes psd)
          freq-amp-pairs (filter (fn [[freq amp]]
                                   (and (>= freq (first freq-range))
                                        (<= freq (second freq-range))))
                                 (map vector freqs amps))
          max-amp-pair (apply max-key second freq-amp-pairs)]
      (if max-amp-pair
        (first max-amp-pair)
        (/ (+ (first freq-range) (second freq-range)) 2.0)))

    (catch Exception e
      (println "Error finding dominant frequency:" (.getMessage e))
      (/ (+ (first freq-range) (second freq-range)) 2.0))))

(defn combine-bands
  "Combine separate frequency bands back into a single FFT representation"
  [bands]
  (try
    ; Determine the total size needed for the combined FFT
    (let [band-keys [:delta :theta :alpha :beta :gamma]
          ; Get the bands in the correct order
          ordered-bands (map #(get bands %) band-keys)

          ; Calculate the total size needed
          total-size (reduce + (map #(count (first %)) ordered-bands))

          ; Create vectors to hold the combined data
          combined-real (vec (repeat total-size 0.0))
          combined-imag (vec (repeat total-size 0.0))

          ; Insert band data at the correct position
          insert-band (fn [result offset band-data]
                        (let [[real-parts imag-parts] band-data
                              size (count real-parts)
                              [current-real current-imag] result
                              ; Create new vectors with band data inserted
                              new-real (reduce
                                        (fn [acc idx]
                                          (if (< idx size)
                                            (assoc acc (+ offset idx) (nth real-parts idx))
                                            acc))
                                        current-real
                                        (range size))

                              new-imag (reduce
                                        (fn [acc idx]
                                          (if (< idx size)
                                            (assoc acc (+ offset idx) (nth imag-parts idx))
                                            acc))
                                        current-imag
                                        (range size))]
                          [[new-real new-imag] (+ offset size)]))
          [result _] (reduce
                      (fn [[result offset] band-data]
                        (if (and band-data (seq (first band-data)))
                          (insert-band result offset band-data)
                          [result offset]))
                      [[combined-real combined-imag] 0]
                      ordered-bands)]
      result)
    (catch Exception e
      (println "Error combining frequency bands:" (.getMessage e))
      [[] []])))

(defn calculate-dynamic-target
  "Calculate a dynamic target distribution based on historical data and current profile"
  [profile-calibration recent-data]
  (try
    (let [current-target (or (get-in profile-calibration [:golden-tensor :spectral :frequency-domain])
                             {:delta 0.2 :theta 0.15 :alpha 0.25 :beta 0.3 :thinking 0.2 :gamma 0.1})

          ; Get recent band distributions from historical data
          historical-distributions (mapv :band-distribution recent-data)

          ; If historical data exists, blend it with the profile target
          dynamic-target (if (seq historical-distributions)
                           (let [; Average the historical distributions
                                 avg-historical (reduce (fn [acc dist]
                                                          (merge-with + acc dist))
                                                        {:delta 0.0 :theta 0.0 :alpha 0.0 :beta 0.0 :thinking 0.0 :gamma 0.0}
                                                        historical-distributions)

                                 ; Normalize to sum to 1.0
                                 total (apply + (vals avg-historical))
                                 norm-historical (if (pos? total)
                                                   (reduce-kv (fn [m k v]
                                                                (assoc m k (/ v total)))
                                                              {}
                                                              avg-historical)
                                                   current-target)

                                 ; Blend with current target (70% profile, 30% historical)
                                 profile-weight 0.7
                                 historical-weight 0.3

                                 blended (reduce-kv (fn [m k v]
                                                      (let [hist-val (get norm-historical k 0.0)
                                                            weighted-val (+ (* v profile-weight)
                                                                            (* hist-val historical-weight))]
                                                        (assoc m k weighted-val)))
                                                    {}
                                                    current-target)

                                 ; Normalize again to ensure sum is 1.0
                                 blend-total (apply + (vals blended))
                                 final-target (reduce-kv (fn [m k v]
                                                           (assoc m k (/ v blend-total)))
                                                         {}
                                                         blended)]

                             (println "Created dynamic target:" final-target)
                             final-target)
                           current-target)]

      dynamic-target)
    (catch Exception e
      (println "Error calculating dynamic target:" (.getMessage e))
      (or (get-in profile-calibration [:golden-tensor :spectral :frequency-domain])
          {:delta 0.2 :theta 0.15 :alpha 0.25 :beta 0.3 :thinking 0.2 :gamma 0.1}))))

(defn apply-calibration
  "Apply calibration to incoming EEG channel data using frequency-domain transformations"
  [eeg-data calibration-index]
  (if (and eeg-data calibration-index (seq eeg-data))
    (try
      (let [calibration-factors (:calibration-factors calibration-index)
            ; Only process if we have valid calibration factors
            valid-calibration? (and (map? calibration-factors)
                                    (some (fn [[_ v]] (not= v 1.0)) calibration-factors))]

        (if-not valid-calibration?
          eeg-data  ; Return unchanged if no valid calibration

          ; Process each channel separately
          (mapv
           (fn [channel-data]
             (when (seq channel-data)
               ; Step 1: Apply FFT to convert to frequency domain
               (let [fft-data (filter/perform-fft (vec channel-data))

                     bands {:delta (extract-band fft-data [1.0 4.0])
                            :theta (extract-band fft-data [4.0 8.0])
                            :alpha (extract-band fft-data [8.0 13.0])
                            :beta (extract-band fft-data [13.0 30.0])
                            :thinking (extract-band fft-data [8.0 30.0])
                            :gamma (extract-band fft-data [30.0 50.0])}

                     ; Step 3: Apply calibration factors to each band
                     calibrated-bands (reduce-kv
                                       (fn [m k v]
                                         (let [factor (get calibration-factors k 1.0)
                                               ; Limit factor to prevent extreme amplification
                                               limited-factor (min 3.0 (max 0.2 factor))]
                                           (assoc m k
                                                  (if (and v (seq (first v)))
                                                    (let [[real-parts imag-parts] v]
                                                      [(mapv #(* % limited-factor) real-parts)
                                                       (mapv #(* % limited-factor) imag-parts)])
                                                    v))))
                                       {}
                                       bands)

                     ; Step 4: Apply focus zone enhancement if specified
                     enhanced-bands (if-let [focus-zone (:focus-zone calibration-index)]
                                      (let [center-freq (get focus-zone :center)
                                            sensitivity (get-in focus-zone [:sensitivity-curve-params :sigma] 2.0)
                                            focus-band (cond
                                                         (<= 1.0 center-freq 4.0) :delta
                                                         (<= 4.0 center-freq 8.0) :theta
                                                         (<= 8.0 center-freq 13.0) :alpha
                                                         (<= 13.0 center-freq 30.0) :beta
                                                         :else :gamma)]
                                        (update calibrated-bands focus-band
                                                (fn [band-data]
                                                  (if (and band-data (seq (first band-data)))
                                                    (let [[real-parts imag-parts] band-data
                                                          [band-start band-end] (get filter/frequency-bands focus-band)
                                                          band-width (- band-end band-start)
                                                          bin-count (count real-parts)
                                                          freq-resolution (/ band-width bin-count)

                                                          enhance-fn (fn [freq amp]
                                                                       (let [dist (Math/abs (- freq center-freq))
                                                                             factor (Math/exp (- (/ (* dist dist)
                                                                                                    (* 2 sensitivity sensitivity))))
                                                                             boost (+ 1.0 (* 0.5 factor))]
                                                                         (* amp boost)))

                                                          enhanced-real (map-indexed
                                                                         (fn [i amp]
                                                                           (let [freq (+ band-start (* i freq-resolution))]
                                                                             (enhance-fn freq amp)))
                                                                         real-parts)
                                                          enhanced-imag (map-indexed
                                                                         (fn [i amp]
                                                                           (let [freq (+ band-start (* i freq-resolution))]
                                                                             (enhance-fn freq amp)))
                                                                         imag-parts)]
                                                      [(vec enhanced-real) (vec enhanced-imag)])
                                                    band-data))))
                                      calibrated-bands)

                     ; Step 5: Recombine bands
                     combined-fft (combine-bands enhanced-bands)

                     ; Step 6: Convert back to time domain using inverse FFT
                     calibrated-time-data (filter/perform-ifft combined-fft (count channel-data))]

                 ; Return the calibrated time-domain data
                 (vec calibrated-time-data))))
           eeg-data)))

      (catch Exception e
        (println "Error in calibration:" (.getMessage e))
        (.printStackTrace e)))
    eeg-data))

(defn assess-signal-quality
  "Assess EEG signal quality for calibration"
  [eeg-data]
  (try
    (let [channel-count (count eeg-data)
          quality-scores (for [channel-data eeg-data]
                           (let [; Calculate signal variance
                                 variance (let [mean (/ (reduce + channel-data) (count channel-data))
                                                squared-diffs (map #(Math/pow (- % mean) 2) channel-data)]
                                            (/ (reduce + squared-diffs) (count channel-data)))

                                 ; Calculate signal-to-noise ratio estimate
                                 ; Using power in alpha/beta bands vs high frequency noise
                                 fft-data (filter/perform-fft (vec channel-data))
                                 alpha-power (:alpha (extract-band-powers [channel-data] 200))
                                 beta-power (:beta (extract-band-powers [channel-data] 200))
                                 high-freq-power (:gamma (extract-band-powers [channel-data] 200))
                                 signal-power (+ alpha-power beta-power)
                                 snr (if (pos? high-freq-power)
                                       (/ signal-power high-freq-power)
                                       0.0)

                                 ; Check for flatlines or excessive amplitudes
                                 max-val (apply max channel-data)
                                 min-val (apply min channel-data)
                                 range-ok? (and (< (Math/abs max-val) 500)
                                                (> (- max-val min-val) 1.0))

                                 ; Calculate final quality score (0-1)
                                 quality-score (* 0.5
                                                  (+ (min 1.0 (/ variance 100.0))
                                                     (min 1.0 (* 0.2 snr))))]
                             (if range-ok? quality-score 0.0)))

          ; Overall quality is minimum across channels
          overall-quality (if (seq quality-scores)
                            (apply min quality-scores)
                            0.0)]
      {:quality overall-quality
       :is-valid (> overall-quality 0.3)})
    (catch Exception e
      (println "Error assessing signal quality:" (.getMessage e))
      {:quality 0.0 :is-valid false})))

(defn initialize-calibration! []
  (state/register-fn! :compute-median-band-powers compute-median-band-powers))