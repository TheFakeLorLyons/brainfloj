(ns floj.wave-refraction
  "Logic for generating the golden-tensors. Golden-tensors aren't tensors at all so the name
   is somewhat misleading, but I do intend on incorporating tensors in the future, and the name
   is both evocative and fun. You can change the calibration script to be as long as you want for
   each section and to prompt whatever activity you think is best for calibration. This serves more
   as an example aimed towards the example pong application included in this alpha stage.
   
   There is a bug here where it will say 'sleep interruped' while calibrating, but it seems to work
   anyways so for now I am leaving it as it is and I will investigate that further as time goes on.
   I could talk about why it is not very important at length but I do not have the space here."
  (:require [floj.api :as api]
            [floj.calibration :as calibrate]
            [floj.frequency-analysis :as fft]
            [floj.io :as fio]
            [floj.profiles :as profiles]
            [floj.state :as state]
            [floj.brainflow.board-shim :as brainflow]
            [floj.brainflow.data-filter :as filter]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.core.matrix :as matrix]
            [clojure.string :as str]))

(defn initialize-calibration-directory!
  "Create a single shared directory for all calibration steps"
  []
  (let [timestamp (System/currentTimeMillis)
        calibration-prefix "refraction-calibration"
        calibration-dir (str "resources/recordings/" calibration-prefix "_" timestamp)]
    (io/make-parents (str calibration-dir "/placeholder"))
    (swap! state/calibration-state assoc :current-directory calibration-dir)
    (println "Created shared calibration directory:" calibration-dir)
    calibration-dir))

(def calibration-state (atom {:baseline-tensor nil
                              :refraction-mirrors {}}))

(def calibration-script
  [{:label :rest    :duration-ms 30000 :instruction "Please relax... recording resting state."}
   {:label :active  :duration-ms 3000 :instruction "Please move your eyes up and down (no head movement)."}
   {:label :imagery :duration-ms 5000 :instruction "Now, imagine the same movement (up/down) without doing it."}])

(defn read-lor-file
  "Read a .lor file and return the data along with header info"
  [file-path]
  (with-open [in (java.io.DataInputStream.
                  (java.io.FileInputStream. file-path))]
    (let [header-size (.readInt in)
          header-bytes (byte-array header-size)
          _ (.read in header-bytes 0 header-size)
          header-str (String. header-bytes "UTF-8")
          header (edn/read-string header-str)
          total-bytes (.available in)
          data-points (/ total-bytes 8)
          data (vec (repeatedly data-points #(.readDouble in)))]
      {:header header
       :data data})))

(defn extract-numbers
  "Deeply extract all numbers from any data structure, returning a flat sequence"
  [data]
  (cond
    (number? data) [data]
    (nil? data) []
    (instance? clojure.lang.MapEntry data)
    (concat (extract-numbers (key data))
            (extract-numbers (val data)))
    (map? data)
    (mapcat extract-numbers (vals data))
    (or (vector? data) (seq? data) (list? data))
    (mapcat extract-numbers data)
    :else []))

(defn compute-covariance-matrix
  "Compute the covariance matrix for multi-channel EEG data"
  [data]
  (let [channel-count (count (first data))
        channels (apply map vector data)
        channel-means (mapv #(/ (apply + %) (count %)) channels)
        centered-channels (mapv (fn [channel mean]
                                  (mapv #(- % mean) channel))
                                channels
                                channel-means)]

    (vec (for [i (range channel-count)]
           (vec (for [j (range channel-count)]
                  (let [ch-i (nth centered-channels i)
                        ch-j (nth centered-channels j)
                        n (count ch-i)]
                    (/ (apply + (map * ch-i ch-j)) (dec n)))))))))

(defn compute-eigendecomposition
  "Compute eigenvalues and eigenvectors of a symmetric matrix
   Uses the Jacobi eigenvalue algorithm for small matrices"
  [matrix]
  (let [n (count matrix)
        eigenvectors (vec (for [i (range n)]
                            (vec (for [j (range n)]
                                   (if (= i j) 1.0 0.0)))))
        m (vec (map vec matrix))
        max-iterations 100
        tolerance 1e-10]
    (loop [iter 0
           m m
           eigenvectors eigenvectors]
      (if (>= iter max-iterations)
        {:eigenvalues (vec (for [i (range n)] (get-in m [i i])))
         :eigenvectors eigenvectors}

        (let [off-diag-indices (for [i (range n) j (range i)] [i j])
              [p q] (apply max-key #(Math/abs (get-in m [(second %) (first %)])) off-diag-indices)
              a-pq (get-in m [p q])]

          (if (< (Math/abs a-pq) tolerance)
            {:eigenvalues (vec (for [i (range n)] (get-in m [i i])))
             :eigenvectors eigenvectors}

            (let [a-pp (get-in m [p p])
                  a-qq (get-in m [q q])
                  theta (/ (* 2 a-pq) (- a-pp a-qq))
                  t (/ 1 (+ (Math/abs theta) (Math/sqrt (+ 1 (* theta theta)))))
                  t (if (neg? theta) (- t) t)
                  c (/ 1 (Math/sqrt (+ 1 (* t t))))
                  s (* t c)

                  update-fn (fn [curr-m]
                              (let [m-prime (vec (map vec curr-m))
                                    tau-p (- (* s (get-in curr-m [p q])) (* c (get-in curr-m [p p])))
                                    tau-q (+ (* c (get-in curr-m [q q])) (* s (get-in curr-m [p q])))]

                                (-> m-prime
                                    (assoc-in [p p] (- (get-in curr-m [p p]) (* tau-p s)))
                                    (assoc-in [q q] (+ (get-in curr-m [q q]) (* tau-q s)))
                                    (assoc-in [p q] 0.0)
                                    (assoc-in [q p] 0.0))))

                  update-eigenvectors (fn [curr-evec]
                                        (vec (for [i (range n)]
                                               (vec (for [j (range n)]
                                                      (if (or (= j p) (= j q))
                                                        (cond
                                                          (= j p) (- (* c (get-in curr-evec [i p]))
                                                                     (* s (get-in curr-evec [i q])))
                                                          (= j q) (+ (* s (get-in curr-evec [i p]))
                                                                     (* c (get-in curr-evec [i q])))
                                                          :else (get-in curr-evec [i j]))
                                                        (get-in curr-evec [i j])))))))]
              (recur (inc iter)
                     (update-fn m)
                     (update-eigenvectors eigenvectors)))))))))

(defn normalize-vector
  "Normalize a single vector to unit length, handling zero vectors gracefully"
  [v]
  (let [magnitude (Math/sqrt (apply + (map #(* % %) v)))]
    (if (> magnitude 1e-10)
      (mapv #(/ % magnitude) v)  ; Normalize the vector to unit length
      (vec (repeat (count v) 0.0)))))

(defn normalize-eigenvectors
  "Normalize eigenvectors to unit length"
  [eigenvectors]
  (mapv (fn [vec]
          (let [magnitude (Math/sqrt (apply + (map #(* % %) vec)))]
            (if (> magnitude 0)
              (mapv #(/ % magnitude) vec)
              vec)))
        eigenvectors))

(defn compute-gradient-direction
  "Calculates the gradient direction between channels, applying PCA direction correction"
  [channels eigenvectors]
  (vec (for [i (range (count channels))]
         (vec (for [j (range (count channels))]
                (if (= i j)
                  ; For diagonal elements, set a default direction with zero magnitude
                  {:direction [(normalize-vector (repeat (max 1 (dec (count (first channels)))) 0.0))]
                   :magnitude 0.0}
                  ; For non-diagonal elements, compute proper derivatives and directions
                  (try
                    (let [ch-i (nth channels i)
                          ch-j (nth channels j)
                          derivatives (mapv (fn [k]
                                              (if (and (< (inc k) (count ch-i))
                                                       (< (inc k) (count ch-j)))
                                                (- (* (nth ch-i (inc k)) (nth ch-j k))
                                                   (* (nth ch-i k) (nth ch-j (inc k))))
                                                0.0))
                                            (range (dec (min (count ch-i) (count ch-j)))))

                          ; Check if eigenvectors is not empty
                          principal-direction (if (and (seq eigenvectors)
                                                       (seq (first eigenvectors)))
                                                (first eigenvectors)
                                                (vec (repeat (count derivatives) (/ 1.0 (count derivatives)))))

                          ; Use dot product to find the projection strength
                          projection-strength (reduce + (map * derivatives principal-direction))

                          ; Normalize for unit direction - with better handling of edge cases
                          normalized-direction (normalize-vector derivatives)

                          ; Calculate magnitude with protection against NaN
                          magnitude (if (seq derivatives)
                                      (Math/sqrt (apply + (map #(* % %) derivatives)))
                                      0.0)]

                      {:direction [normalized-direction]
                       :magnitude (if (Double/isNaN magnitude) 0.0 magnitude)})
                    (catch Exception e
                      (println "Error in gradient calculation:" (.getMessage e))
                      {:direction [(vec (repeat (dec (count (first channels))) 0.0))]
                       :magnitude 0.0}))))))))

(defn principal-component-analysis
  "Perform PCA on the data"
  [data]
  (let [cov-matrix (compute-covariance-matrix data)
        decomp (compute-eigendecomposition cov-matrix)
        eigenvalues (:eigenvalues decomp)
        eigenvectors (:eigenvectors decomp)

        sorted-indices (vec (sort-by #(- (nth eigenvalues %)) (range (count eigenvalues))))
        sorted-eigenvalues (mapv #(nth eigenvalues %) sorted-indices)
        sorted-eigenvectors (mapv #(nth eigenvectors %) sorted-indices)]

    {:eigenvalues sorted-eigenvalues
     :eigenvectors (normalize-eigenvectors sorted-eigenvectors)}))

(defn compute-frequency-domain
  "Calculate frequency domain power in standard EEG bands"
  [magnitudes]
  (let [SRATE (api/get-current-sample-rate)
        resolution (/ SRATE (count magnitudes))
        hz->bin (fn [hz] (int (/ hz resolution)))
        delta-band (subvec magnitudes (hz->bin 0.5) (inc (hz->bin 4)))
        theta-band (subvec magnitudes (inc (hz->bin 4)) (inc (hz->bin 8)))
        alpha-band (subvec magnitudes (inc (hz->bin 8)) (inc (hz->bin 13)))
        beta-band  (subvec magnitudes (inc (hz->bin 13)) (inc (hz->bin 30)))
        gamma-band (if (< (inc (hz->bin 30)) (count magnitudes))
                     (subvec magnitudes (inc (hz->bin 30)))
                     [])]
    {:delta (apply + (map #(* % %) delta-band))
     :theta (apply + (map #(* % %) theta-band))
     :alpha (apply + (map #(* % %) alpha-band))
     :beta  (apply + (map #(* % %) beta-band))
     :gamma (apply + (map #(* % %) gamma-band))}))

(defn compute-wavelet-decomposition
  "Perform wavelet decomposition on signal
   Using a simplified Haar wavelet transform"
  [signal]
  (if (< (count signal) 2)
    {:approximation signal :detail []}
    (let [n (count signal)
          half-n (quot n 2)
          approximation (mapv (fn [i]
                                (let [a (nth signal (* 2 i))
                                      b (nth signal (inc (* 2 i)))]
                                  (/ (+ a b) (Math/sqrt 2))))
                              (range half-n))

          detail (mapv (fn [i]
                         (let [a (nth signal (* 2 i))
                               b (nth signal (inc (* 2 i)))]
                           (/ (- a b) (Math/sqrt 2))))
                       (range half-n))
          further-decomposition (compute-wavelet-decomposition approximation)]
      {:approximation (:approximation further-decomposition)
       :detail (conj (:detail further-decomposition) detail)})))

(defn extract-multiscale-features
  "Extract multiscale features from signal using wavelet decomposition"
  [signal level-count]
  (let [decomp (compute-wavelet-decomposition signal)
        details (:detail decomp)
        approximation (:approximation decomp)]
    (vec
     (for [level (range (min level-count (count details)))]
       (let [level-details (nth details level)
             detail-mean (if (seq level-details)
                           (/ (apply + level-details) (count level-details))
                           0.0)
             detail-variance (if (seq level-details)
                               (/ (apply + (map #(Math/pow (- % detail-mean) 2) level-details))
                                  (max 1 (count level-details)))
                               0.0)]
         {:level level
          :approximation-energy (if (seq approximation)
                                  (apply + (map #(* % %) approximation))
                                  0.0)
          :detail-energy (if (seq level-details)
                           (apply + (map #(* % %) level-details))
                           0.0)
          :detail-variance detail-variance})))))

(defn calculate-correlation
  "Calculate correlation between two signals"
  [signal-a signal-b]
  (if (and (seq signal-a) (seq signal-b))
    (let [min-len (min (count signal-a) (count signal-b))
          a (take min-len signal-a)
          b (take min-len signal-b)
          mean-a (/ (apply + a) min-len)
          mean-b (/ (apply + b) min-len)
          a-centered (map #(- % mean-a) a)
          b-centered (map #(- % mean-b) b)
          numerator (apply + (map * a-centered b-centered))
          denom-a (Math/sqrt (apply + (map #(* % %) a-centered)))
          denom-b (Math/sqrt (apply + (map #(* % %) b-centered)))]
      (if (and (not= denom-a 0) (not= denom-b 0))
        (/ numerator (* denom-a denom-b))
        0.0))
    0.0))

(defn create-golden-tensor
  "Create a multidimensional tensor representation of EEG baseline data
   Incorporates spectral, wavelet, and correlation properties"
  [segment-data]
  (let [first-sample-numbers (extract-numbers (first segment-data))
        desired-channel-count (min 4 (count first-sample-numbers))
        cleaned-data (mapv (fn [sample]
                             (let [numbers (vec (take desired-channel-count
                                                      (concat (extract-numbers sample)
                                                              (repeat 0.0))))]
                               (mapv #(if (and (number? %) (not (Double/isNaN %)) (not (Double/isInfinite %)))
                                        %
                                        0.0)
                                     numbers)))
                           segment-data)

        channel-count (count (first cleaned-data))
        channels (vec (apply map vector cleaned-data))
        channel-stats (mapv (fn [channel]
                              (let [valid-values (filterv #(and (number? %)
                                                                (not (Double/isNaN %))
                                                                (not (Double/isInfinite %)))
                                                          channel)
                                    mean (if (seq valid-values)
                                           (/ (apply + valid-values) (count valid-values))
                                           0.0)
                                    std (if (seq valid-values)
                                          (Math/sqrt (/ (apply + (map #(Math/pow (- % mean) 2) valid-values))
                                                        (max 1 (count valid-values))))
                                          1.0)]
                                {:mean mean :std (max 0.01 std)}))
                            channels)

        normalized-channels  (mapv (fn [channel stats]
                                     (mapv (fn [val]
                                             (let [z-score (/ (- val (:mean stats)) (:std stats))]
                                               (* (Math/signum z-score)
                                                  (Math/pow (Math/abs z-score) 0.7))))
                                           channel))
                                   channels
                                   channel-stats)

        normalized-data (vec (apply map vector normalized-channels))
        correlation-matrix (compute-covariance-matrix normalized-data)
        pca-result (try
                     (principal-component-analysis normalized-data)
                     (catch Exception e
                       (println "Error in PCA calculation:" (.getMessage e))
                       {:eigenvalues (vec (repeat channel-count 1.0))
                        :eigenvectors (vec (for [i (range channel-count)]
                                             (vec (for [j (range channel-count)]
                                                    (if (= i j) 1.0 0.0)))))}))

        project-sample (fn [sample eigenvectors] (mapv #(reduce + (map * sample %)) eigenvectors))
        projected-signal (mapv #(project-sample % (:eigenvectors pca-result)) normalized-data)
        projected-channels (vec (apply map vector projected-signal))

        SRATE (api/get-current-sample-rate)

        fft-results (mapv (fn [channel]
                            (try
                              (fft/perform-fft (vec channel) SRATE)
                              (catch Exception e
                                (println "FFT error on channel:" (.getMessage e))
                                {:frequencies [] :magnitudes []})))
                          projected-channels)

        magnitudes-list (mapv (fn [result]
                                (let [mags (vec (:magnitudes result))]
                                  (mapv #(if (and (number? %)
                                                  (not (Double/isNaN %))
                                                  (not (Double/isInfinite %)))
                                           %
                                           0.0)
                                        mags)))
                              fft-results)

        SRATE (api/get-current-sample-rate)

        frequency-domains (mapv (fn [channel-magnitudes]
                                  (when (seq channel-magnitudes)
                                    (let [scale-factor (/ 1.0 (max 1.0 (apply max 0.0001 channel-magnitudes)))
                                          scaled-magnitudes (mapv #(* % scale-factor) channel-magnitudes)
                                          resolution (/ SRATE (count scaled-magnitudes))
                                          hz->bin (fn [hz] (max 0 (min (dec (count scaled-magnitudes))
                                                                       (int (/ hz resolution)))))
                                          delta-range [(hz->bin 0.5) (hz->bin 4)]
                                          theta-range [(inc (hz->bin 4)) (hz->bin 8)]
                                          alpha-range [(inc (hz->bin 8)) (hz->bin 13)]
                                          beta-range [(inc (hz->bin 13)) (hz->bin 30)]
                                          gamma-start (inc (hz->bin 30))

                                          delta-band (if (< (first delta-range) (second delta-range))
                                                       (subvec scaled-magnitudes
                                                               (first delta-range)
                                                               (inc (second delta-range)))
                                                       [])
                                          theta-band (if (< (first theta-range) (second theta-range))
                                                       (subvec scaled-magnitudes
                                                               (first theta-range)
                                                               (inc (second theta-range)))
                                                       [])
                                          alpha-band (if (< (first alpha-range) (second alpha-range))
                                                       (subvec scaled-magnitudes
                                                               (first alpha-range)
                                                               (inc (second alpha-range)))
                                                       [])
                                          beta-band (if (< (first beta-range) (second beta-range))
                                                      (subvec scaled-magnitudes
                                                              (first beta-range)
                                                              (inc (second beta-range)))
                                                      [])
                                          gamma-band (if (< gamma-start (count scaled-magnitudes))
                                                       (subvec scaled-magnitudes gamma-start)
                                                       [])

                                          delta-power (if (seq delta-band)
                                                        (/ (apply + (map #(* % %) delta-band))
                                                           (max 1 (count delta-band)))
                                                        0.0)
                                          theta-power (if (seq theta-band)
                                                        (/ (apply + (map #(* % %) theta-band))
                                                           (max 1 (count theta-band)))
                                                        0.0)
                                          alpha-power (if (seq alpha-band)
                                                        (/ (apply + (map #(* % %) alpha-band))
                                                           (max 1 (count alpha-band)))
                                                        0.0)
                                          beta-power (if (seq beta-band)
                                                       (/ (apply + (map #(* % %) beta-band))
                                                          (max 1 (count beta-band)))
                                                       0.0)
                                          gamma-power (if (seq gamma-band)
                                                        (/ (apply + (map #(* % %) gamma-band))
                                                           (max 1 (count gamma-band)))
                                                        0.0)
                                          total-power (+ delta-power theta-power alpha-power beta-power gamma-power)
                                          norm-factor (max 0.001 total-power)]

                                      {:delta (/ delta-power norm-factor); Return normalized values as relative powers
                                       :theta (/ theta-power norm-factor)
                                       :alpha (/ alpha-power norm-factor)
                                       :beta  (/ beta-power norm-factor)
                                       :gamma (/ gamma-power norm-factor)})))
                                magnitudes-list)

        frequency-domain (if (seq (filter identity frequency-domains))
                           (let [valid-domains (filter identity frequency-domains)
                                 domain-count (count valid-domains)]
                             {:delta (/ (apply + (map :delta valid-domains)) domain-count)
                              :theta (/ (apply + (map :theta valid-domains)) domain-count)
                              :alpha (/ (apply + (map :alpha valid-domains)) domain-count)
                              :beta  (/ (apply + (map :beta valid-domains)) domain-count)
                              :thinking (/ 2
                                           (+ (/ (apply + (map :alpha valid-domains)) domain-count)
                                              (/ (apply + (map :beta valid-domains)) domain-count)))
                              :gamma (/ (apply + (map :gamma valid-domains)) domain-count)})
                           {:delta 0.2 :theta 0.2 :alpha 0.25 :beta 0.3 :thinking 0.1 :gamma 0.2}); simple average for thinking band

        wavelet-features (mapv (fn [channel-data]
                                 (try
                                   (if (< (count channel-data) 4); Make sure the data is long enough for wavelet decomposition
                                     (vec (for [level (range 4)]; Not enough data points for meaningful wavelets
                                            {:level level
                                             :approximation-energy 0.25
                                             :detail-energy 0.25
                                             :detail-variance 0.1}))
                                     (let [features (extract-multiscale-features channel-data 4); Proceed with wavelet decomposition
                                           total-energy (apply + (map :detail-energy features)); Calculate total energy for normalization
                                           norm-factor (max 0.001 total-energy)]
                                       (mapv #(update % :detail-energy (fn [e] (/ e norm-factor))) features))); Normalize energies by total energy
                                   (catch Exception e
                                     (println "Wavelet error:" (.getMessage e))
                                     (vec (for [level (range 4)]
                                            {:level level
                                             :approximation-energy 0.25
                                             :detail-energy 0.25
                                             :detail-variance 0.1})))))
                               normalized-channels)

        gradient-field (try
                         (let [ch-diffs (for [i (range channel-count)
                                              j (range channel-count)
                                              :when (not= i j)]
                                          (let [ch-i (nth normalized-channels i)
                                                ch-j (nth normalized-channels j)
                                                diffs (mapv #(- %1 %2) ch-i ch-j)]
                                            {:i i :j j :diffs diffs}))
                               default-dir (fn []
                                             (let [random-dir (mapv (fn [_] (- (rand) 0.5))
                                                                    (range (max 1 (dec (count (first normalized-channels))))))]
                                               (normalize-vector random-dir)))
                               gradient-matrix (vec
                                                (for [i (range channel-count)]
                                                  (vec
                                                   (for [j (range channel-count)]
                                                     (if (= i j)
                                                       {:direction [(default-dir)]; Diagonal elements get zero magnitude
                                                        :magnitude 0.0}
                                                       (let [pair-diffs (first (filter #(and (= (:i %) i) (= (:j %) j)) ch-diffs)); Non-diagonal elements get meaningful direction
                                                             diffs (if pair-diffs
                                                                     (:diffs pair-diffs)
                                                                     (mapv (fn [_] (- (rand) 0.5))
                                                                           (range (dec (count (first normalized-channels))))))
                                                             processed-diffs (if (< (count diffs) 2)
                                                                               (conj diffs (rand))
                                                                               diffs)
                                                             direction (normalize-vector processed-diffs)
                                                             magnitude (Math/sqrt (apply + (map #(* % %) processed-diffs)))]
                                                         {:direction [direction]
                                                          :magnitude (max 0.01 magnitude)}))))))]
                           gradient-matrix)
                         (catch Exception e
                           (println "Error in gradient calculation:" (.getMessage e))
                           (vec (for [i (range channel-count)]
                                  (vec (for [j (range channel-count)]
                                         (let [random-dir (normalize-vector
                                                           (mapv (fn [_] (- (rand) 0.5))
                                                                 (range (max 1 (dec (count (first normalized-channels)))))))]
                                           {:direction [random-dir]
                                            :magnitude (if (= i j) 0.0 0.1)})))))))

        manifold (try
                   (let [distance-matrix (vec (for [i (range channel-count)]
                                                (vec (for [j (range channel-count)]
                                                       (let [ch-i (nth normalized-channels i)
                                                             ch-j (nth normalized-channels j)
                                                             squared-diffs (map #(Math/pow (- %1 %2) 2) ch-i ch-j)]
                                                         (Math/sqrt (apply + squared-diffs)))))))]
                     {:distance-matrix distance-matrix
                      :embedding-coordinates (try
                                               (let [eigenvalues (:eigenvalues pca-result)
                                                     eigenvectors (:eigenvectors pca-result)
                                                     scaling-factor (fn [idx]
                                                                      (Math/sqrt (max 0.1
                                                                                      (if (< idx (count eigenvalues))
                                                                                        (nth eigenvalues idx)
                                                                                        0.1))))]
                                                 (mapv (fn [idx]
                                                         (let [eigvec (if (< idx (count eigenvectors))
                                                                        (nth eigenvectors idx)
                                                                        (vec (repeat channel-count
                                                                                     (/ 1.0 (Math/sqrt channel-count)))))
                                                               scale (scaling-factor idx)]
                                                           (mapv #(* % scale) eigvec)))
                                                       (range (min 3 (count eigenvectors)))))
                                               (catch Exception e
                                                 (println "Embedding error:" (.getMessage e))
                                                 (vec (for [i (range 3)]
                                                        (vec (for [j (range channel-count)]
                                                               (if (= (mod j 3) i)
                                                                 (/ 1.0 (Math/sqrt channel-count))
                                                                 0.1)))))))})
                   (catch Exception e
                     (println "Error generating manifold:" (.getMessage e))
                     {:distance-matrix (vec (repeat channel-count (vec (repeat channel-count 0.0))))
                      :embedding-coordinates (vec (for [i (range 3)]
                                                    (vec (for [j (range channel-count)]
                                                           (if (= (mod j 3) i) 0.5 0.1)))))}))]
    {:dimensions [channel-count channel-count]
     :data correlation-matrix
     :spectral {:eigenvalues (:eigenvalues pca-result)
                :eigenvectors (:eigenvectors pca-result)
                :frequency-domain frequency-domain}
     :temporal {:wavelet-features wavelet-features}
     :gradient {:field gradient-field
                :manifold manifold}}))

(defn update-golden-tensor!
  "Update the golden tensor for a profile based on calibration history"
  []
  (try
    (let [profile ((:get-active-profile @state/state))
          calibrations (calibrate/load-calibration-history)
          median-powers (calibrate/compute-median-band-powers calibrations)]

      (if (and profile median-powers)
        (let [current-golden (get-in profile [:golden-tensor :spectral :frequency-domain]
                                     {:delta 0.2 :theta 0.15 :alpha 0.25 :beta 0.3 :thinking 0.1 :gamma 0.1})

              updated-golden (into {}
                                   (for [band (keys filter/frequency-bands)]
                                     (let [current-val (get current-golden band 0.2)
                                           median-val (get median-powers band 0.2)
                                           blended (+ (* 0.3 current-val)
                                                      (* 0.7 median-val))]; Blend current with median (30% current, 70% new)
                                       [band blended])))
              updated-profile (assoc-in profile [:golden-tensor :spectral :frequency-domain] updated-golden)]
          (println "Updating golden tensor with new values:" updated-golden)
          ((:save-profile! @state/state) updated-profile)
          true)
        (do
          (println "Could not update golden tensor - insufficient data")
          false)))
    (catch Exception e
      (println "Error updating golden tensor:" (.getMessage e))
      false)))

(defn extract-signal-features
  "Extract key features from a signal for refraction calculation"
  [signal]
  (let [SRATE (api/get-current-sample-rate)
        segments (partition SRATE 128 signal)
        features (mapv (fn [segment]
                         (let [count-seg (count segment)]
                           (if (> count-seg 0)
                             {:mean (/ (apply + segment) count-seg)
                              :std-dev (Math/sqrt (/ (apply + (map #(Math/pow (- % (/ (apply + segment) count-seg)) 2) segment))
                                                     count-seg))
                              :max (apply max segment)
                              :min (apply min segment)
                              :range (- (apply max segment) (apply min segment))}
                             {:mean 0 :std-dev 0 :max 0 :min 0 :range 0})))
                       segments)]
    (reduce (fn [acc feat]
              (conj acc (:mean feat) (:std-dev feat) (:max feat) (:min feat) (:range feat)))
            []
            features)))

(defn calculate-refraction
  "Calculate the refraction (transfer function) between a signal and baseline"
  [signal baseline]
  (let [signal-features (extract-signal-features signal)
        baseline-features (if (vector? baseline)
                            baseline
                            (vec baseline))]
    (mapv (fn [s b]
            (if (and (number? b) (not= b 0))
              (/ s b)
              1.0))
          signal-features
          baseline-features)))

(defn create-refraction-mirror
  "Create a refraction mirror that transforms signal based on baseline"
  [recording-dir baseline-tensor]
  (let [metadata-file (io/file (str recording-dir "/metadata.edn"))
        metadata (when (.exists metadata-file)
                   (edn/read-string (slurp metadata-file)))
        channel-count (:channel-count metadata)

        channel-data (mapv (fn [channel-idx]
                             (let [lor-file (str recording-dir "/" channel-idx ".lor")
                                   channel-data (:data (read-lor-file lor-file))]
                               channel-data))
                           (range channel-count))

        refraction-mirrors (mapv (fn [channel-idx]
                                   (let [channel-data-vec (get channel-data channel-idx [])
                                         baseline-sig (matrix/get-row baseline-tensor channel-idx)
                                           ; Calculate the refraction - a transfer function between
                                           ; baseline and current reading
                                         refraction (calculate-refraction channel-data-vec baseline-sig)]
                                     {:channel channel-idx
                                      :refraction refraction}))
                                 (range channel-count))]
    refraction-mirrors))

(defn reconstruct-signal
  "Reconstruct a signal from original data and adjusted features"
  [original-data adjusted-features]
    ; Simple version that just scales the original signal
  (let [original-features (extract-signal-features original-data)
        scale-factors (mapv (fn [orig adj]
                              (if (and (number? orig) (not= orig 0))
                                (/ adj orig)
                                1.0))
                            original-features
                            adjusted-features)
        avg-scale (/ (apply + scale-factors) (count scale-factors))]
    (mapv #(* % avg-scale) original-data)))

(defn apply-refraction-to-channel
  "Apply refraction to a single channel"
  [channel-data refraction]
  (let [channel-features (extract-signal-features channel-data)
        adjusted-features (mapv (fn [feature refraction-factor]
                                  (* feature refraction-factor))
                                channel-features
                                (take (count channel-features) refraction))
        refracted-data (reconstruct-signal channel-data adjusted-features)]
    refracted-data))

(defn apply-refraction
  "Apply the refraction mirror to incoming data"
  [data refraction-mirrors]
  (reduce (fn [result mirror]
            (let [channel-idx (:channel mirror)
                  refraction (:refraction mirror)
                  channel-data (get data channel-idx [])
                  refracted-data (apply-refraction-to-channel channel-data refraction)]
              (assoc result channel-idx refracted-data)))
          {}
          refraction-mirrors))

(defn load-calibration-profile
  "Load wave refraction calibration data from a profile"
  [profile-name]
  (let [profile-path (str (fio/config-base-dir) "/profiles/" profile-name ".edn")
        exists? (.exists (io/file profile-path))]
    (if exists?
      (let [calibration-data (edn/read-string (slurp profile-path))]
        (reset! calibration-state calibration-data)
        calibration-data)
      (do
        (println "No wave refraction calibration profile found for" profile-name)
        nil))))

(defn check-golden-tensor-calibration
  "Check if the profile has calibration data"
  [profile-name]
  (let [profile-path (profiles/get-latest-profile-path profile-name)]
    (when (and profile-path (.exists (io/file profile-path)))
      (let [profile (edn/read-string (slurp profile-path))]
        (when (:golden-tensor profile)
          (println "Found calibration data for profile:" profile-name)
          true)))))

(defn load-golden-tensor
  "Load the golden tensor for a profile and set it in state"
  [profile-name]
  (let [profile-path (profiles/get-latest-profile-path profile-name)]
    (when (and profile-path (.exists (io/file profile-path)))
      (let [profile (edn/read-string (slurp profile-path))]
        (when-let [golden-tensor (:golden-tensor profile)]
          (println "Loading calibration data for profile:" profile-name)
          (reset! state/golden-tensor golden-tensor)
          true)))))

(defn calibrate-eeg-data!
  "Apply wave refraction calibration to incoming EEG data"
  [data]
  (let [{:keys [baseline-tensor refraction-mirrors]} @calibration-state]
    (if (and baseline-tensor (seq refraction-mirrors))
      (let [latest-mirror (val (last (sort-by key refraction-mirrors)))]; Apply the most recent refraction mirror
        (apply-refraction data latest-mirror))
      data)))

(defn save-refraction-calibration-file!
  "Save all calibration data to a single calibration.edn file"
  [calibration-data]
  (let [calibration-dir (:current-directory @state/calibration-state)]
    (when-not calibration-dir
      (throw (ex-info "No calibration directory initialized" {})))
    (println "Saving all calibration data to shared directory:" calibration-dir)
    (let [file-path (str calibration-dir "/calibration.edn")
          data-with-timestamp {:data calibration-data
                               :timestamp (System/currentTimeMillis)}]
      (spit file-path (pr-str data-with-timestamp))
      (println "Successfully wrote all calibration data to a single file")
      calibration-dir)))

(defn extreme-value? [x]
  (or (nil? x)
      (not (number? x))
      (> (Math/abs x) 100.0)))

(defn sane-number? [x]
  (and (number? x)
       (not (Double/isNaN x))
       (not (Double/isInfinite x))
       (<= -100.0 x)
       (>= 100.0 x)))

(defn valid-eeg-vector?
  "Returns true if vector is sane: correct length, not all zeros, no extreme values"
  [v]
  (and (vector? v)
       (= (count v) 4)
       (some #(not= % 0.0) v)
       (every? sane-number? v)
       (not (some extreme-value? v))))

(defn collect-eeg-data!
  "Collect data from the EEG device and add it to the state atom"
  [shim]
  (when-let [all-data (brainflow/get-board-data shim)]
    (when (seq all-data)
      (let [eeg-channels (brainflow/get-channel-data :eeg (brainflow/get-board-id shim)) ; dynamically fetch EEG channel indices
            eeg-data (map #(get all-data %) eeg-channels) ; extract each EEG channel's data vector
            transposed (apply map vector eeg-data)
            cleaned-data (filter valid-eeg-vector? transposed)]
        (swap! state/eeg-data into cleaned-data)))))

(defn save-tensor-to-profile!
  "Save the calibration data to the user's profile"
  [golden-tensor]
  (try
    (let [profile-name (:name (profiles/get-active-profile))
          _ (fio/ensure-profile-directories! profile-name)
          profile-path (profiles/get-latest-profile-path profile-name)

          ; If no profile exists yet, create a new timestamped one
          profile-path (if (and profile-path (.exists (io/file profile-path)))
                         profile-path
                         (let [timestamp (System/currentTimeMillis)]
                           (profiles/get-profile-history-path profile-name timestamp)))
          
          ; Load existing profile or create minimal new one
          current-profile (if (and profile-path (.exists (io/file profile-path)))
                            (edn/read-string (slurp profile-path))
                            {:name profile-name})
          
          ; Update profile with golden tensor
          updated-profile (assoc current-profile
                                 :golden-tensor golden-tensor
                                 :updated-at (java.util.Date.))]
      
      ; Create parent directories if they don't exist
      (when-let [parent (.getParentFile (io/file profile-path))]
        (when-not (.exists parent)
          (.mkdirs parent)))
      (spit profile-path (pr-str updated-profile))
      (println "Calibration data successfully saved to profile:" profile-name)
      true)
    (catch Exception e
      (println "Error saving calibration data:" (.getMessage e))
      (.printStackTrace e)
      false)))

(defn add-refraction-mirror! ;not yet implemented and will probably go in a different place
  "Add a new refraction mirror from a calibration recording"
  [profile-name recording-dir]
  (let [current-state (or (load-calibration-profile profile-name)
                          {:baseline-tensor nil
                           :refraction-mirrors {}})
        baseline-tensor (:baseline-tensor current-state)]
    (if baseline-tensor
      (let [mirror (create-refraction-mirror recording-dir baseline-tensor)
            recording-id (last (str/split recording-dir #"/"))]
        (swap! calibration-state update :refraction-mirrors assoc recording-id mirror)
        (save-tensor-to-profile! mirror)
        (println "Added refraction mirror from" recording-id)
        mirror)
      (println "Error: No baseline tensor exists for profile" profile-name))))

(defn run-calibration-step!
  "Run a single step of the calibration process using direct BrainFlow calls"
  [{:keys [label duration-ms instruction]}]
  (println "\n" instruction)
  (println (str "Recording for " (int (/ duration-ms 1000)) " seconds..."))

  (reset! state/eeg-data []) ; Clear previous data

  (println "Starting recording...")
  (brainflow/start-stream! @state/shim)
  (reset! state/recording? true)

  (let [start-time (System/currentTimeMillis)
        data-collection-fut (future   ; Data collection future that runs concurrently with progress tracking
                              (try
                                (while (and @state/recording?
                                            (< (- (System/currentTimeMillis) start-time) duration-ms))
                                  (collect-eeg-data! @state/shim) ; Actively collect data
                                  (Thread/sleep 100))  ; Small sleep to avoid overloading
                                (catch Exception e
                                  (println "Error collecting data:" (.getMessage e)))))
        progress-fut (future
                       (try
                         (let [total-steps 20]
                           (loop [step 0]
                             (when (< step total-steps)
                               (let [current-time (System/currentTimeMillis)
                                     elapsed (- current-time start-time)
                                     progress (min 100 (int (* 100 (/ elapsed duration-ms))))
                                     completed-bars (int (* total-steps (/ progress 100)))
                                     remaining-bars (- total-steps completed-bars)]
                                 (print (str "\rProgress: ["
                                             (apply str (concat (repeat completed-bars "=")
                                                                [(if (< progress 100) ">" " ")]))
                                             (apply str (repeat remaining-bars " "))
                                             "] " progress "%"))
                                 (flush))
                               (Thread/sleep (/ duration-ms total-steps))
                               (when (and @state/recording?
                                          (< (- (System/currentTimeMillis) start-time) duration-ms))
                                 (recur (inc step))))))
                         (catch InterruptedException e)
                         (catch Exception e
                           (println "Error in progress tracking:" (.getMessage e)))))]
    (Thread/sleep duration-ms)

    (println "\nStopping recording...")
    (reset! state/recording? false)
    (brainflow/stop-stream! @state/shim)

    (future-cancel progress-fut)
    (future-cancel data-collection-fut)
    (Thread/sleep 100))

  (let [collected-data @state/eeg-data]
    (if (seq collected-data)
      (do
        (println (str "Collected " (count collected-data) " data points"))
        (vec collected-data))
      (do
        (println "WARNING: No data collected during recording!")
        []))))

(defn generate-golden-tensor
  "Generate the golden tensor from calibration data including rest, active, and imagery."
  [calibration-data]
  (println "Generating baseline tensor from calibration data...")
  (try
    (let [rest-data    (get calibration-data :rest)
          active-data  (get calibration-data :active)
          imagery-data (get calibration-data :imagery)
          all-samples  (vec (concat rest-data active-data imagery-data))]
      (if (and (seq rest-data) (seq active-data) (seq imagery-data))
        (create-golden-tensor all-samples)
        (throw (ex-info "Missing one or more required calibration states (rest, active, imagery)" {}))))
    (catch Exception e
      (println "Error generating golden tensor:" (.getMessage e))
      (.printStackTrace e)
      nil)))

(defn run-calibration!
  "Run the full calibration process using direct BrainFlow calls"
  []
  (println "Starting calibration process...")
  (println "Please follow the instructions carefully for accurate calibration.")
  (println "The process will take approximately"
           (int (/ (apply + (map :duration-ms calibration-script)) 1000))
           "seconds.\n")

  (let [calibration-segments (atom {})]
    (swap! state/calibration-state assoc
           :is-calibrating true
           :stages-completed 0)
    (let [_ (initialize-calibration-directory!)
          was-recording? @state/recording?]
      (when was-recording?
        (println "Stopping existing recording...")
        (reset! state/recording? false)
        (brainflow/stop-stream! @state/shim)
        (Thread/sleep 500))
      (doseq [[index step] (map-indexed vector calibration-script)]
        (swap! state/calibration-state assoc
               :current-stage (:label step)
               :stages-completed index)
        (let [segment-data (run-calibration-step! step)]
          (swap! calibration-segments assoc (:label step) segment-data)
          (println (str "\nCompleted step " (inc index) "/" (count calibration-script)
                        ": " (:label step) " - "
                        (if (seq segment-data)
                          (str "Collected " (count segment-data) " data points")
                          "No data collected")))))
      (save-refraction-calibration-file! @calibration-segments)

      (when was-recording?
        (println "Restarting previous recording...")
        (reset! state/recording? true)
        (brainflow/start-stream! @state/shim)))

    (swap! state/calibration-state assoc
           :is-calibrating false
           :stages-completed (count calibration-script))

    (println "\nCalibration complete!")
    @calibration-segments))

(defn calibrate!
  "Main calibration function to be called from CLI"
  []
  (if-not @state/recording?
    (if-let [active-profile (profiles/get-active-profile)]
      (do
        (println "Starting calibration for profile:" (:name active-profile))
        (try
          (let [calibration-data (run-calibration!)]
            (println "Checking calibration data...")
            (println "Found data segments for:" (keys calibration-data))
            (if (and (seq calibration-data)
                     (every? #(seq (get calibration-data %)) [:rest :active :imagery]))
              (let [golden-tensor (generate-golden-tensor calibration-data)]
                (if golden-tensor
                  (do
                    (println "Calibration successful! Generated baseline tensor.")
                    (save-tensor-to-profile! golden-tensor)
                    ; Reload the profile after saving golden tensor
                    (let [profile-name (:name active-profile)]
                      (println "Reloading profile to update in-memory state...")
                      (profiles/set-active-profile! profile-name)
                      (println "Profile reloaded with updated golden tensor"))
                    (println "Your BCI is now calibrated for optimal performance."))
                  (println "Failed to generate golden tensor from calibration data.")))
              (println "Insufficient calibration data collected. Please try again.")))
          (catch Exception e
            (println "Calibration failed:" (.getMessage e))
            (.printStackTrace e))))
      (println "No active profile. Please create or select a profile first."))
    (println "Cannot calibrate while recording. Please stop recording first.")))

(defn initialize-baseline! []
  (state/register-fn! :update-golden-tensor update-golden-tensor!)
  (state/register-fn! :check-calibration check-golden-tensor-calibration)
  (state/register-fn! :load-golden-tensor load-golden-tensor))