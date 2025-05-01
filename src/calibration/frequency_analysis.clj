(ns calibration.frequency-analysis
  (:require [clojure.string :as str])
  (:import [org.apache.commons.math3.transform DftNormalization FastFourierTransformer TransformType]
           [org.apache.commons.math3.complex Complex]))

(def frequency-bands
  {:delta [0.5 4]
   :theta [4 8]
   :alpha [8 13]
   :beta [13 30]
   :gamma [30 100]})

(defn perform-fft [time-series sampling-rate]
  (try
    (let [filtered-series (filterv number? time-series)
          n (count filtered-series)

          _ (when (< n 2)
              (throw (Exception. "Not enough numeric data for FFT")))

          padded-n (int (Math/pow 2 (Math/ceil (/ (Math/log n) (Math/log 2)))))
          padded-series (concat filtered-series (repeat (- padded-n n) 0.0))

          fft (FastFourierTransformer. DftNormalization/STANDARD)
          complex-array (into-array Complex (map #(Complex. (double %) 0.0) padded-series))
          result (.transform fft complex-array TransformType/FORWARD)

          magnitudes (map #(.abs %) result)
          frequencies (map #(* % (/ sampling-rate padded-n)) (range (/ padded-n 2)))]

      {:frequencies (take (/ padded-n 2) frequencies)
       :magnitudes (take (/ padded-n 2) magnitudes)})
    (catch Exception e
      (println "FFT error:" (.getMessage e))
      {:frequencies [] :magnitudes []})))

(defn calculate-band-powers [fft-result]
  (try
    (let [{:keys [frequencies magnitudes]} fft-result]
      (reduce-kv (fn [acc band-name [low-freq high-freq]]
                   (let [band-indices (keep-indexed
                                        (fn [idx freq]
                                          (when (and (>= freq low-freq) (<= freq high-freq)) idx))
                                        frequencies)
                         band-powers (map #(try (nth magnitudes %) (catch Exception _ 0.0)) band-indices)
                         mean-power (if (seq band-powers)
                                      (/ (apply + band-powers) (count band-powers))
                                      0.0)]
                     (assoc acc band-name mean-power)))
        {}
        frequency-bands))
    (catch Exception e
      (println "Band power calculation error:" (.getMessage e))
      {:delta 0.0, :theta 0.0, :alpha 0.0, :beta 0.0, :gamma 0.0})))

(defn get-dominant-wave [band-powers]
  (try
    (if (empty? band-powers)
      :unknown
      (key (apply max-key val band-powers)))
    (catch Exception e
      (println "Error determining dominant wave:" (.getMessage e))
      :unknown)))

(defn process-channel-data [channel-data sampling-rate]
  (try
    (if (< (count channel-data) 2)
      {:band-powers {:alpha 0.0, :beta 0.0, :delta 0.0, :theta 0.0, :gamma 0.0}
       :dominant-wave :unknown}
      (let [fft-result (perform-fft channel-data sampling-rate)
            band-powers (calculate-band-powers fft-result)]
        {:band-powers band-powers
         :dominant-wave (get-dominant-wave band-powers)}))
    (catch Exception e
      (println "Error processing channel data:" (.getMessage e))
      {:band-powers {:alpha 0.0, :beta 0.0, :delta 0.0, :theta 0.0, :gamma 0.0}
       :dominant-wave :unknown})))

(defn process-eeg-window [eeg-data-window sampling-rate]
  (try
    (println "Processing window with" (count eeg-data-window) "channels")
    (let [channel-results (map #(process-channel-data % sampling-rate) eeg-data-window)]
      {:channel-results channel-results
       :overall-dominant-wave (-> (reduce (fn [acc result]
                                            (merge-with + acc (:band-powers result)))
                                    {:delta 0.0, :theta 0.0, :alpha 0.0, :beta 0.0, :gamma 0.0}
                                    channel-results)
                                get-dominant-wave)})
    (catch Exception e
      (println "Error in process-eeg-window:" (.getMessage e))
      {:channel-results []
       :overall-dominant-wave :unknown})))

(defn get-ui-data [eeg-data-window sampling-rate]
  (try
    (let [result (process-eeg-window eeg-data-window sampling-rate)
          dominant-wave-name (-> result :overall-dominant-wave name str/upper-case)
          channel-results (:channel-results result)

          padded-results (take 4 (concat channel-results
                                   (repeat {:band-powers {:alpha 0.0, :beta 0.0}})))

          channel-data (map-indexed
                         (fn [idx channel-result]
                           {:channel (inc idx)
                            :alpha (double (get-in channel-result [:band-powers :alpha] 0.0))
                            :beta (double (get-in channel-result [:band-powers :beta] 0.0))
                            :rawValue (double (+ (get-in channel-result [:band-powers :alpha] 0.0)
                                                (get-in channel-result [:band-powers :beta] 0.0)))})
                         padded-results)

          alpha-sum (apply + (map :alpha channel-data))
          beta-sum (apply + (map :beta channel-data))
          ratio (if (and (> beta-sum 0.0) (> alpha-sum 0.0))
                  (/ alpha-sum beta-sum)
                  0.0)]

      {:dominant_wave dominant-wave-name
       :channels (vec channel-data)
       :alpha_beta_ratio ratio})
    (catch Exception e
      (println "Error in get-ui-data:" (.getMessage e))
      {:dominant_wave "UNKNOWN"
       :channels [{:channel 1 :alpha 0.0 :beta 0.0 :rawValue 0.0}
                  {:channel 2 :alpha 0.0 :beta 0.0 :rawValue 0.0}
                  {:channel 3 :alpha 0.0 :beta 0.0 :rawValue 0.0}
                  {:channel 4 :alpha 0.0 :beta 0.0 :rawValue 0.0}]
       :alpha_beta_ratio 0.0})))

(defn analyze-eeg-buffer [eeg-buffer window-size sampling-rate]
  (try
    (if (< (count eeg-buffer) 2)
      {:dominant_wave "WAITING"
       :channels [{:channel 1 :alpha 0.0 :beta 0.0 :rawValue 0.0}
                  {:channel 2 :alpha 0.0 :beta 0.0 :rawValue 0.0}
                  {:channel 3 :alpha 0.0 :beta 0.0 :rawValue 0.0}
                  {:channel 4 :alpha 0.0 :beta 0.0 :rawValue 0.0}]
       :alpha_beta_ratio 0.0}

      (let [window (if (> (count eeg-buffer) window-size)
                     (take-last window-size eeg-buffer)
                     eeg-buffer)]
        (get-ui-data window sampling-rate)))
    (catch Exception e
      (println "Error in analyze-eeg-buffer:" (.getMessage e))
      (.printStackTrace e)
      {:dominant_wave "ERROR"
       :channels [{:channel 1 :alpha 0.0 :beta 0.0 :rawValue 0.0}
                  {:channel 2 :alpha 0.0 :beta 0.0 :rawValue 0.0}
                  {:channel 3 :alpha 0.0 :beta 0.0 :rawValue 0.0}
                  {:channel 4 :alpha 0.0 :beta 0.0 :rawValue 0.0}]
       :alpha_beta_ratio 0.0})))

(defn test-fft []
  (println "Testing FFT functionality with synthetic data...")

  ;; Create synthetic alpha wave (10 Hz) in first channel
  ;; Create synthetic beta wave (20 Hz) in second channel
  ;; Create mixed signals in channels 3 and 4
  (let [sampling-rate 200
        window-size 256
        time-points (map #(/ % sampling-rate) (range window-size))

        ;; Generate synthetic waves
        alpha-wave (mapv #(Math/sin (* 2 Math/PI 10 %)) time-points)
        beta-wave (mapv #(Math/sin (* 2 Math/PI 20 %)) time-points)
        mixed-wave1 (mapv #(+ (* 0.7 (Math/sin (* 2 Math/PI 10 %)))
                             (* 0.3 (Math/sin (* 2 Math/PI 20 %))))
                      time-points)
        mixed-wave2 (mapv #(+ (* 0.3 (Math/sin (* 2 Math/PI 10 %)))
                             (* 0.7 (Math/sin (* 2 Math/PI 20 %))))
                      time-points)

        ;; Create test data structure (4 channels)
        test-data [alpha-wave beta-wave mixed-wave1 mixed-wave2]

        ;; Run analysis
        result (analyze-eeg-buffer test-data window-size sampling-rate)]

    (println "FFT Analysis Result:")
    (println "Dominant wave:" (:dominant_wave result))
    (println "Alpha/Beta ratio:" (:alpha_beta_ratio result))
    (println "Channel data:" (:channels result))

    ;; The first channel should have strong alpha, second should have strong beta
    result))