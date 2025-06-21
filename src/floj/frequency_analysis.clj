(ns floj.frequency-analysis
  "Mostly used in the eeg visualizer, and somewhat deprecated in favor of using the included
   BrainFlow data-filter class. I recommend sticking with that namespace if you are going to use
   the math functions."
  (:require [clojure.string :as str])
  (:import [org.apache.commons.math3.transform DftNormalization FastFourierTransformer TransformType]
           [org.apache.commons.math3.complex Complex])
  (:import [com.github.psambit9791.jdsp.filter Butterworth]))

(def frequency-bands
  {:delta [0.5 4]
   :theta [4 8]
   :alpha [8 13]
   :beta [13 30]
   :gamma [30 100]
   :thinking [8 30]})

(defn butterworth-filter
  "Apply a Butterworth filter to a signal
   Parameters:
     - signal: Vector of signal values
     - cutoff-low: Low cutoff frequency (Hz)
     - cutoff-high: High cutoff frequency (Hz, use nil for lowpass)
     - srate: Sampling rate (Hz)
     - order: Filter order
     - filter-type: :low-pass, :high-pass, or :band-pass"
  [signal cutoff-low cutoff-high srate order filter-type]
  (let [signal-array (double-array signal)
        butterworth (Butterworth. srate)
        filtered-signal (case filter-type
                          :low-pass (.lowPassFilter butterworth signal-array order cutoff-low)
                          :high-pass (.highPassFilter butterworth signal-array order cutoff-low)
                          :band-pass (.bandPassFilter butterworth signal-array order cutoff-low cutoff-high)
                          :band-stop (.bandStopFilter butterworth signal-array order cutoff-low cutoff-high))]
    (vec filtered-signal)))

(defn notch-filter
  "Apply a notch filter to remove power line interference (50Hz)"
  [signal srate]
  (let [notch-freq 50
        bandwidth 7.5]
    (butterworth-filter signal (- notch-freq (/ bandwidth 2))
                        (+ notch-freq (/ bandwidth 2))
                        srate 4 :band-stop)))

(defn high-pass-butterworth-filter
  "Apply high-pass filter (>0.5 Hz) to remove drifts from EEG signal"
  [signal]
  (butterworth-filter signal 0.5 nil 200 4 :high-pass))

(defn band-pass-butterworth-filter
  [signal]
  (let [[low high] (get frequency-bands :thinking)]
    (butterworth-filter signal low high 200 4 :band-pass)))

(defn apply-initial-filtering!
  "Apply initial Butterworth filtering to the EEG data"
  [signal]
  (let [notched-signal (notch-filter signal 200)
        high-pass-data (high-pass-butterworth-filter notched-signal)
        filtered-data (band-pass-butterworth-filter high-pass-data)]
    filtered-data))

(defn perform-fft [time-series sampling-rate]
  (try
    (let [filtered-series (filterv number? time-series)
          n (count filtered-series)

          banded (apply-initial-filtering! time-series)

          _ (when (< n 2)
              (throw (Exception. "Not enough numeric data for FFT")))

          padded-n (int (Math/pow 2 (Math/ceil (/ (Math/log n) (Math/log 2)))))
          padded-series (concat banded (repeat (- padded-n n) 0.0))

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

(defn calculate-band-frequencies
  [fft-result]
  (try
    (let [{:keys [frequencies magnitudes]} fft-result]
      (reduce-kv (fn [acc band-name [low-freq high-freq]]
                   (let [band-indices (keep-indexed
                                       (fn [idx freq]
                                         (when (and (>= freq low-freq) (<= freq high-freq)) idx))
                                       frequencies)
                         band-frequencies (map #(try (nth frequencies %) (catch Exception _ 0.0)) band-indices)
                         band-powers (map #(try (nth magnitudes %) (catch Exception _ 0.0)) band-indices)

                         peak-freq-idx (when (seq band-powers)
                                         (apply max-key #(nth band-powers %) (range (count band-powers))))
                         peak-frequency (if peak-freq-idx
                                          (nth band-frequencies peak-freq-idx 0.0)
                                          (/ (+ low-freq high-freq) 2.0))

                         total-power (reduce + 0.0 band-powers)
                         weighted-freq-sum (reduce + 0.0
                                                   (map * band-frequencies band-powers))
                         avg-frequency (if (> total-power 0.0)
                                         (/ weighted-freq-sum total-power)
                                         (/ (+ low-freq high-freq) 2.0))

                         mean-power (if (seq band-powers)
                                      (/ (apply + band-powers) (count band-powers))
                                      0.0)]
                     (assoc acc band-name {:power mean-power
                                           :peak-frequency peak-frequency
                                           :avg-frequency avg-frequency})))
                 {}
                 frequency-bands))
    (catch Exception e
      (println "Band frequency calculation error:" (.getMessage e))
      (reduce-kv (fn [acc band-name _]
                   (assoc acc band-name {:power 0.0
                                         :peak-frequency (/ (+ (first (get frequency-bands band-name))
                                                               (second (get frequency-bands band-name)))
                                                            2.0)
                                         :avg-frequency (/ (+ (first (get frequency-bands band-name))
                                                              (second (get frequency-bands band-name)))
                                                           2.0)}))
                 {}
                 frequency-bands))))

(defn get-dominant-wave
  [band-frequencies]
  (try
    (let [power-values (into {} (map (fn [[band data]]
                                       [band (if (map? data)
                                               (get data :power 0.0)
                                               (or data 0.0))])
                                     band-frequencies))
          max-power (apply max (vals power-values))
          dominant-bands (filter (fn [[band power]] (>= power (* 0.9 max-power)))
                                 power-values)]
      (if (and (> max-power 0.1) (seq dominant-bands))
        (key (first (sort-by (fn [[band _]] (case band
                                              :delta 1
                                              :theta 2
                                              :alpha 3
                                              :beta 4
                                              :gamma 5
                                              0))
                             >
                             dominant-bands)))
        :unknown))
    (catch Exception e
      (println "Error determining dominant wave:" (.getMessage e))
      :unknown)))

(defn process-channel-data
  [channel-data sampling-rate]
  (try
    (if (< (count channel-data) 2)
      {:band-frequencies {:alpha {:power 0.0, :peak-frequency 10.0, :avg-frequency 10.0}
                          :beta {:power 0.0, :peak-frequency 20.0, :avg-frequency 20.0}
                          :delta {:power 0.0, :peak-frequency 2.0, :avg-frequency 2.0}
                          :theta {:power 0.0, :peak-frequency 6.0, :avg-frequency 6.0}
                          :gamma {:power 0.0, :peak-frequency 40.0, :avg-frequency 40.0}}
       :dominant-wave :unknown}
      (let [fft-result (perform-fft channel-data sampling-rate)
            band-frequencies (calculate-band-frequencies fft-result)]
        {:band-frequencies band-frequencies
         :dominant-wave (get-dominant-wave band-frequencies)}))
    (catch Exception e
      (println "Error processing channel data:" (.getMessage e))
      {:band-frequencies {:alpha {:power 0.0, :peak-frequency 10.0, :avg-frequency 10.0}
                          :beta {:power 0.0, :peak-frequency 20.0, :avg-frequency 20.0}
                          :delta {:power 0.0, :peak-frequency 2.0, :avg-frequency 2.0}
                          :theta {:power 0.0, :peak-frequency 6.0, :avg-frequency 6.0}
                          :gamma {:power 0.0, :peak-frequency 40.0, :avg-frequency 40.0}}
       :dominant-wave :unknown})))

(defn process-eeg-window
  [eeg-data-window sampling-rate]
  (try
    (println "Processing window with" (count eeg-data-window) "channels")
    (let [channel-results (map #(process-channel-data % sampling-rate) eeg-data-window)]
      {:channel-results channel-results
       :overall-dominant-wave (-> (reduce (fn [acc result]
                                            (merge-with (fn [v1 v2]
                                                          (+ (get-in v1 [:power] 0.0)
                                                             (get-in v2 [:power] 0.0)))
                                                        (or (:band-frequencies result) {})
                                                        acc))
                                          {:alpha {:power 0.0}
                                           :beta {:power 0.0}
                                           :delta {:power 0.0}
                                           :theta {:power 0.0}
                                           :gamma {:power 0.0}}
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
                                         (repeat {:band-frequencies {:alpha {:power 0.0, :peak-frequency 10.0, :avg-frequency 10.0}
                                                                     :beta {:power 0.0, :peak-frequency 20.0, :avg-frequency 20.0}}})))
          channel-data (map-indexed
                        (fn [idx channel-result]
                          (let [alpha-power (get-in channel-result [:band-frequencies :alpha :power] 0.0)
                                beta-power (get-in channel-result [:band-frequencies :beta :power] 0.0)
                                alpha-freq (get-in channel-result [:band-frequencies :alpha :avg-frequency] 10.0)
                                beta-freq (get-in channel-result [:band-frequencies :beta :avg-frequency] 20.0)
                                total-power (+ alpha-power beta-power)
                                avg-freq (if (> total-power 0.0)
                                           (/ (+ (* alpha-power alpha-freq)
                                                 (* beta-power beta-freq))
                                              total-power)
                                           0.0)]
                            {:channel (inc idx)
                             :alpha (double alpha-power)
                             :beta (double beta-power)
                             :alpha-freq (double alpha-freq)
                             :beta-freq (double beta-freq)
                             :rawValue (double avg-freq)}))
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
       :channels [{:channel 1 :alpha 0.0 :beta 0.0 :alpha-freq 10.0 :beta-freq 20.0 :rawValue 0.0}
                  {:channel 2 :alpha 0.0 :beta 0.0 :alpha-freq 10.0 :beta-freq 20.0 :rawValue 0.0}
                  {:channel 3 :alpha 0.0 :beta 0.0 :alpha-freq 10.0 :beta-freq 20.0 :rawValue 0.0}
                  {:channel 4 :alpha 0.0 :beta 0.0 :alpha-freq 10.0 :beta-freq 20.0 :rawValue 0.0}]
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