(ns floj.brainflow.data-filter
  (:require [clojure.java.io :as io]
            [floj.brainflow.wavelet-types :as wavelet-types])
  (:import [brainflow DataFilter FilterTypes WindowOperations]
           [com.sun.jna Native Library]
           [org.apache.commons.math3.complex Complex]
           [org.apache.commons.math3.transform FastFourierTransformer DftNormalization TransformType]))

(def frequency-bands
  {:delta    [1.0 4.0]   ; 1-4 Hz
   :theta    [4.0 8.0]   ; 4-8 Hz
   :alpha    [8.0 13.0]  ; 8-13 Hz
   :beta     [13.0 30.0] ; 13-30 Hz
   :thinking [8.0 30.0]  ; Thinking 8-30 Hz
   :gamma    [30.0 50.0] ; 30-50 Hz
 })

(def window-types
  {:no-window 0
   :hanning 1
   :hamming 2
   :blackman-harris 3})

; FFT Function that returns both real and imaginary parts
(defn perform-fft
  "Perform FFT using BrainFlow native DataFilter. Returns [real-parts imaginary-parts]."
  [data & {:keys [window-type] :or {window-type :hanning}}]
  (try
    (let [data-len (count data)
          ;; Ensure data is a flat sequence of doubles, not nested vectors
          flat-data (if (and (seq? (first data)) (not (number? (first data))))
                      (double-array (flatten data))
                      (double-array data))
          output-re (double-array data-len)
          output-im (double-array data-len)
          window (get window-types window-type (:hanning wavelet-types/wavelet-types))]
      (DataFilter/perform_fft flat-data window output-re output-im)
      [(vec output-re) (vec output-im)])
    (catch Exception e
      (println "Error in perform-fft:" (.getMessage e))
      [[] []])))

(defn perform-fft-on-channel
  "Process a single EEG channel for FFT analysis, ensuring proper data format"
  [channel-data]
  (when (seq channel-data)
    ;; Ensure we have a flat vector of doubles
    (let [flattened (if (and (sequential? channel-data)
                             (number? (first channel-data)))
                      channel-data  ;; Already 1D vector
                      (vec (flatten channel-data)))  ;; Flatten to 1D

          ;; Apply the FFT
          [real-parts imag-parts] (perform-fft flattened)]

      [real-parts imag-parts])))

(defn perform-ifft
  "Perform inverse FFT to convert frequency domain back to time domain"
  [fft-data data-len]
  (try
    (let [[real-parts imag-parts] fft-data
          ;; Check if we have valid data
          valid-data? (and (seq real-parts)
                           (seq imag-parts)
                           (= (count real-parts) (count imag-parts)))

          ;; Make sure we have enough data
          complex-array (when valid-data?
                          (into-array Complex
                                      (map (fn [r i]
                                             (Complex. (double r) (double i)))
                                           real-parts
                                           imag-parts)))]
      (if (and valid-data? complex-array)
        (vec (DataFilter/perform_ifft complex-array))
        (do
          (println "Warning: Invalid data for IFFT, returning zeros")
          (vec (repeat data-len 0.0)))))
    (catch Exception e
      (println "Error performing IFFT:" (.getMessage e))
      (vec (repeat data-len 0.0)))))

; Calculate power spectrum (amplitude squared)
(defn calculate-power-spectrum
  "Calculate power spectrum from FFT results"
  [[real-parts imag-parts]]
  (if (and (seq real-parts) (seq imag-parts))
    (mapv #(+ (* %1 %1) (* %2 %2)) real-parts imag-parts)
    []))

(defn get-psd-welch
  "Calculate PSD using Welch's method via BrainFlow.
   data: EEG data array
   sampling-rate: in Hz
   nfft: FFT size (must be power of 2)
   Returns a map with :amplitudes and :frequencies vectors"
  [data sampling-rate & {:keys [nfft overlap window-type]
                         :or {nfft nil
                              overlap nil
                              window-type :hanning}}]
  (try
    ;; Ensure data is a flat sequence of doubles
    (let [flat-data (cond
                      ;; Already a flat sequence of numbers
                      (and (sequential? data) (number? (first data)))
                      (double-array data)

                      ;; Nested vectors
                      (and (sequential? data) (sequential? (first data)))
                      (double-array (flatten data))

                      ;; Single number (unlikely)
                      (number? data)
                      (double-array [data])

                      :else
                      (do
                        (println "Warning: Unexpected data format in get-psd-welch:" (type data))
                        (double-array [])))

          ;; Calculate appropriate nfft size if not provided
          data-len (count flat-data)
          calculated-nfft (cond
                            ;; User provided nfft
                            nfft nfft

                            ;; Data too small, use power of 2 less than data length
                            (< data-len 64)
                            (int (Math/pow 2 (Math/floor (/ (Math/log data-len) (Math/log 2)))))

                            ;; Default to 256 for larger data
                            :else 256)

          ;; Make sure nfft is valid
          safe-nfft (if (>= calculated-nfft data-len)
                      (int (/ data-len 2)) ;; Safe fallback
                      calculated-nfft)

          ;; Calculate default overlap if not provided
          safe-overlap (or overlap (int (/ safe-nfft 2)))

          ;; Get window code
          window (if (keyword? window-type)
                   (get window-types window-type wavelet-types/wavelet-types)
                   window-type)
          window-code (if (instance? WindowOperations window)
                        (.get_code window)
                        (int window))]

      ;; Only proceed if we have enough data
      (if (< data-len 4)
        {:amplitudes [] :frequencies []}
        (let [psd-result (DataFilter/get_psd_welch flat-data safe-nfft safe-overlap sampling-rate window-code)]
          {:amplitudes (vec (.getLeft psd-result))
           :frequencies (vec (.getRight psd-result))})))
    (catch Exception e
      (println "Error in get-psd-welch:" (.getMessage e))
      {:amplitudes [] :frequencies []})))

(defn get-band-power
  "Calculate band power for a specific frequency range"
  [data sampling-rate start-freq stop-freq]
  (try
    (let [flat-data (cond
                      ;; Already a flat sequence of numbers
                      (and (sequential? data) (number? (first data)))
                      (double-array data)

                      ;; Nested vectors
                      (and (sequential? data) (sequential? (first data)))
                      (double-array (flatten data))

                      ;; Single number (unlikely)
                      (number? data)
                      (double-array [data])

                      :else
                      (do
                        (println "Warning: Unexpected data format in get-band-power:" (type data))
                        (double-array [])))

          data-len (count flat-data)]

      ;; Only proceed if we have enough data
      (if (< data-len 4)
        0.0
        (let [psd (get-psd-welch flat-data sampling-rate)
              freqs (:frequencies psd)
              amps (:amplitudes psd)]
          (if (or (empty? freqs) (empty? amps))
            0.0
            (let [band-power (reduce +
                                     (map second
                                          (filter (fn [[freq amp]]
                                                    (and (>= freq start-freq) (< freq stop-freq)))
                                                  (map vector freqs amps))))]
              band-power)))))
    (catch Exception e
      (println "Error in get-band-power:" (.getMessage e))
      0.0)))


(defn fallback-fft
  "Fallback FFT implementation using Apache Commons Math"
  [data]
  (try
    (let [data-len (count data)

          padded-len (int (Math/pow 2 (Math/ceil (/ (Math/log data-len) (Math/log 2)))))
          padded-data (concat data (repeat (- padded-len data-len) 0.0))

          complex-data (into-array Complex
                                   (map #(Complex. % 0.0) padded-data))

          windowed-data (map-indexed
                         (fn [idx ^Complex c]
                           (let [window-val (- 0.5 (* 0.5 (Math/cos (* 2.0 Math/PI idx (/ 1.0 (dec padded-len))))))]
                             (.multiply c window-val)))
                         complex-data)
          transformer (FastFourierTransformer. DftNormalization/STANDARD)
         
          fft-result (.transform transformer
                                 (into-array Complex windowed-data)
                                 TransformType/FORWARD)

          real-parts (mapv #(.getReal %) fft-result)
          imag-parts (mapv #(.getImaginary %) fft-result)]

      [real-parts imag-parts])
    (catch Exception e
      (println "Error in fallback-fft:" (.getMessage e))
      [[] []])))

(defn unified-fft
  "Unified FFT function - tries native implementation first, then falls back"
  [data & {:keys [window-type] :or {window-type :hanning}}]
  (try
    (perform-fft data :window-type window-type)
    (println "Warning: Using fallback FFT implementation")
    (fallback-fft data)
    (catch Exception e
      (println "error uniting FFT" (.getMessage e)))))

(def filter-type-map
  {:butterworth FilterTypes/BUTTERWORTH
   :chebyshev FilterTypes/CHEBYSHEV_TYPE_1})

(defn filter-data!
  "Applies an in-place EEG filter using BrainFlow’s static methods.
   Accepts double[] arrays. Mutates them!
   filter-type: one of :lowpass, :highpass, :bandpass, :bandstop
   filter-shape: one of :butterworth or :chebyshev"
  [data
   sampling-rate
   start-freq
   end-freq
   order
   filter-type
   & {:keys [filter-shape ripple]
      :or {filter-shape :butterworth ripple 0.0}}]
  (let [shape (get filter-type-map filter-shape)]
    (case filter-type
      :lowpass
      (DataFilter/perform_lowpass data sampling-rate end-freq order shape ripple)

      :highpass
      (DataFilter/perform_highpass data sampling-rate start-freq order shape ripple)

      :bandpass
      (DataFilter/perform_bandpass data sampling-rate start-freq end-freq order shape ripple)

      :bandstop
      (DataFilter/perform_bandstop data sampling-rate start-freq end-freq order shape ripple)

      (throw (IllegalArgumentException.
              (str "Unsupported filter type: " filter-type))))))

(defn get-psd
  "Calculate Power Spectral Density (PSD) from data"
  [data sampling-rate]
  ; Use the entire data array (start at 0, end at length)
  (DataFilter/get_psd data 0 (count data) sampling-rate WindowOperations/HANNING))

(defn get-band-power
  "Calculate band power for a specific frequency range using PSD"
  [data sampling-rate start-freq stop-freq]
  (let [psd (get-psd data sampling-rate)]
    (DataFilter/get_band_power psd start-freq stop-freq)))

; Calculate band powers for standard EEG bands
(defn calculate-band-powers
  "Calculate the power in each frequency band from PSD data.
   Returns a map with :delta, :theta, :alpha, :beta, and :gamma powers."
  [data sampling-rate]
  (try
    (let [psd (get-psd-welch data sampling-rate)
          freqs (:frequencies psd)
          ampls (:amplitudes psd)]
      (if (or (empty? freqs) (empty? ampls))
        (do
          (println "Empty PSD data")
          nil)
        (let [band-ranges frequency-bands
              ; Calculate total power for normalization
              total-power (reduce + ampls)
              ; Function to calculate power in a specific band
              band-power (fn [band-range]
                           (let [min-freq (first band-range)
                                 max-freq (second band-range)]
                             (reduce +
                                     (map second
                                          (filter (fn [[freq amp]]
                                                    (and (>= freq min-freq) (< freq max-freq)))
                                                  (map vector freqs ampls))))))]
          ; Calculate power for each band
          (into {} (map (fn [[band range]]
                          [band (/ (band-power range) total-power)])
                        band-ranges)))))
    (catch Exception e
      (println "Error calculating band powers:" (.getMessage e))
      nil)))
