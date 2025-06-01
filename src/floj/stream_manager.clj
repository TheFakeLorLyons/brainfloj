(ns floj.stream-manager
  (:require [floj.api :as api]
            [floj.brainflow.board-shim :as brainflow]
            [floj.state :as state])
  (:import [brainflow BrainFlowPresets]
           [brainflow BoardShim]))

; Cache mechanism for channel info to avoid repeated JNI calls
(def channel-info-cache (atom {}))

(defn analyze-raw-data
  "Analyze the raw data structure to determine its format"
  [data]
  (println "Analyzing data structure:")
  (println "  Top level count:" (count data))
  (when (seq data)
    (let [first-elem (first data)]
      (println "  First element type:" (type first-elem))
      (cond
        (map? first-elem)
        (println "  First element keys:" (keys first-elem))
        (sequential? first-elem)
        (do
          (println "  First element count:" (count first-elem))
          (when (seq first-elem)
            (println "  First element first value:" (first first-elem))
            (println "  Inner element type:" (type (first first-elem))))))))
  
  ; Return a map describing the data format
  (cond
    ; Format: Maps with keys
    (and (seq data) (map? (first data)))
    {:format :map-format
     :sample-count (count data)
     :keys (keys (first data))}

    ; Format: ((timestamp, ch1, ch2, ...), (timestamp, ch1, ch2, ...), ...)
    (and (seq data)
         (sequential? (first data))
         (number? (first (first data)))
         (> (count (first data)) 1))
    {:format :timestamps-with-channels
     :sample-count (count data)
     :channel-count (dec (count (first data)))}

    ; Format: [[ch1-samples], [ch2-samples], ...]
    (and (seq data)
         (sequential? (first data))
         (every? sequential? data)
         (every? number? (first (first data))))
    {:format :channels-with-samples
     :channel-count (count data)
     :sample-count (count (first data))}

    ; Format: [[sample1-ch1 sample1-ch2 ...], [sample2-ch1 sample2-ch2 ...], ...]
    (and (seq data)
         (sequential? (first data))
         (every? sequential? data)
         (every? number? (first (first data))))
    {:format :samples-with-channels
     :sample-count (count data)
     :channel-count (count (first data))}

    :else
    {:format :unknown}))

(defn transpose-data
  "Transpose data between [samples][channels] and [channels][samples] formats.
   This function works bidirectionally - it will convert between formats."
  [data]
  (if (and (sequential? data) (sequential? (first data)))
    (apply mapv vector data)
    data))

(defn normalize-data-format
  "Convert data to a standard format: [[ch1-samples], [ch2-samples], ...]"
  [data]
  (let [format-info (analyze-raw-data data)
        _ (println "Detected format:" (:format format-info))]
    (case (:format format-info)
      :timestamps-with-channels
      ; Convert from ((timestamp, ch1, ch2, ...), ...) to [[ch1-samples], [ch2-samples], ...]
      (let [channel-count (:channel-count format-info)]
        (println "Converting data with" channel-count "channels")
        (vec (for [ch-idx (range channel-count)]
               (mapv #(nth % (inc ch-idx) 0.0) data))))

      :brainflow-format
      ; Handle the BrainFlow format where first array is timestamps, subsequent arrays are channel data
      ; Skip the first array (timestamps) and keep only the channel data
      (vec (rest data))

      :map-format
      ; Convert from maps to channels with samples
      (let [keys-to-use (filter #(not= % :timestamp) (keys (first data)))]
        (vec (for [k keys-to-use]
               (mapv #(get % k 0.0) data))))

      :samples-with-channels
      ; Convert from [[sample1-ch1 sample1-ch2 ...], [sample2-ch1 sample2-ch2 ...], ...] 
      ; to [[ch1-samples], [ch2-samples], ...]
      (transpose-data data)

      :channels-with-samples
      ; Already in the right format
      data

      ; Unknown format - log more details and return empty data
      (do
        (println "WARNING: Unknown data format detected!")
        (println "Data sample (first 2 elements):" (take 2 data))
        (println "Please check your data format and update analyze-raw-data function")
        []))))

(defn get-channel-indices
  "Get the indices for specific types of channels for a board.
   Returns a map with keys like :eeg, :accel, :gyro, etc."
  [board-id]
  (if-let [cached (get @channel-info-cache board-id)]
    cached
    (try
      (let [preset BrainFlowPresets/DEFAULT_PRESET
            channel-info (reduce (fn [acc channel-type]
                                   (let [getter (ns-resolve 'floj.brainflow.board-shim
                                                            (symbol (str "get-" (name channel-type) "-channels")))
                                         indices (getter board-id)
                                         scalar? (#{:timestamp :marker :battery :package-num} channel-type)
                                         result (if scalar? (first indices) indices)]
                                     (assoc acc channel-type result)))
                                 {}
                                 brainflow/channel-types)]
        ; Cache the result
        (swap! channel-info-cache assoc board-id channel-info)
        channel-info)
      (catch Exception e
        (println "Error getting channel indices:" (.getMessage e))
        nil))))

(defn extract-channel-data
  "Extract specific channel data from the full board data.
   Now correctly handles BrainFlow's data format where first array is timestamps.
   Returns a map with keys corresponding to the channel types.
   Data is returned in [samples][channels] format."
  [board-data board-id]
  (try
    ; First detect if we have BrainFlow's native format (first array is timestamps)
    (if (and (sequential? board-data)
             (> (count board-data) 1)
             (every? sequential? board-data))
      ; Handle BrainFlow format directly - skip the first array which is timestamps
      (let [channel-count (count (api/get-current-channels))
            eeg-data (vec (take (+ channel-count 1) board-data))] ; +1 to keep timestamps
        {:eeg (transpose-data eeg-data)         ; Convert from [channels][samples] to [samples][channels]
         :timestamp (vec (first board-data))})  ; Keep timestamps separately if needed

      ; Fall back to original channel extraction method
      (let [channel-indices (get-channel-indices board-id)
            data-by-sample (vec board-data)]
        {:eeg (mapv (fn [sample]
                      (mapv #(nth sample % 0.0) (:eeg channel-indices)))
                    data-by-sample)
         :accel (when (seq (:accel channel-indices))
                  (mapv (fn [sample]
                          (mapv #(nth sample % 0.0) (:accel channel-indices)))
                        data-by-sample))
         :analog (when (seq (:analog channel-indices))
                   (mapv (fn [sample]
                           (mapv #(nth sample % 0.0) (:analog channel-indices)))
                         data-by-sample))
         :other (when (seq (:other channel-indices))
                  (mapv (fn [sample]
                          (mapv #(nth sample % 0.0) (:other channel-indices)))
                        data-by-sample))
         :timestamp (when (:timestamp channel-indices)
                      (mapv #(nth % (:timestamp channel-indices) 0.0) data-by-sample))}))
    (catch Exception e
      (println "Error extracting channel data:" (.getMessage e))
      (.printStackTrace e)
      {:eeg []})))

(defn get-streamed-eeg-data
  "Get only the EEG data from board data, properly formatted for processing.
   Returns a map with :eeg key containing data in [samples][channels] format.
   Now correctly handles BrainFlow data format."
  [board-data board-id]
  (let [extracted (extract-channel-data board-data board-id)
        eeg-data (:eeg extracted)]
    {:eeg eeg-data}))

(defn analyze-input-data
  "Debug function to examine the structure of the raw data from BrainFlow"
  [data]
  (println "\nAnalyzing input data:")
  (println "Type:" (type data))
  (println "Count:" (count data))

  (when (seq data)
    (println "First element type:" (type (first data)))
    (println "First element:" (first data))

    (when (sequential? (first data))
      (println "First element size:" (count (first data)))
      (println "First element structure:" (mapv type (first data)))
      (println "Sample of first 3 elements:" (take 3 data))))

  data)

(defn get-board-data-by-type
  "Get board data separated by type.
   Returns a map with keys :eeg, :accel, etc., each containing properly formatted data."
  [board-shim]
  (let [board-id (brainflow/get-board-id board-shim)
        raw-data (brainflow/get-board-data board-shim)]
    (extract-channel-data raw-data board-id)))

(defn describe-board-channels
  "Return a human-readable description of the board's channels."
  [board-id]
  (let [channel-indices (get-channel-indices board-id)
        sampling-rate (brainflow/get-sampling-rate board-id)]

    {:board-name (BoardShim/get_device_name board-id)
     :eeg-channels {:count (count (:eeg channel-indices))
                    :indices (:eeg channel-indices)
                    :sampling-rate sampling-rate}
     :accelerometer-channels {:count (count (:accel channel-indices))
                              :indices (:accel channel-indices)}
     :analog-channels {:count (count (:analog channel-indices))
                       :indices (:analog channel-indices)}
     :other-channels {:count (count (:other channel-indices))
                      :indices (:other channel-indices)}
     :timestamp-channel (:timestamp channel-indices)}))

(defn get-eeg-time-series
  "Get EEG time series data with timestamps"
  []
  (let [board-id (brainflow/get-board-id @state/shim)
        raw-data (brainflow/get-board-data @state/shim)
        parsed-data (extract-channel-data raw-data board-id)

        ; Get timestamps and EEG data
        timestamps (:timestamp parsed-data)
        eeg-data (:eeg parsed-data)]

    ; Pair timestamps with EEG data
    (when (and (seq timestamps) (seq eeg-data))
      {:timestamps timestamps
       :eeg-data eeg-data})))