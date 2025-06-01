(ns floj.lor
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [cognitect.transit :as transit]
            [floj.brainflow.board-ids :as id]
            [floj.io :as fio]
            [floj.state :as state]
            [floj.brainflow.board-shim :as brainflow]
            [floj.profiles :as profiles])
  (:import [java.text SimpleDateFormat]
           [java.util Date]))

(defn write-transit [data]
  (let [out (java.io.ByteArrayOutputStream.)]
    (transit/write (transit/writer out :json) data)
    (.toString out)))

(defn read-transit [s]
  (let [in (java.io.ByteArrayInputStream. (.getBytes s))]
    (transit/read (transit/reader in :json))))

(defn format-date
  "Format a date in a human-readable format"
  [timestamp]
  (let [date (if (instance? Date timestamp)
               timestamp
               (Date. (long timestamp)))
        formatter (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")]
    (.format formatter date)))

(defn update-metadata-calibration!
  "Updates the metadata file with a new calibration index"
  [lorfile-dir metadata calibration-index]
  (try
    (println "Attempting to update metadata file in directory:" lorfile-dir)
    (println "Current metadata keys:" (keys metadata))
    (println "New calibration index:" calibration-index)
    (let [updated-metadata (assoc metadata :calibration-index calibration-index)
          path (str lorfile-dir "/recording_metadata.edn")]
      (with-open [w (io/writer path)]
        (binding [*print-length* nil
                  *print-level* nil]
          (pprint updated-metadata w)))
      (println "Successfully wrote recording metadata file")
      updated-metadata)
    (catch Exception e
      (println "Error updating metadata calibration:" (.getMessage e))
      (println "Stack trace:")
      (.printStackTrace e)
      nil)))

(defn write-metadata!
  "Creates recording directory and initial metadata file with calibration index"
  ([session-name board-id]
   (write-metadata! session-name board-id nil))
  ([base-name board-id custom-config]
   (try
     (let [sampling-rate (brainflow/get-sampling-rate board-id)
           eeg-channels (brainflow/get-channel-data :eeg board-id)

           _ (when-not (and sampling-rate eeg-channels)
               (throw (Exception. "Required state functions not available")))

           lorfile-dir (if custom-config
                         (let [path (:path custom-config)
                               recording-type (:recording-type custom-config)]
                          (fio/create-recording-directory! path recording-type))
                         (fio/create-recording-directory! base-name))

           metadata {:recording-id (str base-name "_" (System/currentTimeMillis))
                     :start-time (System/currentTimeMillis)
                     :board-id board-id
                     :sampling-rate sampling-rate
                     :device-type (id/board-types board-id)
                     :recorded-at (java.util.Date.)
                     :channel-count (count eeg-channels)
                     :version "1.0"}

           _ (swap! state/recording-context assoc :lorfile-dir lorfile-dir)

           current-profile-name (or (:name (profiles/get-active-profile)) "default")
           _ (println "Using profile:" current-profile-name)

           profile-calibration (try
                                 (when (and (resolve 'refract/load-calibration-profile)
                                            (fn? (resolve 'refract/check-calibration))
                                            ((resolve 'refract/check-calibration) current-profile-name))
                                   (println "Profile calibration found, loading...")
                                   ((resolve 'refract/load-calibration-profile) current-profile-name))
                                 (catch Exception e
                                   (println "Error loading calibration profile:" (.getMessage e))
                                   nil))

           default-local-calibration (when profile-calibration
                                       (let [band-distribution (or
                                                                (get-in profile-calibration
                                                                        [:golden-tensor :spectral :frequency-domain])
                                                                {:alpha 0.022 :beta 0.774 :gamma 0.201})]
                                         {:band-distribution band-distribution
                                          :target-distribution band-distribution
                                          :calibration-factors {:alpha 1.0 :beta 1.0 :gamma 1.0}
                                          :focus-zone {:center 17.5
                                                       :width 5.0
                                                       :bounds [10.0 25.0]
                                                       :sensitivity-curve-params {:center 17.5 :sigma 2.83}}
                                          :timestamp (System/currentTimeMillis)}))

           complete-metadata (cond-> metadata
                               default-local-calibration (assoc :calibration-index default-local-calibration)
                               profile-calibration (assoc :calibration-profile-name current-profile-name))

           metadata-path (str lorfile-dir "/recording_metadata.edn")]

       (let [dir-file (java.io.File. lorfile-dir)]
         (when-not (.exists dir-file)
           (println "Creating directory structure:" lorfile-dir)
           (.mkdirs dir-file)))

       (with-open [w (io/writer metadata-path)]
         (binding [*print-length* nil
                   *print-level* nil]
           (pprint complete-metadata w)))

       (println "Initial metadata with calibration written to" metadata-path)

       {:lorfile-dir lorfile-dir
        :metadata complete-metadata
        :eeg-channels eeg-channels
        :sampling-rate sampling-rate})
     (catch Exception e
       (println "Error creating recording metadata:" (.getMessage e))
       (.printStackTrace e)
       nil))))

(defn write-tags!
  "Write tags file to the lorfile directory"
  [lorfile-dir tags]
  (spit (str lorfile-dir "/tags.edn") (pr-str tags)))

(defn get-tags-summary
  "Get a summary of the tags in a lor directory"
  [dir-path]
  (try
    (let [tags-file (str dir-path "/tags.edn")
          tags-data (read-transit (slurp tags-file))
          tags (:tags tags-data)
          tag-count (count tags)]
      {:tag-count tag-count
       :tags (if (> tag-count 0)
               (->> tags
                 (map :label)
                 (frequencies)
                 (sort-by second >)
                 (take 3))
               [])})
    (catch Exception e
      {:tag-count 0 :tags []})))

(defn write-lor-channel-file!
  "Write channel data and metadata to a single .lor file"
  [lorfile-dir channel-idx data channel-name sampling-rate]
  (let [lor-file (str lorfile-dir "/" channel-idx ".lor")
        header-size 1024
        data-points (count data)
        actual-eeg-channel (+ channel-idx 1)
        channel-header {:channel-idx actual-eeg-channel
                        :name channel-name
                        :sampling-rate sampling-rate
                        :data-points data-points
                        :format "double"
                        :header-size header-size}
        header-str (pr-str channel-header)
        header-bytes (.getBytes header-str "UTF-8")
        _ (when (> (count header-bytes) header-size)
            (throw (Exception. (str "Header too large: " (count header-bytes)
                                 " bytes exceeds limit of " header-size " bytes"))))]
    (with-open [out (java.io.DataOutputStream.
                      (java.io.FileOutputStream. lor-file))]
      (.writeInt out (count header-bytes))
      (.write out header-bytes)
      (dotimes [_ (- header-size (count header-bytes) 4)]
        (.writeByte out 0))
      (doseq [value data]
        (.writeDouble out (double value))))
    {:channel-idx channel-idx
     :name channel-name
     :sampling-rate sampling-rate
     :data-points data-points
     :file (str channel-idx ".lor")}))

(defn write-lor!
  "Write a complete lorfile directory with a single metadata file and one .lor file per channel"
  [data tags board-id]
  (let [sampling-rate (brainflow/get-sampling-rate board-id)
        eeg-channels (brainflow/get-channel-data :eeg board-id)
        lorfile-dir (:lorfile-dir @state/recording-context)]
    (if (and sampling-rate eeg-channels lorfile-dir)
      (let [metadata {:recording-id lorfile-dir
                      :start-time (System/currentTimeMillis)
                      :board-id board-id
                      :sampling-rate sampling-rate
                      :device-type (id/board-types board-id)
                      :recorded-at (java.util.Date.)
                      :channel-count (count eeg-channels)
                      :version "1.0"}
            channel-metadata (atom [])
            ; Extract EEG data from the structured format
            eeg-samples (mapcat (fn [data-point]
                                  (when-let [eeg-data (:eeg data-point)]
                                    eeg-data))
                                data)]
        (write-tags! lorfile-dir tags)
        (try
          (doseq [idx (range (count eeg-channels))]
            (let [channel-idx (nth eeg-channels idx)
                  _ (println "Processing channel idx:" channel-idx "at position:" idx)
                  ; Extract data for this specific channel from all samples
                  channel-data (mapv (fn [sample]
                                       ; Each sample is a vector where first element could be timestamp
                                       ; and subsequent elements are channel values
                                       (if (and (vector? sample) (> (count sample) (inc idx)))
                                         (nth sample (inc idx))
                                         0.0))
                                     eeg-samples)]
              (println "Channel" channel-idx "data sample:" (take 5 channel-data))
              (let [channel-meta (write-lor-channel-file!
                                  lorfile-dir
                                  channel-idx
                                  channel-data
                                  (str "Channel " channel-idx)
                                  sampling-rate)]
                (swap! channel-metadata conj channel-meta))))
          (println "Successfully wrote lorfile to directory:" lorfile-dir)
          lorfile-dir
          (catch Exception e
            (println "Error writing channel data:" (.getMessage e))
            (.printStackTrace e)
            nil)))
      (do
        (println "Error: Required data not found - sampling-rate: " sampling-rate
                 ", eeg-channels: " (boolean eeg-channels)
                 ", lorfile-dir: " lorfile-dir)
        nil))))

(defn read-lor-channel-file
  "Read a single .lor channel file, parsing the header and binary data (doubles)"
  [file-path print-header?]
  (when print-header?
    (println "Reading file at path:" file-path))
  (with-open [in (java.io.DataInputStream. (java.io.FileInputStream. file-path))]
    (let [header-len (.readInt in)
          header-bytes (byte-array header-len)]
      (.readFully in header-bytes)
      (let [header-str (String. header-bytes "UTF-8")]
        (when print-header?
          (println "Header string: " header-str))
        (let [header (edn/read-string header-str)
              remaining-bytes (- (:header-size header) (+ 4 header-len))]
          (when print-header?
            (println "Remaining bytes to skip: " remaining-bytes))
          (dotimes [_ remaining-bytes]
            (.readByte in))
          (let [data (loop [values []]
                       (if (>= (count values) (:data-points header))
                         values
                         (let [value (.readDouble in)]
                           (recur (conj values value)))))]
            (assoc header :data data :header header)))))))

(defn get-metadata-summary
  "Get a summary of the metadata for a lor directory including calibration info and duration"
  [dir-path]
  (try
    (let [metadata-file (str dir-path "/recording_metadata.edn")
          metadata (edn/read-string (slurp metadata-file))
          start-time (:start-time metadata)
          board-type (:device-type metadata)
          num-channels (:channel-count metadata)
          sampling-rate (:sampling-rate metadata)

          duration-info (try
                          (let [lor-files (->> (io/file dir-path)
                                               (.listFiles)
                                               (filter #(.endsWith (.getName %) ".lor"))
                                               (sort #(compare (.getName %1) (.getName %2))))
                                first-lor-file (first lor-files)]

                            (when first-lor-file
                              (let [channel-data (read-lor-channel-file (.getPath first-lor-file) false)
                                    data-points (:data-points channel-data)
                                    duration-seconds (double (/ data-points sampling-rate))  ; Convert to double
                                    duration-ms (* duration-seconds 1000)]

                                {:duration-seconds duration-seconds
                                 :duration-ms duration-ms
                                 :duration-str (format "%.1fs" duration-seconds)})))
                          (catch Exception e
                            (println "Error calculating duration for" dir-path ":" (.getMessage e))
                            (.printStackTrace e)
                            {:duration-str "Unknown"}))

          calibration-info (when-let [cal-index (:calibration-index metadata)]
                             {:profile (:calibration-profile-name metadata "default")
                              :band-distribution (:band-distribution cal-index)
                              :timestamp (:timestamp cal-index)})]

      (cond-> {:start-time start-time
               :formatted-start (format-date start-time)
               :board-type board-type
               :sampling-rate sampling-rate
               :channel-count num-channels}
        duration-info (merge duration-info)
        calibration-info (assoc :calibration calibration-info)))
    (catch Exception e
      {:error (.getMessage e)})))

(defn print-tabular-lor-data
  "Print .lor data in a tabular format with all channels side by side"
  [channels]
  (when-let [first-channel (first channels)]
    (println "=== Recording Information ===")
    (println "Sampling Rate:" (:sampling-rate first-channel) "Hz")
    (println "Data Points:" (:data-points first-channel))
    (println "Format:" (:format first-channel))
    (println "Total Channels:" (count channels))
    (println)

    (print "Time (s)")
    (doseq [channel channels]
      (print (format " | %10s" (:name channel))))
    (println)

    (print "---------")
    (dotimes [_ (count channels)]
      (print "------------"))
    (println)
    (let [sampling-rate (:sampling-rate first-channel)
          time-interval (/ 1.0 sampling-rate)
          data-points (:data-points first-channel)
          channel-data-arrays (mapv :data channels)]

      (dotimes [i data-points]
        (when (and (> i 0) (zero? (mod i sampling-rate)))
          (println)
          (println (format "------------- %d second(s) -------------" (/ i sampling-rate)))
          (println))

        (print (format "%8.3f" (* i time-interval)))
        (doseq [data-array channel-data-arrays]
          (if (< i (count data-array))
            (print (format " | %10.4f" (get data-array i)))
            (print " | ----------")))
        (println)))))

(defn read-lor!
  "Read a complete lorfile directory"
  [recordings-dir]
  (let [metadata-file (str recordings-dir "/recording_metadata.edn")
        tags-file (str recordings-dir "/tags.edn")
        metadata (edn/read-string (slurp metadata-file))
        tags (get (edn/read-string (slurp tags-file)) :tags)
        channel-files (->> (io/file recordings-dir)
                        (.listFiles)
                        (filter #(.endsWith (.getName %) ".lor"))
                        (sort #(compare (.getName %1) (.getName %2)))
                        (mapv #(.getPath %)))
        channels (mapv #(read-lor-channel-file %1 (= %2 0))
                   channel-files
                   (range (count channel-files)))]
    (print-tabular-lor-data channels)
    {:metadata metadata
     :tags tags
     :channels channels}))

(defn list-recordings
  "List all lorfile directories with detailed information"
  []
  (let [dir (io/file (fio/get-recordings-dir))]
    (println "Checking recordings directory at:" (.getAbsolutePath dir))
    (if (.isDirectory dir)
      (let [contents (seq (.listFiles dir))]
        (println "Found" (count contents) "items in directory.")
        (->> contents
          (filter #(.isDirectory %))
          (filter (fn [f]
                    (let [meta-file (io/file f "recording_metadata.edn")]
                      (println "Checking for metadata in:" (.getPath meta-file))
                      (.exists meta-file))))
          (map (fn [file]
                 (let [path (.getPath file)
                       name (.getName file)
                       metadata (try (get-metadata-summary path) (catch Exception e
                                                                   (println "Failed to read metadata for:" name "->" (.getMessage e))
                                                                   nil))
                       tags (try (get-tags-summary path) (catch Exception e
                                                           (println "Failed to read tags for:" name "->" (.getMessage e))
                                                           nil))]
                   (if (and metadata tags)
                     {:name name
                      :path path
                      :start-time (:start-time metadata)
                      :formatted-start (:formatted-start metadata)
                      :board-type (:board-type metadata)
                      :sampling-rate (:sampling-rate metadata)
                      :duration-str (:duration-str metadata)
                      :duration-ms (:duration-ms metadata)
                      :tag-count (:tag-count tags)
                      :tags (:tags tags)
                      :num-channels (:channel-count metadata)}
                     (do
                       (println "Skipping directory with missing data:" name)
                       nil)))))
          (remove nil?)
          (sort-by :start-time >)))
      nil)))

(defn str-join [separator coll]
  (str/join separator coll))

(defn display-recordings
  "Display a formatted list of available lorfiles"
  [files]
  (let [files (or files [])
        dir-path (fio/get-recordings-dir)]
    (println "\nAvailable Recordings:")
    (println "========================")
    (doseq [[idx file] (map-indexed vector files)]
      (try
        (let [index-str (format "%2d" (inc idx))
              name-str (or (:name file) "Unknown")
              date-str (or (:formatted-start file) "Unknown")
              duration-str (or (:duration-str file) "Unknown")
              board-str (or (:board-type file) "Unknown")
              channels-str (str (or (:num-channels file) "?") " ch")
              rate-str (if-let [rate (:sampling-rate file)]
                         (str rate " Hz")
                         "Unknown")
              tag-count (or (:tag-count file) 0)
              tags-str (if (pos? tag-count)
                         (str tag-count " tags")
                         "No tags")
              tag-summary (if-let [tags (:tags file)]
                            (str-join ", " (map (fn [[tag count]]
                                                  (str tag "(" count ")"))
                                             tags))
                            "")]
          (println (format "[%s] %s" index-str name-str))
          (println (format "    Date: %s | Duration: %s | Board: %s"
                     date-str duration-str board-str))
          (println (format "    Channels: %s | Rate: %s | %s"
                     channels-str rate-str tags-str))
          (when (seq tag-summary)
            (println (format "    Tags: %s" tag-summary)))
          (println))
        (catch Exception e
          (println "Error displaying recording:" (.getMessage e)))))
    (println (format "Total: %d recording(s) found in %s\n" (count files) dir-path))))

(defn select-recording
  "Interactive function to select a LOR file"
  []
  (let [files (list-recordings)]
    (if (seq files)
      (do
        (display-recordings files)
        (print "Enter recording number to select (or 'q' to cancel): ")
        (flush)
        (let [input (read-line)]
          (if (= input "q")
            nil
            (try
              (let [idx (dec (Integer/parseInt input))]
                (when (< -1 idx (count files))
                  (:path (nth files idx))))
              (catch Exception _
                (println "Invalid selection.")
                nil)))))
      (do
        (println "No recordings found.")
        nil))))

(defn initialize-lor! []
  (state/register-fn! :write-lor! write-lor!))