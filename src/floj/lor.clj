(ns floj.lor
  (:require [floj.brainflow.boardids :as id]
            [floj.io :as fio]
            [floj.state :as state]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [cognitect.transit :as transit]
            [clojure.string :as str]
            [floj.api :as brainflow])
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

(defn write-metadata!
  "Write a single comprehensive metadata file for the entire recording"
  [lorfile-dir metadata channel-metadata]
  (let [complete-metadata (assoc metadata :channels channel-metadata)
        path (str lorfile-dir "/recording_metadata.edn")]
    (with-open [w (io/writer path)]
      (pprint complete-metadata w))))

(defn get-metadata-summary
  "Get a summary of the metadata for a lor directory"
  [dir-path]
  (try
    (let [metadata-file (str dir-path "/recording_metadata.edn")
          metadata (edn/read-string (slurp metadata-file))
          start-time (:start-time metadata)
          board-type (:device-type metadata)
          channels (:channels metadata)
          sampling-rate (:sampling-rate metadata)
          num-channels (count channels)
          max-data-points (apply max (map :data-points channels))
          duration-float (/ max-data-points  (double sampling-rate))
          duration-ms (* duration-float 1000)
          mins (int (/ duration-float 60))
          secs (- duration-float (* mins 60))]
      {:start-time start-time
       :formatted-start (format-date start-time)
       :board-type board-type
       :sampling-rate sampling-rate
       :duration-str (if (>= duration-float 60)
                       (format "%d:%02d" mins (int secs))
                       (format "%.2f sec" duration-float))
       :duration-ms duration-ms
       :channel-count num-channels})
    (catch Exception e
      {:error (.getMessage e)})))

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
  [data tags base-name board-id]
  (let [get-sampling-rate (:get-sampling-rate @state/state)
        get-eeg-channels (:get-eeg-channels @state/state)]
    (if (and get-sampling-rate get-eeg-channels)
      (let [sampling-rate (get-sampling-rate board-id)
            eeg-channels (get-eeg-channels board-id)
            lorfile-dir (fio/create-recording-directory! base-name)
            metadata {:recording-id (str base-name "_" (System/currentTimeMillis))
                      :start-time (System/currentTimeMillis)
                      :board-id board-id
                      :sampling-rate sampling-rate
                      :device-type (id/board-types board-id)
                      :recorded-at (java.util.Date.)
                      :channel-count (count eeg-channels)
                      :version "1.0"}
            channel-metadata (atom [])]
        (write-tags! lorfile-dir tags)
        (try
          (doseq [idx (range (count eeg-channels))]
            (let [channel-idx (nth eeg-channels idx)
                  raw-channel-data (map #(get % (+ idx 1)) data);offset by 1 because channel 0 is sample
                  channel-data (map (fn [item]
                                      (cond
                                        (number? item) item
                                        (and (vector? item) (seq item) (number? (first item))) (first item)
                                        :else 0.0))
                                 raw-channel-data)
                  channel-meta (write-lor-channel-file!
                                 lorfile-dir
                                 channel-idx
                                 channel-data
                                 (str "Channel " channel-idx)
                                 sampling-rate)]
              (swap! channel-metadata conj channel-meta)))
          (write-metadata! lorfile-dir metadata @channel-metadata)
          (println "Successfully wrote lorfile to directory:" lorfile-dir)
          lorfile-dir
          (catch Exception e
            (println "Error writing channel data:" (.getMessage e))
            (.printStackTrace e)
            nil)))
      (do
        (println "Error: Required functions not found in state")
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
    (when (.isDirectory dir)
      (->> (.listFiles dir)
        (filter #(.isDirectory %))
        (filter #(.exists (io/file (str (.getPath %) "/recording_metadata.edn"))))
        (map (fn [file]
               (let [path (.getPath file)
                     name (.getName file)
                     metadata (get-metadata-summary path)
                     tags (get-tags-summary path)]
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
                  :num-channels (:channel-count metadata)})))
        (sort-by :start-time >)))))

(defn str-join [separator coll]
  (str/join separator coll))

(defn display-recordings
  "Display a formatted list of available lorfiles"
  []
  (let [dir-path (fio/get-recordings-dir)
        files (list-recordings)]
    (if (seq files)
      (do
        (println "\nAvailable Recordings:")
        (println "========================")
        (doseq [[idx file] (map-indexed vector files)]
          (let [index-str (format "%2d" (inc idx))
                name-str (:name file)
                date-str (:formatted-start file)
                duration-str (:duration-str file)
                board-str (or (:board-type file) "Unknown")
                channels-str (str (:num-channels file) " ch")
                rate-str (if-let [rate (:sampling-rate file)]
                           (str rate " Hz")
                           "Unknown")
                tags-str (if (> (:tag-count file) 0)
                           (str (:tag-count file) " tags")
                           "No tags")
                tag-summary (if (seq (:tags file))
                              (str-join ", " (map (fn [[tag count]]
                                                    (str tag "(" count ")"))
                                               (:tags file)))
                              "")]

            (println (format "[%s] %s" index-str name-str))
            (println (format "    Date: %s | Duration: %s | Board: %s"
                       date-str duration-str board-str))
            (println (format "    Channels: %s | Rate: %s | %s"
                       channels-str rate-str tags-str))
            (when (seq tag-summary)
              (println (format "    Tags: %s" tag-summary)))
            (println)))
        (println (format "Total: %d recording(s) found in %s\n" (count files) dir-path)))
      (println "\nNo recordings found in" dir-path))))

(defn select-recording
  "Interactive function to select a LOR file"
  []
   (let [files (list-recordings)]
     (if (seq files)
       (do
         (display-recordings)
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