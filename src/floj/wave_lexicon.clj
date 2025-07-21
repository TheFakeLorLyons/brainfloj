(ns floj.wave-lexicon
  "Logic for recording wave-signatures and bundling the collections of wave-signatures into
   categories that are organized as detailed in the readme. In a coming update I will release
   detailed mermaid diagrams detailing this structure more thoroughly."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [floj.brainflow.board-shim :as brainflow]
            [floj.calibration :as calibrate]
            [floj.api :as api]
            [floj.categorization :as category]
            [floj.io :as fio]
            [floj.lor :as lor]
            [floj.profiles :as profiles]
            [floj.record :as record]
            [floj.state :as state]
            [floj.stream-manager :as stream]
            [zprint.core :as zp]))

(def ^:private MIN_SAMPLES_FOR_SIGNATURE 20)
(def ^:private SIMILARITY_THRESHOLD 0.75)
(def DEFAULT_RECORDING_DURATION 5000)

(defn fill-initial-lexicon!
  "Initialize the wave lexicon directory structure and create default categories"
  [profile-name]
  (try
    (let [pong-dir (fio/get-wave-lexicon-dir profile-name "pong")
          pong-up-dir (str pong-dir "/up")
          pong-down-dir (str pong-dir "/down")]

      (fio/ensure-directory! pong-dir)
      (fio/ensure-directory! pong-up-dir)
      (fio/ensure-directory! pong-down-dir)

      (spit (str pong-dir "/README.md")
            "# Pong Wave Signatures\n\nThis directory contains your recorded brain patterns for controlling the Pong game.\n\n- The 'up' directory contains recordings when you're thinking about moving the paddle up\n- The 'down' directory contains recordings when you're thinking about moving the paddle down\n\nTo add new recordings, use the 'A' key in the CLI and select the appropriate category.")

      (spit (str pong-up-dir "/README.md")
            "# Up Movement Recordings\n\nThis directory will contain your recorded brain patterns for moving the paddle upward.\nTo add a new recording, use the 'A' key in the CLI.")

      (spit (str pong-down-dir "/README.md")
            "# Down Movement Recordings\n\nThis directory will contain your recorded brain patterns for moving the paddle downward.\nTo add a new recording, use the 'A' key in the CLI.")

      (let [config-path (str (fio/config-base-dir) "/config.edn")
            config (if (.exists (io/file config-path))
                     (edn/read-string (slurp config-path))
                     {})
            updated-config (update config :wave-lexicon-categories
                                   #(if (seq %)
                                      (if (some #{:pong-up :pong-down} %)
                                        %
                                        (conj % :pong-up :pong-down))
                                      [:pong-up :pong-down]))]
        (spit config-path (pr-str updated-config))))
    (println (str "Loaded " profile-name "'s wave-lexicon!"))
    (catch Exception e
      (println "Error initializing wave lexicon:" (.getMessage e))
      (.printStackTrace e))))

(defn list-wave-signature-categories
  "List all wave signature categories for a profile"
  []
  (try
    (let [profile-name (or (:name ((:get-active-profile @state/state))) "default")
          lexicon-dir (fio/get-wave-lexicon-dir profile-name "")]
      (if (.exists (io/file lexicon-dir))
        (let [dirs (filter #(.isDirectory %)
                           (or (seq (.listFiles (io/file lexicon-dir))) []))
              categories (map #(.getName %) dirs)]
          (println "Found" (count categories) "lexicon categories:")
          (doseq [category categories]
            (println "  -" category))
          categories)
        (do
          (println "No lexicon directory found for profile" profile-name)
          [])))
    (catch Exception e
      (println "Error listing lexicon categories:" (.getMessage e))
      (.printStackTrace e)
      [])))

(defn list-wave-signatures-in-category
  "List all wave signatures within a category"
  [profile-name category]
  (try
    (let [category-dir (str (fio/config-base-dir) "/profiles/" profile-name "/wave_lexicon/" category)]
      (when (.exists (io/file category-dir))
        (->> (.listFiles (io/file category-dir))
             (filter #(.isDirectory %))
             (map #(.getName %)))))
    (catch Exception e
      (println (str "Error listing wave signatures in category " category ": " (.getMessage e)))
      [])))

(defn list-signature-recordings
  "List all recordings for a specific signature"
  [profile-name category signature]
  (try
    (let [signature-dir (str (fio/config-base-dir) "/profiles/" profile-name "/wave_lexicon/"
                             category "/" signature)]
      (when (.exists (io/file signature-dir))
        (->> (.listFiles (io/file signature-dir))
             (filter #(.isDirectory %))
             (map #(.getName %))
             (sort))))
    (catch Exception e
      (println (str "Error listing recordings for signature " signature ": " (.getMessage e)))
      [])))

(defn list-only-signatures
  "List all signatures in a category"
  [profile-name category]
  (try
    (let [category-dir (fio/get-wave-lexicon-dir profile-name category)]
      (when (.exists (io/file category-dir))
        (->> (io/file category-dir)
             .listFiles
             (filter #(.isDirectory %))
             (map #(.getName %))
             sort
             vec)))
    (catch Exception e
      (println "Error listing wave signatures:" (.getMessage e))
      [])))

(defn list-all-wave-signatures
  "List all categories and their signatures in a user's wave lexicon"
  []
  (try
    (let [profile-name (or (:name ((:get-active-profile @state/state))) "default")
          lexicon-dir (fio/get-wave-lexicon-dir profile-name "")]
      (if (.exists (io/file lexicon-dir))
        (let [categories (list-wave-signature-categories)]
          (if (seq categories)

            (doseq [category categories]
              (println "\tCategory:" category)
              (let [signatures (list-wave-signatures-in-category profile-name category)]
                (if (seq signatures)
                  (doseq [signature signatures]
                    (let [recordings (list-signature-recordings profile-name category signature)
                          recording-count (count recordings)]
                      (println "\t  - Signature:" signature
                               (str "(" recording-count " recordings)"))))
                  (println "  No signatures in this category"))))
            (println "No categories found in wave lexicon"))
          categories)
        (do
          (println "No lexicon directory found for profile" profile-name)
          [])))
    (catch Exception e
      (println "Error listing lexicon categories:" (.getMessage e))
      (.printStackTrace e)
      [])))

(defn list-wave-signatures-by-category
  "List all wave signatures for a specific category"
  [_]
  (let [profile-name (or (:name ((:get-active-profile @state/state))) "default")
        categories (list-wave-signature-categories)]

    (if (seq categories)
      (do
        (println "Available categories:")
        (doall (map-indexed #(println (str (inc %1) ". " %2)) categories))

        (print "\nSelect category number: ")
        (flush)
        (let [input (read-line)
              idx (try (Integer/parseInt input) (catch Exception _ -1))]

          (when (and (>= idx 1) (<= idx (count categories)))
            (let [category (nth categories (dec idx))
                  dir (fio/get-wave-lexicon-dir profile-name category)
                  signatures (when (.exists (io/file dir))
                               (->> (.listFiles (io/file dir))
                                    (filter #(.isDirectory %))
                                    (sort-by #(.getName %))))]

              (if (seq signatures)
                (do
                  (println (format "\nWave signatures for category '%s':" category))
                  (doseq [sig signatures]
                    (let [metadata-file (io/file (str (.getPath sig) "/recording_metadata.edn"))
                          metadata (when (.exists metadata-file)
                                     (try
                                       (clojure.edn/read-string (slurp metadata-file))
                                       (catch Exception _ nil)))
                          timestamp (or (:recorded-at metadata) "Unknown")
                          sample-count (or (:sample-count metadata) "Unknown")]
                      (println (format "  %s (%s samples, recorded at %s)"
                                       (.getName sig)
                                       sample-count
                                       timestamp)))))
                (println (format "\nNo signatures found for category '%s'." category))))))
        (println))
      (println "\nNo wave signature categories found."))))

(defn load-category
  "Load the comprehensive category.edn file"
  [category]
  (try
    (let [profile-name (:name ((:get-active-profile @state/state)))
          category-path (str (System/getProperty "user.home")
                             "/.lor/profiles/" profile-name
                             "/wave_lexicon/" category "/category.edn")]
      (when (.exists (io/file category-path))
        (edn/read-string (slurp category-path))))
    (catch Exception e
      (println "Error loading category intelligence:" (.getMessage e))
      nil)))

(defn load-category-summary
  "Load category data from category.edn for assistance calculation"
  [category]
  (try
    (let [profile-name (:name ((:get-active-profile @state/state)))
          lexicon-path  (fio/get-wave-lexicon-dir profile-name "")
          category-dir (str lexicon-path "/" category)
          category-path (str category-dir "/category.edn")
          category-data (when (.exists (java.io.File. category-path))
                          (read-string (slurp category-path)))]
      (when category-data
        (println "Found signature data structure:")
        (println "  Summary keys:" (keys (:summary category-data)))
        (when (get-in category-data [:summary :all])
          (println "  All keys:" (keys (get-in category-data [:summary :all]))))
        category-data))
    (catch Exception e
      (println "Error loading category data:" (.getMessage e))
      nil)))

(defn load-signature-data
  "Load category data from category.edn for assistance calculation"
  [category signature]
  (try
    (let [profile-name (:name ((:get-active-profile @state/state)))
          lexicon-path  (fio/get-wave-lexicon-dir profile-name "")
          category-dir (str lexicon-path "/" category)
          signature-dir (str category-dir "/" signature)
          signature-path (str signature-dir "/signature.edn")
          signature-data (when (.exists (java.io.File. signature-path))
                           (read-string (slurp signature-path)))]
      (when signature-data
        (println "Found signature data structure:")
        (println "  Summary keys:" (keys (:summary signature-data)))
        (when (get-in signature-data [:summary :all])
          (println "  All keys:" (keys signature-data)))
        signature-data))
    (catch Exception e
      (println "Error loading category data:" (.getMessage e))
      nil)))

(defn save-wave-lexicon-entry!
  "Save a wave lexicon entry for a specific category"
  [profile-name category entry]
  (try
    (let [timestamp (System/currentTimeMillis)
          category-dir (fio/get-wave-lexicon-dir profile-name category)
          entry-path (str category-dir "/" category "_" timestamp)
          metadata-path (str entry-path "/recording_metadata.edn")]
      (fio/ensure-directory! category-dir)
      (fio/ensure-directory! entry-path)

      (spit metadata-path (pr-str (assoc entry
                                         :category category
                                         :created-at (java.util.Date.)
                                         :recording-id (str category "_" timestamp))))
      entry-path)
    (catch Exception e
      (println "Error saving wave lexicon entry:" (.getMessage e))
      (.printStackTrace e)
      nil)))

(defn compute-tensor-difference
  "Compute difference between current band powers and golden tensor"
  [band-powers golden-tensor]
  (let [keys-to-compare (keys golden-tensor)]
    (into {} (map (fn [k]
                    [k (- (get band-powers k 0) (get golden-tensor k 0))])
                  keys-to-compare))))

(defn match-brain-activity
  "Match current brain activity against lexicon entries"
  [current-data]
  (try
    (let [profile ((:get-active-profile @state/state))
          profile-name (:name profile)
          lexicon-dir (str ".home/.lor/profiles/lexicon/" profile-name)

          ; Process current data
          sampling-rate (brainflow/get-sampling-rate (brainflow/get-board-id @state/shim))
          current-powers (calibrate/extract-band-powers current-data sampling-rate)

          ; Load all lexicon entries
          entries (when (.exists (io/file lexicon-dir))
                    (for [file (.listFiles (io/file lexicon-dir))
                          :when (.isFile file)
                          :let [entry (try
                                        (edn/read-string (slurp file))
                                        (catch Exception e nil))]
                          :when entry]
                      entry))

          ; Calculate similarity scores
          scores (for [entry entries
                       :let [entry-powers (:band-powers entry)
                             ; Calculate Euclidean distance between power spectra
                             distance (reduce +
                                              (for [channel-idx (range (min (count current-powers)
                                                                            (count entry-powers)))]
                                                (let [current-ch (nth current-powers channel-idx {})
                                                      entry-ch (nth entry-powers channel-idx {})
                                                      keys (into #{} (concat (keys current-ch) (keys entry-ch)))
                                                      dist-sq (reduce +
                                                                      (for [k keys]
                                                                        (let [curr-val (get current-ch k 0.0)
                                                                              entry-val (get entry-ch k 0.0)
                                                                              diff (- curr-val entry-val)]
                                                                          (* diff diff))))]
                                                  dist-sq)))
                             similarity (/ 1.0 (+ 1.0 (Math/sqrt distance)))]]
                   {:label (:label entry)
                    :similarity similarity})

          ; Sort by similarity (highest first)
          sorted-scores (sort-by :similarity > scores)

          ; Get the best match
          best-match (first sorted-scores)]

      (if best-match
        (do
          (println "Best match:" (:label best-match) "with similarity" (:similarity best-match))
          best-match)
        (do
          (println "No lexicon entries to match against")
          nil)))
    (catch Exception e
      (println "Error matching brain activity:" (.getMessage e)))))

(defn triangulate-signature-features
  "Triangulate between live recording, golden tensor baseline, and existing signature patterns"
  [signature-features calibration-context category signature-name]
  (try
    (let [profile (profiles/get-active-profile)
          profile-name (:name profile)
          existing-signature (try
                               (let [signature-file (str (fio/get-wave-lexicon-dir profile-name category)
                                                         "/" signature-name "/signature.edn")]
                                 (when (.exists (io/file signature-file))
                                   (edn/read-string (slurp signature-file))))
                               (catch Exception e
                                 (println "No existing signature found for" category "/" signature-name)
                                 nil))

          ; Extract the three key data sources
          live-band-powers (:band-powers signature-features)
          golden-tensor-powers (get-in calibration-context [:golden-tensor])
          calibration-baseline-powers (get calibration-context :band-powers)
          existing-signature-powers (get-in existing-signature [:band-powers :average])

          ; Calculate triangulation relationships
          ; 1. Live vs Golden Tensor (deviation from ideal baseline)
          golden-tensor-ratios (when golden-tensor-powers
                                 (into {} (for [[band live-power] live-band-powers]
                                            (let [golden-power (get golden-tensor-powers band 1.0)]
                                              [band (if (> golden-power 0) (/ live-power golden-power) 1.0)]))))

          ; 2. Live vs Current Calibration (deviation from current session baseline)
          calibration-ratios (when calibration-baseline-powers
                               (into {} (for [[band live-power] live-band-powers]
                                          (let [cal-power (get calibration-baseline-powers band 1.0)]
                                            [band (if (> cal-power 0) (/ live-power cal-power) 1.0)]))))

          ; 3. Live vs Existing Signature (similarity to learned pattern)
          signature-ratios (when existing-signature-powers
                             (into {} (for [[band live-power] live-band-powers]
                                        (let [sig-power (get existing-signature-powers band 1.0)]
                                          [band (if (> sig-power 0) (/ live-power sig-power) 1.0)]))))

          ; Calculate multi-dimensional signature strength
          triangulated-strength (let [live-total (reduce + (vals live-band-powers))
                                      golden-total (when golden-tensor-powers
                                                     (reduce + (vals golden-tensor-powers)))
                                      cal-total (when calibration-baseline-powers
                                                  (reduce + (vals calibration-baseline-powers)))
                                      sig-total (when existing-signature-powers
                                                  (reduce + (vals existing-signature-powers)))]
                                  {:live-intensity live-total
                                   :vs-golden-tensor (when golden-total
                                                       (if (> golden-total 0) (/ live-total golden-total) 1.0))
                                   :vs-calibration (when cal-total
                                                     (if (> cal-total 0) (/ live-total cal-total) 1.0))
                                   :vs-signature (when sig-total
                                                   (if (> sig-total 0) (/ live-total sig-total) 1.0))})

          ; Create comprehensive triangulation data
          triangulation-data {:golden-tensor-ratios golden-tensor-ratios
                              :calibration-ratios calibration-ratios
                              :signature-ratios signature-ratios
                              :triangulated-strength triangulated-strength
                              :pattern-recognition {:has-existing-signature (boolean existing-signature-powers)
                                                    :signature-confidence (when signature-ratios
                                                                            (let [ratios (vals signature-ratios)
                                                                                  avg-ratio (/ (reduce + ratios) (count ratios))
                                                                                  variance (/ (reduce + (map #(Math/pow (- % avg-ratio) 2) ratios))
                                                                                              (count ratios))]
                                                                              {:average-similarity avg-ratio
                                                                               :pattern-variance variance}))
                                                    :novelty-score (when (and golden-tensor-ratios calibration-ratios)
                                                                     (let [golden-avg (/ (reduce + (vals golden-tensor-ratios))
                                                                                         (count golden-tensor-ratios))
                                                                           cal-avg (/ (reduce + (vals calibration-ratios))
                                                                                      (count calibration-ratios))]
                                                                       (Math/abs (- golden-avg cal-avg))))}
                              :triangulation-timestamp (System/currentTimeMillis)}

          ; Create detailed comparison matrix
          detailed-comparison {:three-way-analysis
                               (when (and live-band-powers golden-tensor-powers calibration-baseline-powers)
                                 (into {} (for [[band live-power] live-band-powers]
                                            (let [golden-power (get golden-tensor-powers band 1.0)
                                                  cal-power (get calibration-baseline-powers band 1.0)
                                                  sig-power (get existing-signature-powers band 1.0)]
                                              [band {:live live-power
                                                     :golden-tensor golden-power
                                                     :calibration cal-power
                                                     :existing-signature sig-power
                                                     :live-vs-golden (if (> golden-power 0) (/ live-power golden-power) 1.0)
                                                     :live-vs-calibration (if (> cal-power 0) (/ live-power cal-power) 1.0)
                                                     :live-vs-signature (if (> sig-power 0) (/ live-power sig-power) 1.0)
                                                     :golden-vs-calibration (if (> cal-power 0) (/ golden-power cal-power) 1.0)}]))))}]

      ; Return enhanced signature features with full triangulation
      (assoc signature-features
             :triangulation-data triangulation-data
             :calibration-comparison detailed-comparison))

    (catch Exception e
      (println "Error in triangulation:" (.getMessage e))
      signature-features)))

(defn create-wave-signature-context
  "Create a recording context for a wave signature that enhances existing recording context"
  [profile-name category signature-name & {:keys [include-in-aggregation] :or {include-in-aggregation true}}]
  (try
    (let [timestamp (System/currentTimeMillis)
          signature-dir (str (fio/get-wave-lexicon-dir profile-name category)
                             "/" (str/lower-case signature-name) "_" timestamp)
          board-id (brainflow/get-board-id @state/shim)
          device-name (brainflow/get-device-name board-id)

          ; Get existing recording context if it exists
          existing-context @state/recording-context

          ; Wave signature specific metadata
          wave-signature-metadata {:category (name category)
                                   :signature-name (name signature-name)
                                   :is-wave-signature true
                                   :include-in-aggregation include-in-aggregation
                                   :wave-signature-timestamp timestamp}

          ; Enhanced metadata that combines existing with wave signature data
          enhanced-metadata (merge (:metadata existing-context)
                                   wave-signature-metadata)

          context {:lorfile-dir signature-dir
                   :metadata enhanced-metadata
                   :wave-signature-context true}]

      ; Update the recording context by merging, not replacing
      (swap! state/recording-context merge context)

      context)

    (catch Exception e
      (println "Error creating wave signature context:" (.getMessage e))
      (.printStackTrace e)
      nil)))

(defn start-wave-signature-recording!
  "Start recording a wave signature with calibration"
  [category signature-name & {:keys [include-in-aggregation] :or {include-in-aggregation true}}]
  (try
    (when (or (str/blank? category) (str/blank? signature-name))
      (throw (Exception. "Category and signature name must be provided")))
    (let [profile-name (or (:name ((:get-active-profile @state/state))) "default")
          recording-dir (str (fio/get-wave-lexicon-dir profile-name category)
                             "/" signature-name)
          recording-info {:recording-type signature-name
                          :path recording-dir}]

      ; Start recording using existing infrastructure
      (record/start-recording! recording-info)

      ; Fresh calibration update immediately for wave signatures
      (println "Forcing fresh calibration for wave signature...")
      (reset! record/last-calibration-update 0) ; Reset to force immediate update

      ; Enhance recording context with wave signature data
      (swap! state/recording-context merge
             {:wave-signature-metadata {:category (name category)
                                        :signature-name (name signature-name)
                                        :is-wave-signature true
                                        :include-in-aggregration include-in-aggregation
                                        :wave-signature-timestamp (System/currentTimeMillis)}})

      ; Update the main metadata in recording context
      (swap! state/recording-context update-in [:metadata] merge
             {:category (name category)
              :signature-name (name signature-name)
              :is-wave-signature true
              :include-in-aggregation include-in-aggregation
              :force-fresh-calibration true}) ; Flag to ensure fresh calibration

      ; Add start tag
      (let [start-time (System/currentTimeMillis)]
        (swap! state/tags conj {:label (str "WAVE_SIGNATURE_START:"
                                            (name category) "/" (name signature-name))
                                :timestamp start-time}))

      (println "Started wave signature recording for" category "/" signature-name
               (if include-in-aggregation "" "(practice mode - will not be aggregated)"))

      ; Wait a moment then force calibration update
      (future
        (Thread/sleep 1000) ; Give it a second to collect some data
        (record/update-calibration-if-needed!))

      @state/recording-context)
    (catch Exception e
      (println "Error starting wave signature recording:" (.getMessage e))
      (.printStackTrace e)
      nil)))
(defn create-category-context
  "Create a recording context for a wave signature category"
  [profile-name category]
  (try
    (let [timestamp (System/currentTimeMillis)
          category-dir (str (fio/get-wave-lexicon-dir profile-name category)
                            "/" (str/lower-case category) "_recordings")
          board-id (brainflow/get-board-id @state/shim)
          device-name (brainflow/get-device-name board-id)
          metadata {:recording-id (str category "_" timestamp)
                    :start-time timestamp
                    :device-type device-name
                    ;:board-id board-id
                    :sampling-rate (api/get-current-sample-rate)
                    :category-name (name category)
                    :is-category true
                    :version "1.0"
                    #_#_:signatures-count "Maybe this will be included later"}

          context {:lorfile-dir category-dir
                   :metadata metadata}]
      context)
    (catch Exception e
      (println "Error creating wave signature context:" (.getMessage e))
      (.printStackTrace e)
      nil)))

(defn start-category-recording!
  "Start recording a whole category"
  [category]
  (try
    (when (str/blank? category)
      (throw (Exception. "Category and signature name must be provided")))

    (let [profile-name (or (:name ((:get-active-profile @state/state))) "default")
          context (create-category-context profile-name category)
          category-recording-dir (:lorfile-dir context)
          category-recording-info {:recording-type category
                                   :path category-recording-dir}]

      (when-not context
        (throw (Exception. "Failed to create recording context")))

      ; Start the actual recording
      (record/start-recording! category-recording-info)

      ; Store context for recording
      (swap! state/recording-context update-in [:metadata] merge
             {:signature-name (name category)
              :category (name category)
              :is-category true})

      ; Add tag for wave signature start
      (swap! state/tags conj {:label (str (str/upper-case category) "_START:")
                              :timestamp (:start-time (:metadata context))})

      ; Reset EEG data collection
      (reset! state/eeg-data [])

      ; Log success
      (println "Started recording wave signature for" category)
      context)
    (catch Exception e
      (println "Error starting category recording:" (.getMessage e))
      (.printStackTrace e)
      nil)))

(defn calculate-correlation
  "Calculate correlation between two signals"
  [signal1 signal2]
  (try
    (let [n (count signal1)
          mean1 (/ (reduce + signal1) n)
          mean2 (/ (reduce + signal2) n)

          variance1 (/ (reduce + (map #(Math/pow (- % mean1) 2) signal1)) n)
          variance2 (/ (reduce + (map #(Math/pow (- % mean2) 2) signal2)) n)

          std1 (Math/sqrt variance1)
          std2 (Math/sqrt variance2)

          covariance (/ (reduce +
                                (map #(* (- %1 mean1) (- %2 mean2))
                                     signal1 signal2))
                        n)
          correlation (if (and (> std1 0) (> std2 0))
                        (/ covariance (* std1 std2))
                        0.0)]
      correlation)
    (catch Exception e
      (println "Error calculating correlation:" (.getMessage e))
      0.0)))

(defn extract-signature-features
  "Extract features from EEG data for signature matching"
  [eeg-data sampling-rate]
  (try
    (println "Extracting signature features from" (count eeg-data) "EEG samples")

    ; Extract the EEG matrix: [{:eeg [[samples]], :timestamp}] -> [[channels][samples]]
    (let [raw-eeg-matrix (mapcat :eeg eeg-data)
          ; Remove timestamps: [timestamp ch1 ch2 ch3 ch4] -> [ch1 ch2 ch3 ch4]
          without-timestamps (mapv rest raw-eeg-matrix)
          ; Convert to [channels][samples] format
          channels-samples (apply mapv vector without-timestamps)

          ; Extract FRESH band powers from the actual signature data
          signature-band-powers (calibrate/extract-band-powers channels-samples sampling-rate)

          ; Calculate signature-specific statistics
          channel-stats (for [channel channels-samples]
                          (let [mean (/ (reduce + channel) (count channel))
                                variance (/ (reduce + (map #(Math/pow (- % mean) 2) channel))
                                            (count channel))
                                std-dev (Math/sqrt variance)]
                            {:mean mean :std-dev std-dev :variance variance}))

          ; Calculate power distribution (what makes this signature unique)
          total-power (reduce + (vals signature-band-powers))
          power-distribution (into {} (for [[band power] signature-band-powers]
                                        [band (/ power total-power)]))

          ; Signature strength (how distinct this pattern is)
          signature-strength (let [powers (vals signature-band-powers)
                                   max-power (apply max powers)
                                   min-power (apply min powers)]
                               (if (> min-power 0)
                                 (/ max-power min-power)
                                 1.0))]

      {:band-powers signature-band-powers
       :power-distribution power-distribution
       :signature-strength signature-strength
       :channel-statistics channel-stats
       :sample-count (count raw-eeg-matrix)
       :duration (/ (count raw-eeg-matrix) sampling-rate)})

    (catch Exception e
      (println "Error extracting signature features:" (.getMessage e))
      (.printStackTrace e)
      nil)))

(defn create-signature-template
  "Create a template from signature features"
  [features]
  {:band-powers (:band-powers features)
   :temporal-patterns (mapv #(select-keys % [:mean :std-dev]) (:temporal-stats features))
   :coherence-patterns (mapv #(select-keys % [:channels :correlation]) (:channel-coherence features))
   :created-at (java.util.Date.)})

(defn process-wave-signature!
  "Process wave signature with proper triangulation"
  [recording-dir enhanced-metadata]
  (try
    ; Get the actual EEG data from the recording
    (let [eeg-data @state/eeg-data
          sampling-rate (:sampling-rate enhanced-metadata)
          category (:category enhanced-metadata)
          signature-name (:signature-name enhanced-metadata)

          ; Extract signature features from the actual EEG data
          signature-features (extract-signature-features eeg-data sampling-rate)

          ; Get recent calibration context (this should have real calibration factors now)
          calibration-context (get enhanced-metadata :calibration-index)

          ; Triangulate the signature features
          triangulated-features (triangulate-signature-features
                                 signature-features
                                 calibration-context
                                 category
                                 signature-name)

          ; Calculate band distribution from calibration band powers if available
          calibration-band-distribution (when-let [band-powers (get calibration-context :band-powers)]
                                          (let [total-power (reduce + (vals band-powers))]
                                            (when (> total-power 0)
                                              (into {} (for [[band power] band-powers]
                                                         [band (/ power total-power)])))))

          ; Create signature data with triangulated features as the main content
          signature-data {:signature-features triangulated-features

                         ; Reference calibration data (background context) - with calculated distribution
                          :calibration-context {:band-powers (get calibration-context :band-powers)
                                                :calibration-factors (get calibration-context :calibration-factors)
                                                :golden-tensor (get calibration-context :golden-tensor)
                                                :band-distribution (or (get calibration-context :band-distribution)
                                                                       calibration-band-distribution)}
                          :signature-metadata {:category category
                                               :signature-name signature-name
                                               :recording-id (:recording-id enhanced-metadata)
                                               :device-type (:device-type enhanced-metadata)
                                               :channel-count (:channel-count enhanced-metadata)
                                               :sampling-rate sampling-rate
                                               :sample-count (:sample-count enhanced-metadata)
                                               :recorded-at (:recorded-at enhanced-metadata)}
                          :processed-at (java.util.Date.)
                          :tags (:tags enhanced-metadata)
                          :include-in-aggregation (:include-in-aggregation enhanced-metadata)}

          signature-file (str recording-dir "/signature_features.edn")
          metadata-file (str recording-dir "/recording_metadata.edn")]

      (with-open [w (clojure.java.io/writer signature-file)]
        (binding [*print-length* nil *print-level* nil]
          (clojure.pprint/pprint signature-data w)))

      ; Update recording metadata with signature processing info
      (when (.exists (clojure.java.io/file metadata-file))
        (let [existing-metadata (edn/read-string (slurp metadata-file))
              updated-metadata (assoc existing-metadata
                                      :signature-features-path signature-file
                                      :signature-processed-at (java.util.Date.)
                                      :signature-data-available true
                                      :triangulation-completed true)]
          (with-open [w (clojure.java.io/writer metadata-file)]
            (binding [*print-length* nil *print-level* nil]
              (clojure.pprint/pprint updated-metadata w)))))

      (println "Saved triangulated signature features to:" signature-file)
      signature-data)

    (catch Exception e
      (println "Error processing wave signature:" (.getMessage e))
      (.printStackTrace e)
      nil)))

(defn stop-wave-signature-recording!
  "Stop wave signature recording and process the signature"
  [category signature]
  (try
    (let [context @state/recording-context]
      ; Add end tag
      (let [timestamp (System/currentTimeMillis)]
        (swap! state/tags conj {:label (str "WAVE_SIGNATURE_END:"
                                            category "/" signature)
                                :timestamp timestamp}))

      ; Stop recording (this handles the metadata)
      (record/stop-recording!)

      ; Process signature using existing metadata
      (let [recording-dir (:lorfile-dir context)
            metadata-file (str recording-dir "/recording_metadata.edn")
            existing-metadata (when (.exists (io/file metadata-file))
                                (edn/read-string (slurp metadata-file)))
            eeg-data @state/eeg-data
            profile-name (:name ((:get-active-profile @state/state)))
            include-in-aggregation (get-in context [:metadata :include-in-aggregation] true)]

        ; Enhance metadata with wave signature specific fields
        (when existing-metadata
          (let [enhanced-metadata (merge existing-metadata
                                         {:sample-count (count eeg-data)
                                          :tags @state/tags
                                          :category (name category)
                                          :signature-name (name signature)
                                          :is-wave-signature true
                                          :include-in-aggregation include-in-aggregation})]

            ; Write enhanced metadata
            (with-open [w (io/writer metadata-file)]
              (binding [*print-length* nil
                        *print-level* nil]
                (clojure.pprint/pprint enhanced-metadata w)))

            ; Process signature features using existing calibration data
            (when (>= (count eeg-data) 10) ; MIN_SAMPLES_FOR_SIGNATURE
              (process-wave-signature! recording-dir enhanced-metadata)

              ; Auto-aggregate
              (when include-in-aggregation
                (try
                  (category/aggregate-after-recording! profile-name category signature)
                  (println "Category aggregation completed successfully")
                  (catch Exception agg-e
                    (println "Error during category aggregation:" (.getMessage agg-e))))))

            recording-dir))))

    (catch Exception e
      (println "Error stopping wave signature recording:" (.getMessage e))
      (.printStackTrace e)
      (when @state/recording?
        (record/stop-recording!)))))

(defn stop-category-recording!
  "Stop recording a wave signature category and process it"
  [category]
  (try
    (let [context @state/recording-context]

      ; Add end tag
      (let [timestamp (System/currentTimeMillis)]
        (swap! state/tags conj {:label (str category "_END:")
                                :timestamp timestamp}))
      ; Stop recording
      (record/stop-recording!)

          ; Process the recording into a wave signature
      (let [category-recording-dir (:lorfile-dir context)
            metadata-file (str category-recording-dir "/recording_metadata.edn")
            existing-metadata (when (.exists (io/file metadata-file))
                                (edn/read-string (slurp metadata-file)))
            eeg-data @state/eeg-data
            profile-name (:name ((:get-active-profile @state/state)))

                ; Additional signature-specific metadata
            signature-metadata (merge existing-metadata
                                      {:sample-count (count eeg-data)
                                       :tags @state/tags})
            include-in-aggregation false]

        (when existing-metadata
          (let [enhanced-metadata (merge existing-metadata
                                         {:sample-count (count eeg-data)
                                          :tags @state/tags
                                          :category (name category)
                                          :signature-name (name category)
                                          :is-wave-signature false
                                          :include-in-aggregation include-in-aggregation})]

            ; Write enhanced metadata
            (with-open [w (io/writer metadata-file)]
              (binding [*print-length* nil
                        *print-level* nil]
                (clojure.pprint/pprint enhanced-metadata w)))

            category-recording-dir))))
    (catch Exception e
      (println "Error stopping category recording:" (.getMessage e))
      (.printStackTrace e)
      ; Ensure recording is stopped even on error
      (when @state/recording?
        (record/stop-recording!))))
  nil)

(defn add-wave-category
  "Interactive command to add a new wave category"
  []
  (try
    (print "Enter new category name: ")
    (flush)
    (let [category-name (read-line)]
      (when-not (str/blank? category-name)
        (let [config-path (str (fio/config-base-dir) "/config.edn")
              config (if (.exists (io/file config-path))
                       (edn/read-string (slurp config-path))
                       {})
              updated-config (update config :wave-lexicon-categories
                                     #(if (seq %)
                                        (conj % (keyword category-name))
                                        [(keyword category-name)]))]
          (spit config-path (pr-str updated-config)))

        (println "Created new category:" category-name)
        category-name))
    (catch Exception e
      (println "Error adding wave category:" (.getMessage e))
      nil)))

(defn add-wave-signature
  "Interactive command to add a new wave signature"
  [board-shim]
  (if-not board-shim
    (println "No board connected. Please connect to a board first.")
    (try
      ; First, select a category
      (let [profile-name (or (:name ((:get-active-profile @state/state))) "default")
            categories (list-wave-signature-categories)]

        (println "\nAvailable categories:")
        (if (seq categories)
          (do
            (println (str "Found " (count categories) " lexicon categories:"))
            (doseq [cat categories]
              (println "  -" cat))
            (doall (map-indexed #(println (str (inc %1) ". " %2)) categories)))
          (println "No categories found."))

        (println (str (inc (count categories)) ". Create new category"))

        (print "\nSelect category number or enter new category name: ")
        (flush)
        (let [input (read-line)
              category (try
                         (let [idx (Integer/parseInt input)]
                           (if (<= idx (count categories))
                             (nth categories (dec idx))
                             nil))
                         (catch Exception _ nil))

              ; If input is a number and greater than categories count, create new category
              category-name (cond
                              ; Selected existing category
                              category
                              category

                              ; Create new category
                              (and (re-matches #"\d+" input)
                                   (= (Integer/parseInt input) (inc (count categories))))
                              (add-wave-category)

                              ; Input is a new category name
                              :else
                              input)]

          (when-not (str/blank? category-name)
            ; Now select or create a signature within that category
            (let [signatures (list-only-signatures profile-name category-name)]

              (println "\nAvailable signatures in category" category-name ":")
              (if (seq signatures)
                (do
                  (println (str "Found " (count signatures) " signatures:"))
                  (doseq [sig signatures]
                    (println "  -" sig))
                  (doall (map-indexed #(println (str (inc %1) ". " %2)) signatures)))
                (println "No signatures found."))

              (println (str (inc (count signatures)) ". Create new signature"))

              (print "\nSelect signature number or enter new signature name: ")
              (flush)
              (let [sig-input (read-line)
                    signature (try
                                (let [idx (Integer/parseInt sig-input)]
                                  (if (<= idx (count signatures))
                                    (nth signatures (dec idx))
                                    nil))
                                (catch Exception _ nil))

                    ; Determine signature name
                    signature-name (cond
                                     ; Selected existing signature
                                     signature
                                     signature

                                     ; Create new signature name
                                     (and (re-matches #"\d+" sig-input)
                                          (= (Integer/parseInt sig-input) (inc (count signatures))))
                                     (do
                                       (print "Enter new signature name: ")
                                       (flush)
                                       (read-line))

                                     ; Input is a new signature name
                                     :else
                                     sig-input)]

                (when-not (str/blank? signature-name)
                  ; Ask if this is practice or real
                  (println "\nIs this a practice recording? (y/n)")
                  (println "Practice recordings will be saved but won't affect your aggregated signature data.")
                  (print "Enter choice (default: n): ")
                  (flush)
                  (let [practice? (= (str/lower-case (or (read-line) "n")) "y")
                        include-in-aggregation (not practice?)]

                    (println "\nStarting wave signature recording for:" category-name "/" signature-name)
                    (if practice?
                      (println "PRACTICE MODE: This recording will not be included in aggregation.")
                      (println "RECORD MODE: This recording will be included in aggregation."))
                    (println "Focus on the mental state you want to capture.")
                    (println "Press any key when ready to start recording...")
                    (read-line)

                    (let [context (start-wave-signature-recording!
                                   category-name signature-name
                                   :include-in-aggregation include-in-aggregation)]
                      (if context
                        (do
                          (println "Recording started. Please focus...")
                          (println "Recording will automatically stop after 5 seconds, or press any key to stop sooner...")

                          ; Timed stop or manual stop, whichever comes first
                          (let [start-time (System/currentTimeMillis)
                                future-stop (future
                                              (Thread/sleep DEFAULT_RECORDING_DURATION)
                                              (println "Automatic recording stop after 5 seconds")
                                              (stop-wave-signature-recording! category-name signature-name))]

                            ; Wait for user input
                            (read-line)

                            ; Cancel the automatic stop if user pressed key
                            (future-cancel future-stop)

                            ; Stop recording if not already stopped
                            (when @state/recording?
                              (println "Manual recording stop")
                              (stop-wave-signature-recording! category-name signature-name))

                            (println "Wave signature recorded and saved successfully.")))
                        (println "Failed to start recording. Check your device connection."))))))))))
      (catch Exception e
        (println "Error in add-wave-signature:" (.getMessage e))
        (.printStackTrace e)))))

(defn capture-wave-signature!
  "Start capturing a wave signature from ongoing category recording"
  [category signature & {:keys [include-in-aggregation] :or {include-in-aggregation true}}]
  (try
    (when (str/blank? signature)
      (throw (Exception. "Signature name must be provided")))

    (let [profile-name (or (:name ((:get-active-profile @state/state))) "default")
          timestamp (System/currentTimeMillis)
          board-id (brainflow/get-board-id @state/shim)

          signature-base-dir (str (fio/get-wave-lexicon-dir profile-name category) "/" (str/lower-case signature))
          recording-dir (str signature-base-dir "/" (str/lower-case signature) "_" timestamp)

          ; Get the current recording context (from the ongoing "pong" recording)
          current-recording-context @state/recording-context

          ; Create wave signature context that inherits from current recording
          wave-signature-context {:signature signature
                                  :category category
                                  :is-wave-signature true
                                  :include-in-aggregation include-in-aggregation
                                  :capture-start-time timestamp
                                  :recording-dir recording-dir
                                  :board-id board-id
                                  :signature-base-dir signature-base-dir
                                  :start-data-index (count @state/eeg-data)
                                  :parent-recording-context current-recording-context}]

      (.mkdirs (java.io.File. recording-dir))

      ; Store in a separate wave signature state
      (swap! state/state assoc :active-wave-signature wave-signature-context)

      ; Mark the start point in the continuous data stream
      (let [current-data-length (count @state/eeg-data)]
        (swap! state/state assoc-in [:active-wave-signature :start-data-index] current-data-length))

      ; Add start tag to existing recording
      (swap! state/tags conj {:label (str "WAVE_SIGNATURE_START:"
                                          category "/" signature)
                              :timestamp timestamp})

      (println "🎯 Started wave signature capture for" signature
               "(capturing from ongoing category recording)")

      wave-signature-context)

    (catch Exception e
      (println "Error starting wave signature capture:" (.getMessage e))
      (.printStackTrace e)
      nil)))

(defn stop-wave-signature-capture!
  "Stop capturing wave signature and create standalone recording files"
  [signature]
  (try
    (let [wave-sig-context (get @state/state :active-wave-signature)]
      (if wave-sig-context
        (let [timestamp (System/currentTimeMillis)
              board-id (:board-id wave-sig-context)
              category (:category wave-sig-context)
              start-index (:start-data-index wave-sig-context)
              current-data-length (count @state/eeg-data)

              ; Extract the wave signature data segment
              raw-data-segment (subvec @state/eeg-data start-index current-data-length)

              recording-dir (:recording-dir wave-sig-context)

              ; Get relevant tags for this signature
              start-time (:capture-start-time wave-sig-context)
              relevant-tags (filter #(>= (:timestamp %) start-time) @state/tags)]

          ; Add end tag
          (swap! state/tags conj {:label (str "WAVE_SIGNATURE_END:"
                                              category "/" signature)
                                  :timestamp timestamp})

          ; Create complete metadata for this wave signature
          (let [signature-metadata {:signature signature
                                    :category category
                                    :is-wave-signature true
                                    :capture-start-time start-time
                                    :capture-end-time timestamp
                                    :sample-count (count raw-data-segment)
                                    :data-start-index start-index
                                    :data-end-index current-data-length
                                    :recording-id (str signature "_" timestamp)
                                    :version "1.0"
                                    :board-id board-id
                                    :tags relevant-tags}]

            ; Write recording_metadata.edn
            (with-open [w (io/writer (str recording-dir "/recording_metadata.edn"))]
              (binding [*print-length* nil
                        *print-level* nil]
                (clojure.pprint/pprint signature-metadata w)))

            ; Write tags.edn
            (with-open [w (io/writer (str recording-dir "/tags.edn"))]
              (binding [*print-length* nil
                        *print-level* nil]
                (clojure.pprint/pprint relevant-tags w)))

            ; Write the extracted wave signature data using write-lor!
            ; Temporarily set the recording context to point to our wave signature directory
            (let [original-recording-context @state/recording-context
                  temp-recording-context (assoc original-recording-context :lorfile-dir recording-dir)]

              ; Temporarily update the recording context
              (reset! state/recording-context temp-recording-context)

              (try
                ; Use the existing write-lor! function to write the extracted segment
                (let [lor-result (lor/write-lor! raw-data-segment relevant-tags board-id)]
                  (if lor-result
                    (println "✅ Wave signature .lor files written successfully")
                    (println "❌ Error writing wave signature .lor files")))

                (finally
                  ; Restore the original recording context
                  (reset! state/recording-context original-recording-context))))

            ; Process signature features if we have enough data
            (when (>= (count raw-data-segment) 10) ; MIN_SAMPLES_FOR_SIGNATURE
              (try
                ; Generate signature_features.edn using existing processing
                (process-wave-signature! recording-dir signature-metadata)
                (println "✅ Wave signature features processed")

                ; Auto-aggregate if enabled - this updates the signature.edn file
                (when (:include-in-aggregation wave-sig-context)
                  (try
                    (let [profile-name (or (:name ((:get-active-profile @state/state))) "default")
                          category (:category wave-sig-context)]

                      (category/aggregate-after-recording! profile-name category signature)
                      (println "✅ Signature aggregation completed successfully"))
                    (catch Exception agg-e
                      (println "❌ Error during signature aggregation:" (.getMessage agg-e)))))

                (catch Exception proc-e
                  (println "❌ Error processing wave signature:" (.getMessage proc-e)))))

            ; Clear the wave signature context
            (swap! state/state dissoc :active-wave-signature)

            (println "✅ Wave signature capture completed:" recording-dir)
            (println "📁 Files created:")
            (println "  - recording_metadata.edn")
            (println "  - tags.edn")
            (println "  - [1-" (count (api/get-current-channels)) "].lor files (" (count raw-data-segment) "samples each)")
            (println "  - signature_features.edn (if processed)")

            {:success true
             :recording-dir recording-dir
             :sample-count (count raw-data-segment)}))

        (do
          (println "❌ No active wave signature capture to stop")
          {:success false :error "No active wave signature capture"})))

    (catch Exception e
      (println "Error stopping wave signature capture:" (.getMessage e))
      (.printStackTrace e)
      {:success false :error (.getMessage e)})))

(defn load-category-template
  "Load a category template for matching"
  [profile-name category]
  (try
    (let [category-dir (fio/get-wave-lexicon-dir profile-name category)
          template-file (str category-dir "/" category "/category_template.edn")]

      (when (.exists (io/file template-file))
        (edn/read-string (slurp template-file))))
    (catch Exception e
      (println "Error loading category template:" (.getMessage e))
      nil)))

(defn calculate-temporal-similarity
  "Calculate similarity between temporal patterns"
  [current-stats template-patterns]
  (try
    (let [CURRENT_CHANNEL_COUNT (count (api/get-current-channels))
          channel-similarities (for [i (range CURRENT_CHANNEL_COUNT)
                                     :let [current-chan (nth current-stats i)
                                           template-chan (nth template-patterns i)]]
                                 (let [mean-diff (Math/abs (- (:mean current-chan)
                                                              (:mean template-chan)))
                                       std-diff (Math/abs (- (:std-dev current-chan)
                                                             (:std-dev template-chan)))
                                       ; Normalize differences
                                       mean-sim (/ 1.0 (+ 1.0 mean-diff))
                                       std-sim (/ 1.0 (+ 1.0 std-diff))]
                                   ; Average for this channel
                                   (/ (+ mean-sim std-sim) 2.0)))
          ; Average across channels
          overall-similarity (if (pos? CURRENT_CHANNEL_COUNT)
                               (/ (reduce + channel-similarities) CURRENT_CHANNEL_COUNT)
                               0.5)]
      overall-similarity)
    (catch Exception e
      (println "Error calculating temporal similarity:" (.getMessage e))
      0.5)))

(defn calculate-coherence-similarity
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

(defn calculate-similarity
  "Calculate similarity between two feature sets"
  [current-features template-features]
  (try
    (let [; Extract band powers for comparison
          current-powers (get-in current-features [:band-powers])
          template-powers (get-in template-features [:band-powers])

          ; Calculate Euclidean distance between power spectra
          band-keys (into #{} (concat (keys current-powers) (keys template-powers)))
          power-distance-sq (reduce +
                                    (for [k band-keys]
                                      (let [curr-val (get current-powers k 0.0)
                                            template-val (get template-powers k 0.0)
                                            diff (- curr-val template-val)]
                                        (* diff diff))))

          ; Compare temporal patterns (if available)
          current-temporal (get-in current-features [:temporal-stats])
          template-temporal (get-in template-features [:temporal-patterns])

          temporal-distance (when (and current-temporal template-temporal)
                              (reduce +
                                      (for [i (range (min (count current-temporal)
                                                          (count template-temporal)))]
                                        (let [curr-stats (get current-temporal i)
                                              temp-stats (get template-temporal i)
                                              mean-diff (- (get curr-stats :mean 0)
                                                           (get temp-stats :mean 0))
                                              std-diff (- (get curr-stats :std-dev 0)
                                                          (get temp-stats :std-dev 0))]
                                          (+ (* mean-diff mean-diff) (* std-diff std-diff))))))

          ; Combined distance
          total-distance (+ power-distance-sq (or temporal-distance 0))

          ; Convert to similarity score (0-1)
          similarity (/ 1.0 (+ 1.0 (Math/sqrt total-distance)))]

      similarity)
    (catch Exception e
      (println "Error calculating similarity:" (.getMessage e))
      0.0)))

(defn calculate-signature-similarity
  "Calculate similarity between current features and a template"
  [current-features template]
  (try
    (let [; Calculate band power similarity (most important)
          band-similarity (calculate-band-power-similarity
                           (:band-powers current-features)
                           (:band-powers template))

          ; Calculate temporal pattern similarity
          temporal-similarity (if (and (:temporal-stats current-features)
                                       (:temporal-patterns template))
                                (calculate-temporal-similarity
                                 (:temporal-stats current-features)
                                 (:temporal-patterns template))
                                0.5) ; Default if not available

          ; Calculate coherence pattern similarity
          coherence-similarity (if (and (:channel-coherence current-features)
                                        (:coherence-patterns template))
                                 (calculate-coherence-similarity
                                  (:channel-coherence current-features)
                                  (:coherence-patterns template))
                                 0.5) ; Default if not available

          ; Weighted combination (band powers most important)
          combined-similarity (+ (* 0.6 band-similarity)
                                 (* 0.2 temporal-similarity)
                                 (* 0.2 coherence-similarity))]

      combined-similarity)
    (catch Exception e
      (println "Error calculating signature similarity:" (.getMessage e))
      0.0)))

(defn similarity-score
  "Calculate similarity score between current features and a template"
  [features template]
  (try
    (let [; Compare band powers (main feature)
          bp-current (:band-powers features)
          bp-template (:band-powers template)

          ; Calculate similarity for each band
          band-scores (for [band [:delta :theta :alpha :beta :gamma]]
                        (let [c (get bp-current band 0.0)
                              t (get bp-template band 0.0)
                              diff (Math/abs (- c t))
                              max-val (max 0.0001 (max c t))  ; Avoid division by zero
                              similarity (- 1.0 (min 1.0 (/ diff max-val)))]
                          similarity))

          ; Average band power similarity
          avg-band-score (/ (apply + band-scores) (count band-scores))

          ; Could add more feature comparisons here

          ; Overall score (can adjust weights)
          overall-score avg-band-score]

      overall-score)
    (catch Exception e
      (println "Error calculating similarity:" (.getMessage e))
      0.0)))

(defn calculate-feature-distance
  "Calculate distance between two feature sets"
  [features1 features2]
  (try
    (let [; Calculate band power distance
          band-dist (let [bp1 (:band-powers features1)
                          bp2 (:band-powers features2)
                          keys (keys bp1)]
                      (/ (reduce + (for [k keys
                                         :let [v1 (get bp1 k 0)
                                               v2 (get bp2 k 0)]]
                                     (Math/pow (- v1 v2) 2)))
                         (count keys)))

          ; Calculate temporal pattern distance
          temp-dist (let [tp1 (:temporal-patterns features1)
                          tp2 (:temporal-patterns features2)]
                      (if (and tp1 tp2 (= (count tp1) (count tp2)))
                        (/ (reduce + (for [i (range (count tp1))
                                           :let [t1 (nth tp1 i)
                                                 t2 (nth tp2 i)]]
                                       (+ (Math/pow (- (:mean t1) (:mean t2)) 2)
                                          (Math/pow (- (:std-dev t1) (:std-dev t2)) 2))))
                           (count tp1))
                        1.0))

          ; Calculate coherence pattern distance (simplified)
          coh-dist (let [cp1 (:coherence-patterns features1)
                         cp2 (:coherence-patterns features2)]
                     (if (and cp1 cp2 (= (count cp1) (count cp2)))
                       (/ (reduce + (for [i (range (count cp1))
                                          :let [c1 (nth cp1 i)
                                                c2 (nth cp2 i)]]
                                      (Math/pow (- (:correlation c1) (:correlation c2)) 2)))
                          (count cp1))
                       1.0))

          ; Weighted total distance
          total-dist (+ (* 0.5 band-dist)
                        (* 0.3 temp-dist)
                        (* 0.2 coh-dist))

          ; Convert to similarity score (0-1)
          similarity (max 0.0 (min 1.0 (- 1.0 (/ total-dist 3.0))))]
      similarity)
    (catch Exception e
      (println "Error calculating feature distance:" (.getMessage e))
      0.0)))

(defn match-against-category
  "Match current EEG data against a category template"
  [eeg-data profile-name category]
  (try
    (let [category-dir (fio/get-wave-lexicon-dir profile-name category)
          template-file (str category-dir "/category_template.edn")]

      (if (.exists (io/file template-file))
        (let [template (edn/read-string (slurp template-file))
              sampling-rate (brainflow/get-sampling-rate (brainflow/get-board-id @state/shim))
              features (extract-signature-features eeg-data sampling-rate)
              feature-template (create-signature-template features)
              similarity (calculate-feature-distance feature-template template)]

          {:category category
           :confidence similarity
           :timestamp (System/currentTimeMillis)})

        {:category category
         :confidence 0.0
         :timestamp (System/currentTimeMillis)}))
    (catch Exception e
      (println "Error matching against category:" (.getMessage e))
      {:category category
       :confidence 0.0
       :timestamp (System/currentTimeMillis)
       :error (.getMessage e)})))

(defn match-live-activity
  "Match current brain activity against wave signatures"
  [current-data]
  (try
    (let [SRATE (api/get-current-sample-rate)
          profile-name (or (:name ((:get-active-profile @state/state))) "default")
          normalized-data (stream/normalize-data-format current-data)
          current-features (extract-signature-features normalized-data SRATE)
          categories (list-wave-signature-categories)]

      (if (empty? categories)
        (do
          (println "No wave signature categories available")
          nil)

        ; Calculate similarity with each category template
        (let [similarities (for [category categories
                                 :let [template (load-category-template profile-name category)]
                                 :when template]
                             {:category category
                              :similarity (calculate-signature-similarity
                                           current-features
                                           template)})

              ; Sort by similarity (highest first)
              sorted-similarities (sort-by :similarity > similarities)

              ; Get best match above threshold
              best-match (first (filter #(>= (:similarity %) SIMILARITY_THRESHOLD)
                                        sorted-similarities))]
          (if best-match
            (do
              (println "Detected activity:" (:category best-match)
                       "with confidence" (:similarity best-match))
              best-match)
            (do
              (println "No matching activity detected")
              nil)))))
    (catch Exception e
      (println "Error matching live activity:" (.getMessage e))
      (.printStackTrace e)
      nil)))

(defn initialize-lexicon! []
  (state/register-fn! :start-wave-signature-recording!  start-wave-signature-recording!)
  (state/register-fn! :stop-wave-signature-recording!   stop-wave-signature-recording!)
  (state/register-fn! :list-all-wave-signatures         list-all-wave-signatures)
  (state/register-fn! :list-wave-signature-categories   list-wave-signature-categories)
  (state/register-fn! :list-wave-signatures-by-category list-wave-signatures-by-category)
  (state/register-fn! :save-wave-lexicon-entry!         save-wave-lexicon-entry!)
  (state/register-fn! :match-live-activity              match-live-activity)
  (state/register-fn! :get-category-stats               category/get-category-stats)
  (state/register-fn! :add-wave-signature               add-wave-signature)
  (state/register-fn! :fill-initial-lexicon!            fill-initial-lexicon!))