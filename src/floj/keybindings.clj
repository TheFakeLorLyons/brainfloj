(ns floj.keybindings
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [floj.api :as api]
            [floj.brainflow.board-shim :as brainflow]
            [floj.brainflow.board-ids :as id]
            [floj.io :as fio]
            [floj.lor :as lor]
            [floj.profiles :as profiles]
            [floj.state :as state]
            [floj.wave-refraction :as refraction])
  (:import [brainflow BoardIds]))

(declare default-commands)
(declare default-command-map)

(defn load-command-map
  "Load command map from a file"
  [path]
  (try
    (edn/read-string (slurp path))
    (catch Exception e
      (println "Warning: Failed to load command map from" path ":" (.getMessage e))
      default-command-map)))

(defn get-keymap-path
  "Get path to the default keymap file"
  []
  (str (fio/config-base-dir) "/command-map.edn"))

(defn normalize-keys
  "Convert string keys to char keys where applicable"
  [m]
  (into {} (map (fn [[k v]]
                  [(if (and (string? k) (= 1 (count k))) (first k) k) v])
                m)))

(defn read-command-map-file
  "Read the command map file"
  [path]
  (try
    (let [content (slurp path)
          parsed (read-string content)]
      (normalize-keys parsed))
    (catch Exception e
      (println "Error reading command map file:" (.getMessage e))
      {})))

(defn write-command-map-file
  "Write command map to file"
  [path command-map]
  (spit path (pr-str command-map)))

(defn configure-command-map
  "Configure the command map, ensuring new commands are always added and changed commands are updated"
  []
  (let [path (get-keymap-path)
        file (io/file path)]
    (if (not (.exists file))
      (do
        (println "Creating default command map")
        (io/make-parents file)
        (write-command-map-file path default-command-map))
      (let [existing-map (read-command-map-file path)
            new-commands (into {} (filter (fn [[k _]] (not (contains? existing-map k))) default-command-map))
            changed-commands (into {} (filter (fn [[k v]]
                                                (and (contains? existing-map k)
                                                     (not= (get existing-map k) v)))
                                              default-command-map))
            needs-update? (or (seq new-commands) (seq changed-commands))
            updated-map (if needs-update?
                          (merge existing-map new-commands changed-commands)
                          existing-map)]
        (when needs-update?
          (println "Updating command map with" (count new-commands) "new commands and"
                   (count changed-commands) "changed commands")
          (write-command-map-file path updated-map)))))
  (get-keymap-path))

(defn get-active-command-map
  []
  (if @state/active-keymap
    @state/active-keymap
    (do
      (configure-command-map)
      (let [command-map (read-command-map-file (get-keymap-path))]
        (reset! state/active-keymap command-map)
        @state/active-keymap))))

(defn get-command-for-key
  "Get the command for a given key"
  [k]
  (get-in (get-active-command-map) [k :command]))

(defn key-for-command
  "Find the key assigned to a command"
  [cmd]
  (first (for [[k v] (get-active-command-map) :when (= (:command v) cmd)] k)))

(defn get-command-description
  "Get description for a command"
  [cmd-key]
  (or (get-in (get-active-command-map) [(key-for-command cmd-key) :description])
      (str cmd-key)))

(defn display-help []
  (println "\nAvailable commands:")
  (let [command-map (get-active-command-map)]
    (doseq [[k data] (sort-by first command-map)]
      (println (str k " - " (:description data)))))
  (println "\nPress 'd' to enter direct key mode (no Enter needed for commands)"))

(defn create-recording-function [fn-key]
  (fn [_]
    (if-let [f (fn-key @state/state)]
      (f)
      (println (str (name fn-key) " function not registered")))))

(defn create-tag-function [fn-tag]
  (fn [_]
    (if-let [f (fn-tag @state/state)]
      (f fn-tag)
      (println "Tag function not registered"))))

(defn get-key-bindings
  "Extracts key-to-command bindings"
  []
  (into {} (map (fn [[k v]] [k (:command v)]) (get-active-command-map))))


(defn load-profile-keymap!
  "Load keymap for a specific profile, ensuring latest commands are included"
  []
  (let [keymap-path (get-keymap-path)]
    (configure-command-map)
    (if (.exists (io/file keymap-path))
      (let [keymap-bindings (load-command-map keymap-path)]
        (reset! state/active-keymap keymap-bindings)
        keymap-bindings)
      (do
        (println "No keymap found, using default keybindings")
        (reset! state/active-keymap default-command-map)
        default-command-map))))

(defn save-profile-keymap!
  "Save custom keybindings (same for all profiles, stored in the default keymap)"
  [bindings]
  (let [keymap-path (get-keymap-path)]
    (spit keymap-path (pr-str bindings))
    bindings))

(defn get-keymap
  "Build a keymap function table from the active bindings"
  []
  (let [bindings (get-key-bindings)]
    (into {} (for [[k cmd] bindings]
               [k (get default-commands cmd
                       (fn [_] (println "Command not implemented:" cmd)))]))))

(def default-command-map
  {:h {:command :help
       :description "Show this help"}

       ; Connection to the device
   :b {:command :bluetooth-connection!
       :description "Scan for a wireless BCI"}
   :B {:command :switch-board!
       :description "Change to another board"}
   :x {:command :connect!
       :description "Connect to your board!!"}

       ; Basic profile management
   :P {:command :create-profile
       :description "Create a new profile containing calibration settings and keybindings"}
   :p {:command :switch-profile
       :description "Change profile"}
   :D {:command :delete-profile
       :description "Delete a profile"}

       ; Board/Profile/Signal information
   :d {:command :describe-board
       :description "Displays the current values of the eeg-data stream"}
   :a {:command :active-profile?
       :description "Display the active profile"}
   :i {:command :signal-info
       :description "Check signal quality (during recording)"}
   :I {:command :get-board-info
       :description "Get board information (any time)"}
   :r {:command :recording-status
       :description "Show recording status"}

       ; Tag/Mark a recording with a timestamp marker
   :1 {:command :tag-channel-1
       :description "Add tag: channel-1"}
   :2 {:command :tag-channel-2
       :description "Add tag: channel-2"}
   :3 {:command :tag-channel-3
       :description "Add tag: channel-3"}
   :4 {:command :tag-channel-4
       :description "Add tag: channel-4"}
   :5 {:command :tag-event
       :description "Add tag event"}

       ; Settings
   :k {:command :customize-keys
       :description "Customize keybindings"}
   :n {:command :set-session-name
       :description "Set new session name"}

       ; Basic recording
   :s {:command :start-recording
       :description "Start recording"}
   :e {:command :stop-recording
       :description "Stop recording and save as lorfile"}

       ; Reading recording files
   :l {:command :list-lorfiles
       :description "List available eeg recordings"}
   :R {:command :read-lor
       :description "Parse and display an available lorfile"}

       ; Calibration
   :C {:command :calibrate!
       :description "Begin calibration routine (30s)"}
   := {:command :cursor-test
       :description "Start cursor control test window"}
   :0 {:command :calibrate-cursor
       :description "Calibrate cursor control"}
   :> {:command :start-cursor
       :description "Start cursor control"}
   :. {:command :stop-cursor
       :description "Stop cursor control"}

       ; Wave signature functions
   :A {:command :add-wave-signature
       :description "Add a wave signature to your wave lexicon"}
   :W {:command :list-wave-signature-categories
       :description "View the categories of signatures in your wave lexicon"}
   :w {:command :list-all-wave-signatures
       :description "List of currently created wave signatures"}
   :g {:command :list-wave-signatures-by-category
       :description "List of signatures of a particular category"}
   :M {:command :match-live-activity
       :description "Match current brain activity against your wave lexicon"}
   :T {:command :train-model
       :description "Train a model based on your wave signatures"}

       ; Exit
   :q {:command :quit
       :description "Quit the application"}})

(def default-commands
  {:help (fn [_]
           (println "\nAvailable commands:")
           (let [bindings (get-key-bindings)]
             (doseq [[key cmd] (sort-by first bindings)]
               (println (str key " - " cmd)))))


    ; Connection to the device
   :bluetooth-connection! (fn [_]
                            (if-let [f (:bluetooth-connection! @state/state)]
                              (f nil)
                              (println "Bluetooth connection function not registered")))
    ; To be expanded for all board types
   :switch-board!  (fn [_]
                     (if @state/recording?
                       (println "Cannot switch board while recording!")
                       (do
                         (println "Available board types:")
                         (println "1 - Synthetic (testing)")
                         (println "2 - OpenBCI Ganglion")
                         (println "3 - OpenBCI Cyton")
                         (println "4 - OpenBCI Cyton+Daisy")
                         (println "\nEnter board number:")
                         (let [selection (read-line)
                               board-id (case selection
                                          "1" BoardIds/SYNTHETIC_BOARD
                                          "2" BoardIds/GANGLION_BOARD
                                          "3" BoardIds/CYTON_BOARD
                                          "4" BoardIds/CYTON_DAISY_BOARD
                                          BoardIds/SYNTHETIC_BOARD)
                               active-profile (:get-active-profile @state/state)
                               profile-name (:name active-profile)
                               updated-profile (assoc active-profile :board-id board-id)
                               profile-path (str (System/getProperty "user.home")
                                                 "/.lor/profiles/" profile-name ".edn")]

                           (spit profile-path (pr-str updated-profile))
                           (println "Board type updated. Please restart the application.")
                           (System/exit 0)))))
   :connect! (fn [_]
               (println "Enter the board MAC address:")
               (let [mac-address (read-line)
                     _ (println "Enter the COM port for the board's BLE dongle (e.g., COM3):")
                     serial-port (read-line)]
                 (api/connect! mac-address serial-port)))


    ; Basic profile management
   :create-profile   (fn [_]
                       (print "Enter new profile name: ")
                       (flush)
                       (let [name (read-line)]
                         ((:create-profile! @state/state) name)
                         ((:set-default-profile! @state/state) name)))
   :switch-profile   (fn [_]
                       ((:switch-profile! @state/state)))
   :delete-profile   (fn [_]
                       ((:delete-profile @state/state)))


    ; Board/Profile/Signal information
   :describe-board   (fn [_]
                       ((println (:describe-board @state/state))))

   :active-profile?  (fn [_]
                       (profiles/show-current-profile))
   :signal-info (fn [_]
                  (if @state/recording?
                    (let [last-samples (last @state/eeg-data)
                          channel-count (count last-samples)
                          signal-stats (for [i (range channel-count)]
                                         (let [samples (map #(nth % i) (take-last 50 @state/eeg-data))
                                               mean (/ (apply + samples) (count samples))
                                               stdev (Math/sqrt (/ (apply + (map #(Math/pow (- % mean) 2) samples)) (count samples)))]
                                           {:channel i
                                            :mean (double mean)
                                            :stdev (double stdev)
                                            :quality (cond
                                                       (< stdev 0.1) "Poor (Too stable)"
                                                       (> stdev 300) "Poor (Too noisy)"
                                                       :else "Good")}))
                          flat-channels (filter #(< (:stdev %) 0.1) signal-stats)
                          noisy-channels (filter #(> (:stdev %) 300) signal-stats)]
                      (println "Signal quality check:")
                      (doseq [stat signal-stats]
                        (println (format "Channel %d: Mean: %.2f, StdDev: %.2f, Quality: %s"
                                         (:channel stat) (:mean stat) (:stdev stat) (:quality stat))))
                      (when (seq flat-channels)
                        (println "Warning: These channels appear flat:" (map :channel flat-channels)))
                      (when (seq noisy-channels)
                        (println "Warning: These channels appear noisy:" (map :channel noisy-channels))))
                    (println "Not recording. Start recording first to check signal quality.")))
   :get-board-info (fn [_]
                     (try
                       (let [shim @state/shim
                             board-type (id/board-types (brainflow/get-board-id shim))
                             channels (brainflow/get-channel-data :eeg shim)
                             count (count channels)
                             #_#_accel-channels (:accel-channels info)
                             sampling-rate (brainflow/get-sampling-rate (brainflow/get-board-id shim))
                             is-prepared (brainflow/board-ready? shim)
                             is-recording @state/recording?]
                         (when shim
                           (println "\nBoard Information:")
                           (println "-------------------")
                           (println "Board Type:" board-type)
                           (println "Channels:" channels)
                           (println "Number of Channels:" count)
                           (println "Sampling Rate:" sampling-rate "Hz")
                           (println "Board Prepared:" is-prepared)
                           (println "Currently Recording:" is-recording)))
                       (catch Exception e
                         (println "Error getting board info:" (.getMessage e))
                         {:error (.getMessage e)})))
   :recording-status (fn [_]
                       (println "Recording status:")
                       (println "- Recording active:" @state/recording?)
                       (println "- Session name:" @state/current-session-name)
                       (println "- Data points collected:" (count (or @state/eeg-data [])))
                       #_(println  (str "current data: " @state/eeg-data));fills the screen with collected data
                       (println "- Tags added:" (count (or @state/tags [])))
                       (let [active-profile-name (or (:name (profiles/get-active-profile)) "[Not available]")]
                         (if active-profile-name
                           (println "- Active profile:" active-profile-name)
                           (println "- Active profile: [Not available]")))
                       (when (and @state/recording?
                                  (seq @state/tags))
                         (let [duration-ms (- (System/currentTimeMillis)
                                              (:timestamp (first @state/tags)))
                               duration-sec (/ duration-ms 1000.0)]
                           (println "- Recording duration:" (format "%.1f seconds" duration-sec)))))


    ; Tag/Mark a recording with a timestamp marker
   :tag-channel-1    (create-tag-function :channel-1)
   :tag-channel-2    (create-tag-function :channel-2)
   :tag-channel-3    (create-tag-function :channel-3)
   :tag-channel-4    (create-tag-function :channel-4)
   :tag-event        (create-tag-function :tag-event)

    ; Settings
   #_#_:customize-keys (nil)
   :set-session-name (fn [_]
                       (println "Enter new session name:")
                       (let [name (read-line)]
                         (reset! state/current-session-name name)
                         (println "Session name set to:" name)))


    ; Basic recording
   :start-recording (create-recording-function :start-recording!)
   :stop-recording  (create-recording-function :stop-recording!)

    ; Reading recording files
   :list-lorfiles   (fn [_]
                      (lor/display-recordings (lor/list-recordings)))

   :read-lor        (fn [_]
                      (when-let [selected-lor (lor/select-recording)]
                        (lor/read-lor! selected-lor)))

    ; Calibration
   :calibrate! (fn [_]
                 (refraction/calibrate!))

   :cursor-test (fn [board-shim]
                  (if board-shim
                    ((:start-cursor-test! @state/state) (brainflow/get-board-id @board-shim))
                    (println "No board connected. Please connect to a board first.")))
   :calibrate-cursor (fn [board-shim]
                       (if board-shim
                         ((:calibrate-cursor! @state/state) board-shim 5)
                         (println "No board connected. Please connect to a board first.")))
   :start-cursor (fn [board-shim]
                   (if board-shim
                     ((:start-cursor-control! @state/state) board-shim)
                     (println "No board connected. Please connect to a board first.")))
   :stop-cursor (fn [_]
                  ((:stop-cursor-control! @state/state)))


    ; Wave signature functions
   :add-wave-signature                (fn [_]
                                        ((:add-wave-signature @state/state) @state/shim))
   :list-wave-signature-categories    (fn [_]
                                        ((:list-wave-signature-categories @state/state)))
   :list-all-wave-signatures          (fn [_]
                                        ((:list-all-wave-signatures @state/state)))
   :list-wave-signatures-by-category  (fn [_]
                                        ((:list-wave-signatures-by-category @state/state)))
   :match-live-activity              (fn [_]
                                       (if-let [f (:match-live-activity @state/state)]
                                         (let [result (f @state/eeg-data)]
                                           (if result
                                             (println "Match found:" (:category result)
                                                      "with confidence" (format "%.2f" (:similarity result)))
                                             (println "No match found in wave lexicon")))
                                         (println "Match live activity function not registered")))
   :train-model                      (fn [_]
                                       (:train-model @state/state))

    ;Leave!
   :quit (fn [_]
           (if @state/recording?
             (println "Please stop recording first with '"
                      (key-for-command :stop-recording) "'")
             (do
               (println "Releasing session and quitting...")
               (when-let [f (:release-board! state/state)]
                 (f @state/shim))
               (System/exit 0))))

   :else (println "Unknown command")})

(defn initialize-keybindings! []
  (state/register-fn! :create-command-map!      configure-command-map)
  (state/register-fn! :get-default-command-map! default-command-map))

