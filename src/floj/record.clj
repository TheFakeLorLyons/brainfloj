(ns floj.record
  (:require [floj.brainflow.boardshim :as brainflow]
            [floj.api :as api]
            [floj.keybindings :as kb]
            [floj.state :as state])
  (:import [java.awt.event KeyListener KeyEvent]
           [javax.swing JFrame]))

(defn tag! [label]
  (when @state/recording?
    (let [timestamp (System/currentTimeMillis)]
      (swap! state/tags conj {:timestamp timestamp :label label})
      (println "Tagged:" label "at" timestamp))))

(defn record-loop! 
  [interval-ms]
  (let [fut (future
             (try
               (loop []
                 (when @state/recording?
                   (let [data (brainflow/get-board-data @state/shim)]
                     (when (seq data)
                       (swap! state/eeg-data conj data)))
                   (Thread/sleep interval-ms)
                   (recur)))
               (catch Exception e
                 (println "Error in recording loop:" (.getMessage e))
                 (reset! state/recording? false))))]
   (swap! state/state assoc :recording-future fut)))

(defn start-recording!
  []
  (if @state/recording?
    (println "Already recording!")
    (do
      (println "Starting recording...")
      (reset! state/recording? true)
      (reset! state/eeg-data [])
      (reset! state/tags [])
      (brainflow/start-stream! @state/shim)
      (record-loop! 100))))

(defn stop-recording!
  []
  (let [shim @state/shim]
    (if-not @state/recording?
    (println "Not currently recording!")
    (do
      (println "Stopping recording...")
      (reset! state/recording? false)
      (Thread/sleep 200)
      (brainflow/stop-stream! shim)
      (let [board-id (brainflow/get-board-id  shim)
            eeg-data (or @state/eeg-data [])
            tags (or @state/tags [])
            current-session-name @state/current-session-name
            write-lor-fn (:write-lor! @state/state)]
        (if write-lor-fn
          (let [lorfile-dir (write-lor-fn eeg-data tags current-session-name board-id)]
            lorfile-dir)
          (println "Error: write-lor! function is not registered in state")))))))

(defn execute-command
  "Execute a command by its key"
  [input board-shim]
  (let [keymap (kb/get-keymap)]
    (if-let [cmd-fn (get keymap (keyword (str input)))]
      (try
        (cmd-fn board-shim)
        (catch Exception e
          (println "Error executing command:" (.getMessage e))))
      (println "Unknown command:" key))))

(defn direct-key-mode [board-shim]
  (println "\nEntering direct key mode (no Enter needed)")
  (println "Press ESC to exit this mode")

  (let [frame (JFrame. "Key Listener (Hidden)")
        key-listener (proxy [KeyListener] []
                       (keyPressed [e]
                         (let [key-char (.getKeyChar e)
                               key-code (.getKeyCode e)]
                           (if (= key-code KeyEvent/VK_ESCAPE)
                             (do
                               (.dispose frame)
                               (println "\nExited direct key mode"))
                             (execute-command key-char board-shim))))
                       (keyReleased [e])
                       (keyTyped [e]))]

    (.setSize frame 0 0)
    (.setVisible frame true)
    (.addKeyListener frame key-listener)
    (.setFocusable frame true)
    (.requestFocus frame)

    (while (.isDisplayable frame)
      (Thread/sleep 100))))

(defn customize-keybinding!
  "Allow user to customize a keybinding"
  []
  (let [active-profile (:name (:get-active-profile @state/state))
        current-bindings (kb/get-key-bindings)]
    (println "\nCurrent keybindings for profile" active-profile ":")
    (doseq [[k cmd] (sort-by first current-bindings)]
      (println (str k " → " cmd " (" (kb/get-command-description cmd) ")")))
    (println "\nEnter command to rebind (e.g., start-recording):")
    (let [cmd-name (keyword (read-line))]
      (if (contains? kb/default-commands cmd-name)
        (do
          (println "Enter key to bind to" cmd-name ":")
          (let [key (first (read-line))
                updated-bindings (assoc current-bindings key cmd-name)]
            (kb/save-profile-keymap! updated-bindings)
            (kb/load-profile-keymap!)
            (println "Keybinding updated:" key "→" cmd-name)))
        (println "Unknown command:" cmd-name)))))

(defn initialize-record! []
  (state/register-fn! :start-recording! start-recording!)
  (state/register-fn! :stop-recording!  stop-recording!)
  (state/register-fn! :tag!             tag!))