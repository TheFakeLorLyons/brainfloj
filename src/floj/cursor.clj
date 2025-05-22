(ns floj.cursor
  (:require [clojure.core.async :as async :refer [go go-loop <! timeout]]
            [clojure.edn :as edn]
            [clojure.math :as math]
            [clojure.java.io :as io]
            [floj.brainflow.board-shim :as brainflow]
            [floj.io :as fio]
            [floj.state :as state]
            [floj.record :as record])
  (:import [java.awt Robot Color Dimension]
           [java.awt.event KeyEvent InputEvent ActionListener]
           [javax.swing JFrame JPanel JLabel JButton BoxLayout KeyStroke JComponent JOptionPane SwingUtilities]
           [org.jtransforms.fft DoubleFFT_1D]))

(defonce robot (delay (Robot.)))
(defonce cursor-control-running? (atom false))
(defonce cursor-control-loop (atom nil))
(defonce calibration-data (atom {:baseline {:alpha 0 :beta 0}
                                 :samples []
                                 :scale {:x 10 :y 10}
                                 :threshold 0.2
                                 :calibrated false}))

(defonce movement-history (atom (repeat 5 [0 0])))

(defn smooth-movement
  "Apply exponential moving average to cursor movements"
  [raw-delta-x raw-delta-y]
  (swap! movement-history (fn [hist]
                            (conj (vec (drop 1 hist)) [raw-delta-x raw-delta-y])))
  (let [weights (map #(Math/pow 0.7 %) (range (count @movement-history)))
        weighted-x (reduce + (map #(* %1 (first %2)) weights @movement-history))
        weighted-y (reduce + (map #(* %1 (second %2)) weights @movement-history))
        weight-sum (reduce + weights)]
    [(/ weighted-x weight-sum) (/ weighted-y weight-sum)]))

(defn extract-alpha-power
  "Extract alpha wave power from EEG data for the given channel"
  [data channel sampling-rate]
  (try
    (let [channel-data (cond
                         (and (> (count data) channel)
                              (vector? (nth data channel))
                              (every? number? (nth data channel)))
                         (nth data channel)

                         (and (vector? data)
                              (every? vector? data)
                              (> (count (first data)) channel))
                         (map #(nth % channel nil) data)

                         :else
                         (do (println "Warning: Channel" channel "not available or invalid format")
                             []))

          ; Filter out non-numeric values
          numeric-channel-data (if (sequential? channel-data)
                                 (filter number? channel-data)
                                 [])

          ; Only proceed if we have valid numeric data
          fft-data (when (and (seq numeric-channel-data)
                              (every? number? numeric-channel-data))
                     (let [transformer (DoubleFFT_1D. (count numeric-channel-data))
                           data-array (double-array numeric-channel-data)]
                       (.realForward transformer data-array)
                       data-array))

          freq-bin-size (if (and (seq numeric-channel-data) (pos? sampling-rate))
                          (/ sampling-rate (* 2 (count numeric-channel-data)))
                          1.0) ; Default if no valid data

          alpha-min-bin (int (/ 8 freq-bin-size))
          alpha-max-bin (int (/ 13 freq-bin-size))

          alpha-power (if (and fft-data
                               (>= (alength fft-data) alpha-max-bin))
                        (reduce + (map #(Math/pow (aget fft-data %) 2)
                                       (range alpha-min-bin alpha-max-bin)))
                        0.0)]
      (if (> alpha-power 0) ; Normalized alpha power
        (Math/log10 alpha-power)
        0.1)) ; Small non-zero default
    (catch Exception e
      (println "Error extracting alpha power:" (.getMessage e))
      (.printStackTrace e)
      0.1))) ; Default value on error

(defn extract-beta-power
  "Extract beta wave power from EEG data for the given channel"
  [data channel sampling-rate]
  (try
    (let [channel-data
          (cond
            (and (> (count data) channel)
                 (vector? (nth data channel))
                 (every? number? (nth data channel)))
            (nth data channel)

            (and (vector? data)
                 (every? vector? data)
                 (> (count (first data)) channel))
            (map #(nth % channel nil) data)

            :else
            (do (println "Warning: Channel" channel "not available or invalid format")
                []))

          ; Filter out non-numeric values
          numeric-channel-data (if (sequential? channel-data)
                                 (filter number? channel-data)
                                 [])

          ; Only proceed if we have valid numeric data
          fft-data (when (and (seq numeric-channel-data)
                              (every? number? numeric-channel-data))
                     (let [transformer (DoubleFFT_1D. (count numeric-channel-data))
                           data-array (double-array numeric-channel-data)]
                       (.realForward transformer data-array)
                       data-array))

          freq-bin-size (if (and (seq numeric-channel-data) (pos? sampling-rate))
                          (/ sampling-rate (* 2 (count numeric-channel-data)))
                          1.0) ; Default if no valid data

          beta-min-bin (int (/ 13 freq-bin-size))
          beta-max-bin (int (/ 30 freq-bin-size))

          beta-power (if (and fft-data
                              (>= (alength fft-data) beta-max-bin))
                       (reduce + (map #(Math/pow (aget fft-data %) 2)
                                      (range beta-min-bin beta-max-bin)))
                       0.0)]
      (if (> beta-power 0) ; Normalized beta power
        (Math/log10 beta-power)
        0.1)) ; Small non-zero default
    (catch Exception e
      (println "Error extracting beta power:" (.getMessage e))
      (.printStackTrace e)
      0.1))) ; " "

(defn calibrate!
  "Calibrate the cursor control system based on EEG data"
  [board-shim duration-seconds]
  (println "Starting calibration. Please relax for" duration-seconds "seconds...")
  (reset! calibration-data {:baseline {:alpha 0.0 :beta 0.0}
                            :samples []
                            :scale {:x 10 :y 10}
                            :threshold 0.2
                            :calibrated false})
  (let [calibration-done (promise)]
    (future
      (try
        (println "Collecting EEG data for calibration...")
        (when-not @state/recording?
          (println "Board not streaming. Starting stream...")
          (try
            (record/start-recording!)
            (Thread/sleep 1000)
            (catch Exception e
              (println "Error starting stream:" (.getMessage e))
              (.printStackTrace e))))

        (let [start-time (System/currentTimeMillis)
              end-time (+ start-time (* duration-seconds 1000))
              board-id (when board-shim
                         (try
                           (brainflow/get-board-id board-shim)
                           (catch Exception e
                             (println "Error getting board ID:" (.getMessage e))
                             nil)))
              _ (println "Board ID type: " (type board-id))
              sampling-rate (try
                              (if board-id
                                (brainflow/get-sampling-rate board-id)
                                250) ; Default if board ID not available
                              (catch Exception e
                                (println "Error getting sampling rate, using default 250:" (.getMessage e))
                                250))
              x-channel 0  ; Use fixed channels for calibration
              y-channel 1]

          (println "Using profile:" (-> @state/state :get-active-profile .invoke :name))

          (while (< (System/currentTimeMillis) end-time) ; Collection loop
            (Thread/sleep 100)
            (println "Collecting sample at time:" (- (System/currentTimeMillis) start-time) "ms")
            (when-let [data (try
                              ; Get limited number of samples, just enough for calibration
                              (let [data-result (brainflow/get-current-board-data board-shim (int (/ sampling-rate 10)))]
                                ; Make sure we only use the first few channels to avoid the dimension errors
                                (if (and (sequential? data-result) (> (count data-result) 4))
                                  (subvec data-result 0 4) ; Limit to 4 channels
                                  data-result))
                              (catch Exception e
                                (println "Error getting board data:" (.getMessage e))
                                nil))]
              (when (and (seq data) (> (count data) 0))
                (try
                  (let [alpha (extract-alpha-power data x-channel sampling-rate)
                        beta (extract-beta-power data y-channel sampling-rate)]
                    (println "Sample alpha:" alpha "beta:" beta)
                    (swap! calibration-data update :samples conj {:alpha alpha :beta beta}))
                  (catch Exception e
                    (println "Error processing calibration sample:" (.getMessage e)))))))

          ; Ensure at least some samples exist before calculating baseline
          (if (seq (:samples @calibration-data))
            (do
              (println "Calculating baseline values...")
              (let [samples (:samples @calibration-data)
                    alpha-values (filter number? (map :alpha samples))
                    beta-values (filter number? (map :beta samples))
                    alpha-baseline (if (seq alpha-values) (/ (reduce + alpha-values) (count alpha-values)) 0.5)
                    beta-baseline (if (seq beta-values) (/ (reduce + beta-values) (count beta-values)) 0.5)]

                (println "Collected" (count samples) "samples")
                (println "Alpha baseline:" alpha-baseline)
                (println "Beta baseline:" beta-baseline)

                ; Store baseline values
                (swap! calibration-data assoc :baseline {:alpha alpha-baseline :beta beta-baseline})
                (swap! calibration-data assoc :calibrated true)

                ; Mark calibration as complete
                (deliver calibration-done true)))
            (do
              (println "No samples collected during calibration") ; No samples collected, mark as failure
              (swap! calibration-data assoc :calibrated false)
              (deliver calibration-done false))))
        (catch Exception e
          (println "Calibration error:" (.getMessage e))
          (.printStackTrace e)
          (swap! calibration-data assoc :calibrated false)
          (deliver calibration-done false)))
      (println "Calibration process completed"))
    calibration-done)) ; return promise

(defn save-calibration-to-profile!
  "Save current calibration data to the active profile"
  [profile-name]
  (let [profile-path (str (fio/config-base-dir) "/profiles/" profile-name ".edn")
        current-profile (edn/read-string (slurp profile-path))
        updated-profile (assoc current-profile :cursor-calibration @calibration-data)]
    (spit profile-path (pr-str updated-profile))))

(defn load-calibration-from-profile!
  "Load calibration data from the active profile"
  [profile-name]
  (let [profile-path (str (fio/config-base-dir) "/profiles/" profile-name ".edn")]
    (when (.exists (io/file profile-path))
      (let [profile (edn/read-string (slurp profile-path))]
        (when-let [saved-calibration (:cursor-calibration profile)]
          (reset! calibration-data saved-calibration)
          true)))))

(defn save-calibration-to-active-profile! []
  (when-let [active-profile ((:get-active-profile @state/state))]
    (let [profile-name (:name active-profile)]
      (save-calibration-to-profile! profile-name)
      (println "Calibration saved to profile:" profile-name))))

(defn load-calibration-from-active-profile! []
  (when-let [active-profile ((:get-active-profile @state/state))]
    (let [profile-name (:name active-profile)]
      (if (load-calibration-from-profile! profile-name)
        (println "Calibration loaded from profile:" profile-name)
        (println "No calibration data found for profile:" profile-name)))))

(defn adaptive-calibration-update!
  "Update calibration values based on recent activity"
  [new-alpha new-beta]
  (let [current-alpha (get-in @calibration-data [:baseline :alpha])
        current-beta (get-in @calibration-data [:baseline :beta])
        ; Weighted moving average to adapt gradually
        updated-alpha (+ (* 0.95 current-alpha) (* 0.05 new-alpha))
        updated-beta (+ (* 0.95 current-beta) (* 0.05 new-beta))]
    (swap! calibration-data assoc-in [:baseline :alpha] updated-alpha)
    (swap! calibration-data assoc-in [:baseline :beta] updated-beta)))

(defn get-screen-dimensions []
  (let [screen (.getScreenSize (java.awt.Toolkit/getDefaultToolkit))]
    {:width (.getWidth screen)
     :height (.getHeight screen)}))

(defn move-cursor [x y]
  (.mouseMove @robot x y))

(defn click []
  (.mousePress @robot InputEvent/BUTTON1_DOWN_MASK)
  (Thread/sleep 50)
  (.mouseRelease @robot InputEvent/BUTTON1_DOWN_MASK))

(defn normalize-and-scale
  "Normalize the power relative to baseline and scale for cursor movement"
  [power baseline scale]
  (* (- power baseline) scale))

(defn start-cursor-control!
   "Start controlling cursor with EEG"
   [board-shim & {:keys [interval x-channel y-channel click-channel]
                  :or {interval 100
                       x-channel 0
                       y-channel 1
                       click-channel 2}}]
   (when-not @cursor-control-running?
     (reset! cursor-control-running? true)
     (let [board-id (try
                      (brainflow/get-board-id board-shim)
                      (catch Exception e
                        (println "Error getting board ID:" (.getMessage e))
                        nil))
           sampling-rate (try
                           (if board-id
                             (brainflow/get-sampling-rate board-id)
                             250) ; Default if board ID not available
                           (catch Exception e
                             (println "Error getting sampling rate, using default 250:" (.getMessage e))
                             250))
           screen (get-screen-dimensions)
           center-x (int (/ (:width screen) 2))
           center-y (int (/ (:height screen) 2))]

       (println "Starting cursor control with sampling rate:" sampling-rate)
       (println "Using channels - x:" x-channel "y:" y-channel "click:" click-channel)

       (try
         (move-cursor center-x center-y) ; Initialize cursor position
         (catch Exception e
           (println "Error setting initial cursor position:" (.getMessage e))))

       (reset! cursor-control-loop
               (go-loop [current-x center-x
                         current-y center-y]
                 (if-not @cursor-control-running?
                   nil  ; Exit the loop if not running
                   (let [next-x current-x
                         next-y current-y
                         updated-position
                         (try
                           (if-let [data (try
                                           (let [raw-data (brainflow/get-board-data board-shim)]
                                              ; Ensure we only use valid channels
                                             (if (and (sequential? raw-data)
                                                      (> (count raw-data) (max x-channel y-channel)))
                                               raw-data
                                               nil))
                                           (catch Exception e
                                             (println "Error getting board data:" (.getMessage e))
                                             nil))]
                             (if (and (seq data) (> (count data) 0))
                               (let [alpha-x (extract-alpha-power data x-channel sampling-rate)
                                     beta-y (extract-beta-power data y-channel sampling-rate)
                                     baseline-alpha (get-in @calibration-data [:baseline :alpha])
                                     baseline-beta (get-in @calibration-data [:baseline :beta])
                                     scale-x (get-in @calibration-data [:scale :x])
                                     scale-y (get-in @calibration-data [:scale :y])
                                     threshold (get-in @calibration-data [:threshold])

                                      ; Safety check for NaN or infinity
                                     delta-x (let [val (normalize-and-scale alpha-x baseline-alpha scale-x)]
                                               (if (or (Double/isNaN val) (Double/isInfinite val)) 0.0 val))
                                     delta-y (let [val (normalize-and-scale beta-y baseline-beta scale-y)]
                                               (if (or (Double/isNaN val) (Double/isInfinite val)) 0.0 val))

                                     [smoothed-x smoothed-y] (smooth-movement delta-x delta-y)

                                     new-x (if (> (math/abs smoothed-x) threshold)
                                             (int (max 0 (min (:width screen) (+ current-x smoothed-x))))
                                             current-x)
                                     new-y (if (> (math/abs smoothed-y) threshold)
                                             (int (max 0 (min (:height screen) (+ current-y smoothed-y))))
                                             current-y)]

                                 (when (or (not= new-x current-x) (not= new-y current-y))
                                   (try
                                     (move-cursor new-x new-y)
                                     (catch Exception e
                                       (println "Error moving cursor:" (.getMessage e)))))

                                 (when (and click-channel (< click-channel (count data)))
                                   (try
                                     (let [click-power (extract-beta-power data click-channel sampling-rate)
                                           click-threshold (* 2 baseline-beta)]
                                       (when (> click-power click-threshold)
                                         (click)))
                                     (catch Exception e
                                       (println "Error processing click:" (.getMessage e)))))
                                 
                                 [new-x new-y])       ; Return updated position
                               [current-x current-y]) ; No change in position
                             [current-x current-y])   ; No data available
                           (catch Exception e
                             (println "Error in cursor control loop:" (.getMessage e))
                             (.printStackTrace e)
                             [current-x current-y]))] ; Return unchanged position on error
                     (<! (timeout interval))
                     (recur (first updated-position) (second updated-position)))))))))

(defn multi-channel-control
  "Use multiple EEG channels for more robust control"
  [data eeg-channels sampling-rate]
  (let [left-channels [0 1]
        right-channels [2 3]
        left-alphas (map #(extract-alpha-power data % sampling-rate) left-channels)
        right-alphas (map #(extract-alpha-power data % sampling-rate) right-channels)
        avg-left (/ (reduce + left-alphas) (count left-alphas))
        avg-right (/ (reduce + right-alphas) (count right-alphas))
        delta-x (- avg-right avg-left)
        delta-y (- avg-left avg-right)] ; not real
    [delta-x delta-y]))

(defn stop-cursor-control! []
  (when @cursor-control-running?
    (println "Stopping cursor control...")
    (reset! cursor-control-running? false)
    (when @cursor-control-loop
      (try
        (async/close! @cursor-control-loop)
        (catch Exception e
          (println "Error closing control loop:" (.getMessage e))))
      (reset! cursor-control-loop nil))))

(defn adjust-calibration!
  "Adjust calibration settings for cursor control"
  [settings]
  (swap! calibration-data merge settings))

(defn create-calibrate-button [ui board-shim]
   (when-let [calibrate-button (:calibrate-button ui)]
     (.addActionListener
      calibrate-button
      (proxy [ActionListener] []
        (actionPerformed [e]
          (try
            (.setText (:status-label ui) "Status: Calibrating...")
            (.setEnabled calibrate-button false)
            (future ; Start calibration in separate thread
              (try
                (let [result-promise (calibrate! board-shim 5)
                      result (deref result-promise 7000 :timeout)] ; Wait for calibration to complete with timeout
                  (if (= result :timeout)
                    (do
                      (println "Calibration timed out!")
                      (SwingUtilities/invokeLater
                       (fn []
                         (.setText (:status-label ui) "Status: Calibration timed out")
                         (.setEnabled calibrate-button true))))
                    (SwingUtilities/invokeLater
                     (fn []
                       (if result
                         (do
                           (.setText (:status-label ui) "Status: Calibrated")
                           (.setText (:alpha-label ui)
                                     (str "Alpha: " (format "%.6f" (get-in @calibration-data [:baseline :alpha]))))
                           (.setText (:beta-label ui)
                                     (str "Beta: " (format "%.6f" (get-in @calibration-data [:baseline :beta]))))
                           (when-let [start-btn (:start-button ui)]
                             (.setEnabled start-btn true)))
                         (.setText (:status-label ui) "Status: Calibration failed"))
                       (.setEnabled calibrate-button true)))))
                (catch Exception ex
                  (println "Calibration thread error:" (.getMessage ex))
                  (.printStackTrace ex)
                  (SwingUtilities/invokeLater
                   (fn []
                     (.setText (:status-label ui) "Status: Calibration error")
                     (.setEnabled calibrate-button true))))))
            (catch Exception ex
              (println "Button action error:" (.getMessage ex))
              (.printStackTrace ex)
              (.setText (:status-label ui) "Status: Error")
              (.setEnabled calibrate-button true))))))))

(defn add-signal-visualization
  "Add a real-time signal visualization panel"
  [ui]
  (let [viz-panel (doto (JPanel.)
                    (.setPreferredSize (Dimension. 280 80))
                    (.setBackground Color/BLACK))
        update-viz (fn [alpha beta]
                     (let [g (.getGraphics viz-panel)
                           width (.getWidth viz-panel)
                           height (.getHeight viz-panel)]
                       (.setColor g Color/BLACK)
                       (.fillRect g 0 0 width height)

                       (.setColor g Color/GREEN)
                       (let [level (min 1.0 (/ alpha (get-in @calibration-data [:baseline :alpha] 1)))]
                         (.fillRect g 10 10 (int (* (- width 20) level)) 25))

                       (.setColor g Color/BLUE)
                       (let [level (min 1.0 (/ beta (get-in @calibration-data [:baseline :beta] 1)))]
                         (.fillRect g 10 45 (int (* (- width 20) level)) 25))))]
    (.add (:main-panel ui) viz-panel)
    update-viz))

(defn create-visualization-window
  "Create a simple window to visualize EEG activity and cursor control status"
  []
  (let [frame (JFrame. "BCI Cursor Control Test")
        main-panel (JPanel.)
        status-label (JLabel. "Status: Not Connected")
        alpha-label (JLabel. "Alpha: N/A")
        beta-label (JLabel. "Beta: N/A")
        calibrate-button (JButton. "Calibrate (5s)")
        start-button (JButton. "Start Cursor Control")
        stop-button (JButton. "Stop Cursor Control")]

    ; Layout
    (.setLayout main-panel (BoxLayout. main-panel BoxLayout/Y_AXIS))
    (.add main-panel status-label)
    (.add main-panel alpha-label)
    (.add main-panel beta-label)
    (.add main-panel calibrate-button)
    (.add main-panel start-button)
    (.add main-panel stop-button)

    ; Disable buttons initially
    (.setEnabled start-button false)
    (.setEnabled stop-button false)

    ; Configure the frame
    (.setPreferredSize frame (Dimension. 300 200))
    (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
    (.setContentPane frame main-panel)
    (.pack frame)
    (.setVisible frame true)

    ; Return a map with all the UI components
    {:frame frame
     :status-label status-label
     :alpha-label alpha-label
     :beta-label beta-label
     :calibrate-button calibrate-button
     :start-button start-button
     :stop-button stop-button}))

(defn cleanup! 
  "Clean up resources when done"
  [shim ui]
  (when shim
    (stop-cursor-control!)
    (try
      (brainflow/stop-stream! shim)
      (brainflow/release-session! shim)
      (catch Exception e
        (println "Error during cleanup:" (.getMessage e))))
    (.dispose (:frame ui))))

(defn start-test!
  "Start testing the cursor control with the currently connected board"
  [& {:keys [board-id board-shim] :or {board-id nil board-shim nil}}]
  (let [actual-shim (or board-shim @state/shim)]
    ; Check for a valid board connection
    (if (nil? actual-shim)
      (do
        (println "No board connected. Please connect a board to use the cursor feature.")
        (JOptionPane/showMessageDialog
         nil
         "No board connected. Please connect a board to use the cursor feature."
         "Board Required"
         JOptionPane/WARNING_MESSAGE)
        nil)
      (let [ui (create-visualization-window)] ; Board exists, proceed with cursor control setup
        (.setText (:status-label ui) "Status: Connected") ; Updates the UI
        (create-calibrate-button ui actual-shim)

        ; Make sure the start button exists and is a JButton before calling setEnabled
        (when-let [start-button (:start-button ui)]
          (when (instance? javax.swing.JButton start-button)
            (.setEnabled start-button false)))

        (.addActionListener  ; Start cursor control
         (:start-button ui)
         (proxy [java.awt.event.ActionListener] []
           (actionPerformed [e]
             (try
               (.setText (:status-label ui) "Status: Controlling Cursor")
               (when-let [start-btn (:start-button ui)]
                 (.setEnabled start-btn false))
               (when-let [stop-btn (:stop-button ui)]
                 (.setEnabled stop-btn true))

               (start-cursor-control! actual-shim)
               (catch Exception ex
                 (println "Start control error:" (.getMessage ex))
                 (.printStackTrace ex)
                 (.setText (:status-label ui) "Status: Start control error")
                 (when-let [start-btn (:start-button ui)]
                   (.setEnabled start-btn true))
                 (when-let [stop-btn (:stop-button ui)]
                   (.setEnabled stop-btn false)))))))

        (.addActionListener  ; Stop cursor control
         (:stop-button ui)
         (proxy [java.awt.event.ActionListener] []
           (actionPerformed [e]
             (try
               (.setText (:status-label ui) "Status: Connected")
               (when-let [start-btn (:start-button ui)]
                 (.setEnabled start-btn true))
               (when-let [stop-btn (:stop-button ui)]
                 (.setEnabled stop-btn false))

               (stop-cursor-control!)
               (catch Exception ex
                 (println "Stop control error:" (.getMessage ex))
                 (.printStackTrace ex))))))

        (future
          (try
            (while (.isVisible (:frame ui))
              (when-let [data (try
                                (brainflow/get-current-board-data actual-shim)
                                (catch Exception e nil))]
                (try
                  (let [sampling-rate (brainflow/get-sampling-rate (brainflow/get-board-id actual-shim))
                        x-channel 0
                        y-channel 1
                        alpha (extract-alpha-power data x-channel sampling-rate)
                        beta (extract-beta-power data y-channel sampling-rate)]

                    (SwingUtilities/invokeLater
                     (fn []
                       (.setText (:alpha-label ui) (str "Alpha: " (format "%.6f" alpha)))
                       (.setText (:beta-label ui) (str "Beta: " (format "%.6f" beta))))))
                  (catch Exception e
                    (println "Error updating UI:" (.getMessage e)))))
              (Thread/sleep 200))
            (catch Exception e
              (println "UI update loop error:" (.getMessage e))
              (.printStackTrace e))))

        (.. (:frame ui) (getRootPane) (registerKeyboardAction
                                       (proxy [java.awt.event.ActionListener] []
                                         (actionPerformed [e]
                                           (println "\nCursor Control Commands:")
                                           (println "C - Calibrate")
                                           (println "+ - Start control")
                                           (println "- - Stop control")
                                           (println "S - Save calibration to profile")
                                           (println "L - Load calibration from profile")
                                           (println "Q - Quit test window")))
                                       (KeyStroke/getKeyStroke KeyEvent/VK_SLASH 0)
                                       JComponent/WHEN_IN_FOCUSED_WINDOW))

        (doseq [[key-code action-fn] [[KeyEvent/VK_C #(let [p (calibrate! actual-shim 5)] nil)]
                                      [KeyEvent/VK_PLUS #(start-cursor-control! actual-shim)]
                                      [KeyEvent/VK_MINUS #(stop-cursor-control!)]
                                      [KeyEvent/VK_S #(save-calibration-to-active-profile!)]
                                      [KeyEvent/VK_L #(load-calibration-from-active-profile!)]
                                      [KeyEvent/VK_Q #(.dispose (:frame ui))]]]
          (.. (:frame ui) (getRootPane) (registerKeyboardAction
                                         (proxy [java.awt.event.ActionListener] []
                                           (actionPerformed [e]
                                             (try
                                               (action-fn)
                                               (catch Exception ex
                                                 (println "Keyboard action error:" (.getMessage ex))))))
                                         (KeyStroke/getKeyStroke key-code 0)
                                         JComponent/WHEN_IN_FOCUSED_WINDOW)))

        (.addShutdownHook (Runtime/getRuntime)
                          (Thread. #(cleanup! actual-shim ui)))

        {:board actual-shim :ui ui}))))

(defn initialize-cursor!
  "Initialize the cursor control module and register its functions in the state."
  []
  (state/register-fn! :start-cursor-test! start-test!)
  (state/register-fn! :start-cursor-control! start-cursor-control!)
  (state/register-fn! :stop-cursor-control! stop-cursor-control!)
  (state/register-fn! :calibrate-cursor! calibrate!)
  (state/register-fn! :adjust-calibration! adjust-calibration!))