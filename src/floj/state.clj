(ns floj.state
  "Using mount, state is initialized every session. Params is presented simply as an example of what
   will be necessary to connect to your device other than the device name and board-id which can be found
   in src/floj/brainflow/board_ids.clj."
  (:require [mount.core :as mount :refer [defstate]]
            [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [brainflow BoardShim BoardIds BrainFlowInputParams]))

(def params (doto (BrainFlowInputParams.)
              (.set_serial_port "COM1")
              (.set_mac_address "00:11:22:33:44:55")))

(def board-id BoardIds/SYNTHETIC_BOARD)

(def shim (atom (BoardShim. board-id params)))
(def simpleble (atom nil))
(defonce golden-tensor (atom nil))
(defonce recording-context (atom nil))

(defonce calibration-state (atom {:is-calibrating false
                                  :current-stage nil
                                  :stages-completed 0
                                  :total-stages 3}))

(defonce state (atom {:create-default-config! nil
                      :get-default-command-map! nil

                      :load-configurations! nil
                      :get-sampling-rate nil
                      :get-eeg-channels nil
                      :get-board-info nil

                      :start-recording! nil
                      :stop-recording! nil
                      :tag-fn! nil
                      :write-lor! nil

                      :create-profile! nil
                      :set-default-profile! nil
                      :switch-profile! nil
                      :get-active-profile nil
                      :delete-profile! nil

                      :add-wave-signature nil
                      :list-wave-signature-categories nil
                      :list-wave-signatures nil
                      :train-model nil

                      :digital-twin {:frames []
                                     :current-tensor nil
                                     :frame-buffer-size 256
                                     :tensor-update-counter 0
                                     :comparison-metrics {}
                                     :streaming-enabled false
                                     :last-update-time 0}

                      :release-board! nil}))

(defn register-fn! [key f]
  (swap! state assoc key f))

(defstate active-keymap
  :start (let [home (System/getProperty "user.home")
               path (str home "/.lor/command-map.edn")
               file (io/file path)]
           (when-not (.exists file)
             (println "Creating default keymap...")
             (when-let [f (:create-command-map! @state)]
               (f)))
           (atom (try
                   (edn/read-string (slurp path))
                   (catch Exception e
                     (println "Error loading keymap after creation:" (.getMessage e) "\n Creating new...")
                     (get @state :get-default-command-map!)))))
  :stop (reset! active-keymap nil))

(defstate recording?
  :start (atom false)
  :stop (when @recording?
          (when-let [f (:stop-recording! @state)]
            (f))))

(defstate eeg-data
  :start (atom [])
  :stop (reset! eeg-data []))

(defstate tags
  :start (atom [])
  :stop (reset! tags []))

(defstate current-session-name
  :start (atom "recording")
  :stop (reset! current-session-name "recording"))