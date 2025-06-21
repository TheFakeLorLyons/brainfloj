(ns floj.cli
  "This is the entry point to the BrainFloj CLI and initializes the shared state between
   BrainFloj modules and applications built on top of it."
  (:require [floj.api :as api]
            #_[floj.bluetooth :as bluetooth] ;coming soon!
            [floj.calibration :as calibrate]
            #_[floj.cursor :as cursor]
            [floj.io :as fio]
            [floj.keybindings :as kb]
            [floj.wave-lexicon :as lexi]
            [floj.log :as log]
            [floj.lor :as lor]
            [floj.profiles :as profiles]
            [floj.record :as record]
            [floj.state :as state]
            [floj.wave-refraction :as refraction]
            [mount.core :as mount])
  (:gen-class))

(defn initialize-modules! []
  (api/initialize-brainflow!)
  (fio/initialize-io!)
  (lor/initialize-lor!)
  (kb/initialize-keybindings!)
  (profiles/initialize-profiles!)
  #_(bluetooth/initialize-bluetooth-module!) ;see above
  (lexi/initialize-lexicon!)
  (record/initialize-record!)
  (calibrate/initialize-calibration!)
  (refraction/initialize-baseline!)
  #_(cursor/initialize-cursor!))

(defn check-and-load-calibration! [profile]
  (when profile
    (let [profile-name (:name profile)]
      (println "Checking calibration for profile:" profile-name)
      (if ((:check-calibration @state/state) profile-name)
        (do
          ((:load-golden-tensor @state/state) profile-name)
          (println "Calibration data loaded successfully."))
        (println "No calibration data found. Use 'c' to run calibration.")))))

(defn initialize-application! []
  (fio/application-setup!)
  (log/redirect-system-output!)
  
  (profiles/create-default-profile!)
  (initialize-modules!))

(defn cli-program [board-shim]
  (let [active-profile ((:get-active-profile @state/state))]
    (lexi/fill-initial-lexicon! (:name active-profile))
    (println "You are logged in as:" (:name active-profile))
    (kb/load-profile-keymap!)

    (check-and-load-calibration! active-profile)
    (api/connect-to-default-device active-profile))

  (kb/display-help)

  (loop []
    (print "\nCommand: ")
    (flush)
    (let [input (read-line)]
      (cond
        (empty? input)
        (recur)

        :else
        (do
          (record/execute-command (first input) board-shim)
          (recur))))))

(defn -main []

  (initialize-application!)
  (let [_ (mount/start)]
    (cli-program state/shim)
    (mount/stop)))