(ns floj.cli
  (:require [floj.api :as api]
            #_[floj.bluetooth :as bluetooth] ;coming soon!
            [floj.cursor :as cursor]
            [floj.io :as fio]
            [floj.keybindings :as kb]
            [floj.lor :as lor]
            [floj.profiles :as profiles]
            [floj.record :as record]
            [floj.state :as state]
            [floj.wave-refraction :as refraction]
            [mount.core :as mount])
  (:gen-class))

(System/setProperty "jna.debug_load" "false")
(System/setProperty "jna.debug_load.jna" "false")
(System/setProperty "jna.nosys" "true") 
(state/redirect-system-output!)

(defn initialize-modules! []
  (api/initialize-brainflow!)
  (fio/initialize-io!)
  (lor/initialize-lor!)
  (kb/initialize-keybindings!)
  (profiles/initialize-profiles!)
  #_(bluetooth/initialize-bluetooth-module!) ;see above
  (record/initialize-record!)
  (refraction/initialize-calibration!)
  (cursor/initialize-cursor!))

(defn check-and-load-calibration! [profile]
  (when profile
    (let [profile-name (:name profile)]
      (println "Checking calibration for profile:" profile-name)
      (if ((:check-calibration @state/state) profile-name)
        (do
          ((:load-golden-tensor @state/state) profile-name)
          (println "Calibration data loaded successfully."))
        (println "No calibration data found. Use 'c' to run calibration.")))))

(defn initialize-application! [args]
  (fio/application-setup!)      ;creates/loads configurations and profiles
  (initialize-modules!))        ;get boardType

(defn cli-program [board-shim]
  (let [active-profile ((:get-active-profile @state/state))]
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

(defn -main [& args]
  (initialize-application! args)
  (let [_ (mount/start)]
    (cli-program state/shim)
    (mount/stop)))