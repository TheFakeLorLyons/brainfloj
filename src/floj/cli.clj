(ns floj.cli
  (:require [floj.bluetooth :as bluetooth]
            [floj.api :as api]
            [floj.io :as fio]
            [floj.keybindings :as kb]
            [floj.lor :as lor]
            [floj.profiles :as profiles]
            [floj.record :as record]
            [floj.state :as state]
            [mount.core :as mount])
  (:gen-class))

(defn initialize-modules! []
  (api/initialize-brainflow!)
  (fio/initialize-io!)
  (lor/initialize-lor!)
  (kb/initialize-keybindings!)
  (profiles/initialize-profiles!)
  #_(bluetooth/initialize-bluetooth-module!)
  (record/initialize-record!))

(defn initialize-application! [args]
  (fio/application-setup!)           ;creates/loads configurations and profiles
  (initialize-modules!)              ;intializes the application state
  (api/determine-board args))        ;get boardType

(defn cli-program [board-shim]
  (let [active-profile ((:get-active-profile @state/state))]
    (println "You are logged in as:" (:name active-profile))
    (kb/load-profile-keymap!)
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