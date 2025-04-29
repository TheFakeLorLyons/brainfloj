(ns floj.cli
  (:require [floj.brainflow :as brainflow]
            [floj.io :as fio]
            [floj.keybindings :as kb]
            [floj.lor :as lor]
            [floj.profiles :as profiles]
            [floj.record :as record]
            [floj.state :as state]
            [mount.core :as mount])
  (:gen-class))

(defn initialize-modules! []
  (brainflow/initialize-brainflow!)
  (fio/initialize-io!)
  (lor/initialize-lor!)
  (kb/initialize-keybindings!)
  (profiles/initialize-profiles!)
  (record/initialize-record!))

(defn initialize-application! [args]
  (fio/application-setup!)           ;creates/loads configurations and profiles
  (initialize-modules!)              ;intializes the application state
  (brainflow/determine-board args))  ;get boardType

(defn cli-program [board-shim]
  (let [active-profile ((:get-active-profile @state/state))]
    (println "You are logged in as:" (:name active-profile))
    (kb/load-profile-keymap!))

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