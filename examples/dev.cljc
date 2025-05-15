(ns dev
  (:require [hyperfiddle.electric :as e]
            #?(:clj  [user :as user])
            #?(:clj  [app.server :as server])
            #?(:clj  [shadow.cljs.devtools.server :as shadow-server])
            #?(:clj  [shadow.cljs.devtools.api :as shadow])
            #?(:clj  [clojure.tools.logging :as log])
            #?(:cljs [app.client :as client])))

#?(:clj
   (def config
      {:host "0.0.0.0"
       :port 8080
       :resources-path "public"
       :manifest-path "public/example/js/manifest.edn"}))

#?(:clj
(defn -main [& args]
  (log/info "Starting Electric server...")
  (log/info "DEV USER VERSION:" user/USER_VERSION)

  (let [user-version user/USER_VERSION
        electric-entrypoint (fn [ring-request]
                              (e/boot-server
                               {:hyperfiddle.electric/user-version user-version}
                               server/App
                               ring-request))]

    (def server
      (server/start-server!
       electric-entrypoint
       config))
    (comment (.stop server)))))

#?(:cljs
   (do
     (defn ^:dev/after-load ^:export start-client! []
       (js/console.log "Starting Electric client...")
       (client/start!))

     (defn ^:dev/before-load stop! []
       (when-let [stop-fn (resolve 'app.client/stop!)]
         (stop-fn)))))
