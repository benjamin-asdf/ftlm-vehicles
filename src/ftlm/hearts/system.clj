(ns ftlm.hearts.system
  (:require
   [clojure.java.io :as io]
   [integrant.core :as ig]))

(defonce system (atom nil))

(def config
  {:adapter/jetty {:port 8093
                   :handler (ig/ref :handler/handler)}
   :handler/handler {:routes (ig/ref :router/routes)}
   :router/routes {}
   :xtdb/node
   (let [kv-store (fn [dir]
                    {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store
                                :db-dir (io/file dir)
                                :sync? true}})]
     {:xtdb/tx-log (kv-store "data/dev/tx-log")
      :xtdb/document-store (kv-store "data/dev/doc-store")
      :xtdb/index-store (kv-store "data/dev/index-store")})})

(defn start! []
  (reset! system (ig/init config))
  (println "Started server on " (-> config :adapter/jetty :port)))

(defn halt! []
  (when-let [system @system] (ig/halt! system)))

(defn restart []
  (halt!)
  (start!))
