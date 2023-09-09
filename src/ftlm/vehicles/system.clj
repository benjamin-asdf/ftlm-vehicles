(ns ftlm.vehicles.system
  (:require
   [clojure.java.io :as io]
   [integrant.core :as ig]))

(defonce system (atom nil))

(def config
  {:adapter/jetty {:port 8095
                    :handler (ig/ref :handler/handler)}
   :handler/handler {:routes (ig/ref :router/routes)}
   :router/routes {}})

(defn start! []
  (reset! system (ig/init config))
  (println "Started server on " (-> config :adapter/jetty :port)))

(defn halt! []
  (when-let [system @system] (ig/halt! system)))

(defn restart []
  (halt!)
  (start!))
