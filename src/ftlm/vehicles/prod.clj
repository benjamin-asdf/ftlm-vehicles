(ns ftlm.vehicles.prod
  (:gen-class)
  (:require
   [ftlm.vehicles.server]
   [ftlm.vehicles.system]))

(defn -main [& _]
  (ftlm.vehicles.system/start!))

(comment
  (ftlm.vehicles.system/restart)
  ;; http://localhost:8095/art/g/brownians

  )
