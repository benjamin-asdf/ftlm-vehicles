(ns ftlm.vehicles.prod
  (:gen-class)
  (:require
   [ftlm.vehicles.server]
   [ftlm.vehicles.system]))

(defn -main [& _]
  (ftlm.vehicles.system/start!))

(comment
  (ftlm.vehicles.system/restart)
  ;; http://localhost:8095/art/g/brownians?page=0
  ;; http://localhost:8095/art/p/brownians/0?controls=true
  ;; http://localhost:8095/art/p/getting-around/0



  )
