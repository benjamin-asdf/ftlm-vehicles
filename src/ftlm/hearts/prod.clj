(ns ftlm.hearts.prod
  (:gen-class)
  (:require

   [ftlm.hearts.server]
   [ftlm.hearts.db]

   [ftlm.hearts.system]))

(defn -main [& _]
  (ftlm.hearts.system/start!))

(comment
  (ftlm.hearts.system/restart)
  ;; http://localhost:8093
  ;; http://localhost:8093/clip/foo

  )
