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

(defn type [obj]
  (-> obj meta :type))

(defn union [& types] {:type (set types)})

(def alltypes (atom {}))

(defn def-a-type [typename type]
  (swap! alltypes assoc typename type))

(def-a-type :int :int)

(defn lookup-type [typename]
  (get @alltypes typename))


(require '[clojure.string :as str])


(defn replace-special-chars [text]
  (-> text
      (str/replace #"…" "...")
      (str/replace #"“" "\"")
      (str/replace #"”" "\"")
      (str/replace #"‟" "\"")))

(update-vals (fn [v] (if (string? v) (replace-special-chars v) v)) obj)
