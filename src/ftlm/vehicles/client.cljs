(ns ftlm.vehicles.client
  (:require
   [shadow.graft :as graft]
   [shadow.cljs.modern :refer (js-await)]
   [cljs.reader :as reader]
   [ftlm.vehicles.art :as art]

   [ftlm.vehicles.art.gaus-circles]
   ftlm.vehicles.art.vehicles.getting-around
   ftlm.vehicles.art.vehicles.fear-and-aggression
   [ftlm.vehicles.art.user-controls :as user-controls]))

;; (defn req [href opts]
;;   (js-await [res (js/fetch href (clj->js opts))]
;;             (.text res)))

(defmethod graft/scion "art" [opts place]
  (art/view (assoc opts :place place)))

(defmethod graft/scion "controls-app" [opts place]
  (user-controls/view opts place))

(defn init []
  (graft/init reader/read-string))
