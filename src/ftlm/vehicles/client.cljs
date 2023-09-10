(ns ftlm.vehicles.client
  (:require
   [shadow.graft :as graft]
   [shadow.cljs.modern :refer (js-await)]
   [cljs.reader :as reader]
   [ftlm.vehicles.art :as art]

   [ftlm.vehicles.art.gaus-circles]))

(defn req [href opts]
  (js-await [res (js/fetch href (clj->js opts))]
            (.text res)))

(defmethod graft/scion "art" [opts place]
  (art/view (assoc opts :place place)))

(defn init []
  (graft/init reader/read-string))
