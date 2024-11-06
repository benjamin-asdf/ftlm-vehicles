(ns ftlm.vehicles.html
  (:require
   [hiccup2.core :as h]
   [ring.util.response :as resp]
   [ring.middleware.anti-forgery :as csrf]))

(defn base
  [body]
  (h/html
    {:escape-strings? false}
    [:head
     [:link
      {:as "script" :href "/js/main.js" :rel "preload"}]
     [:link {:href "/css/ui.css" :rel "stylesheet"}]
     [:title "ftlm-vehicles"]]
    [:body body
     [:script
      {:defer true
       :src "/js/main.js"
       :type "text/javascript"}]]))

(defn page-resp [body]
  (->
   (base body)
   str
   resp/response
   (resp/header "Content-Type" "text/html")))

;; --------------------------------------------

(defn embed-page-resp
  [body]
  (-> (h/html
        {:escape-strings? false}
        [:head
         [:link
          {:as "script" :href "/js/main.js" :rel "preload"}]
         [:title "ftml-vehicles-embed"]]
        [:body {:style {:margin "0px"}} body
         [:script
          {:defer true
           :src "/js/main.js"
           :type "text/javascript"}]])
      str
      resp/response
      (resp/header "Content-Type" "text/html")))
