(ns ftlm.hearts.html
  (:require
   [hiccup2.core :as h]
   [ring.util.response :as resp]
   [ring.middleware.anti-forgery :as csrf]))

(defn base [body]
  (h/html
      {:escape-strings? false}
      [:head
       [:link {:rel "preload" :as "script" :href "/js/main.js"}]
       [:link {:rel "stylesheet" :href "/css/ui.css"}]
       (when (bound? #'csrf/*anti-forgery-token*)
         [:meta {:name "x-csrf-token" :content csrf/*anti-forgery-token*}])
       [:title "ftl-hearts"]]
      [:body
       body
       [:script {:type "text/javascript" :src "/js/main.js" :defer true}]]))

(defn page-resp [body]
  (->
   (base body)
   str
   resp/response
   (resp/header "Content-Type" "text/html")))
