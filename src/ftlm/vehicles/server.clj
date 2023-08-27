(ns ftlm.vehicles.server
  (:require
   [integrant.core :as ig]

   [ring.adapter.jetty :as jetty]
   ;; [ring.util.response :as resp]
   [ftlm.vehicles.html :refer [page-resp]]

   [shadow.graft :as graft]
   [shadow.css :refer [css]]


   [muuntaja.core :as m]
   [ring.middleware.gzip :refer [wrap-gzip]]
   [ring.middleware.defaults :as ring-defaults]
   [reitit.coercion.malli :as coercion-malli]

   [reitit.ring :as ring]
   [reitit.coercion.spec]
   [reitit.ring.middleware.defaults]
   [reitit.dev.pretty :as pretty]))

(def graft (graft/start pr-str))

(defn intro-page [req]
  (page-resp
   [:canvas {:class (css :w-2of3 :h-2of3) :id "main"}]))

(defmethod ig/init-key :router/routes [_ _]
  [["/" {:get {:handler intro-page}}]])

(defmethod ig/init-key :handler/handler [_ {:keys [routes]}]
  (ring/ring-handler
   (ring/router
    routes
    {:exception pretty/exception
     :data
     {:coercion reitit.coercion.malli/coercion
      :muuntaja m/instance
      :defaults
      (-> ring-defaults/site-defaults
          (assoc :exception pretty/exception))
      :middleware
      (concat
       [{:wrap wrap-gzip}]
       reitit.ring.middleware.defaults/defaults-middleware)}})
   (ring/routes
    (ring/create-resource-handler {:path "/"})
    (ring/create-default-handler))))

(defmethod ig/init-key :adapter/jetty [_ {:keys [handler] :as opts}]
  (jetty/run-jetty handler (-> opts (dissoc :handler) (assoc :join? false))))

(defmethod ig/halt-key! :adapter/jetty [_ server]
  (.stop server))

(ftlm.vehicles.system/restart)
