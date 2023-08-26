(ns ftlm.hearts.server
  (:require
   [integrant.core :as ig]

   [ring.adapter.jetty :as jetty]
   [ring.util.response :as resp]
   [ftlm.hearts.html :refer [page-resp]]
   ;; [clojure.java.io :as io]
   ;; [xtdb.api :as xt]

   [shadow.graft :as graft]
   [shadow.css :refer [css]]


   [muuntaja.core :as m]
   [ring.middleware.gzip :refer [wrap-gzip]]
   [ring.middleware.defaults :refer [api-defaults] :as ring-defaults]
   [ring.middleware.session.memory :as memory]
   [reitit.coercion.malli :as coercion-malli]

   [reitit.ring :as ring]
   [reitit.coercion.spec]
   [reitit.ring.middleware.defaults]
   [ftlm.hearts.auth.auth :as auth]
   [reitit.dev.pretty :as pretty]
   [ftlm.hearts.auth.ui :as auth-ui]))

(def session-store (memory/memory-store))

;; lub, wait, dub, wait, diastole, repeat
(def clip {:clip/timestamps [250, 50, 100, 600]})

(def graft (graft/start pr-str))

(defn clip-page [_req]
  (page-resp
   [:div.clip
    {:class (css :flex :justify-center)}
    [:div
     ;; [:svg
     ;;  {:xmlns "http://www.w3.org/2000/svg" :width "200" :height "200" :viewBox "0 0 100 100"}
     ;;  [:circle {:cx "50" :cy "50" :r "50" :fill "orange"}]]
     ;; (graft "clip" :prev-sibling clip)
     ;; [:button {:class (css :px-4 :shadow {:background-color "red"})} "lub-dub"]
     ;; (graft "clip" :prev-sibling clip)
     [:canvas {:id "main" :class (css :w2of4 :h2of5)}]
     ]]))

(defmethod ig/init-key :router/routes [_ _]
  [["/" {:get {:handler #'clip-page}}]
   ["/login" {:get auth-ui/login
              :post auth/login}]
   ["/api"
    {:defaults api-defaults
     :middleware [auth/auth-middleware]}
    ["/clip/:clip-id"
     {:get {:handler #'clip-page}
      :delete {:handler (constantly nil)}
      :post {:handler (constantly nil)}}]]
   ["/clip/:clip-id"
    {:get {:handler #'clip-page}}]])

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
          (assoc-in [:session :store] session-store)
          (assoc-in [:security :anti-forgery] true)
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

;; login
;; logout
;; clip create

;; clip delete

;; users
;; login
;; auth,

(comment
  #_{:clj-kondo/ignore [:unresolved-namespace]}
  (ftlm.hearts.system/restart))
