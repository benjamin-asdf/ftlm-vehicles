(ns ftlm.vehicles.server
  (:require
   [integrant.core :as ig]

   [ring.adapter.jetty :as jetty]
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
   [reitit.dev.pretty :as pretty]

   [ftlm.vehicles.art.controls :as art-controls]))

(def graft (graft/start pr-str))

(defn art [req]
  (let [piece (-> req :path-params :piece)
        version (-> req :path-params :version)]
    (page-resp
     [:div
      [:div {:id "main"}]
      (graft "art" :prev-sibling {:piece piece :version version})])))

(defn art-gallery [req]
  (let [piece (-> req :path-params :piece)
        page (or (-> req :parameters :query :page) 0)]
    (page-resp
     (let
         [all-pages (sort-by read-string compare (keys (get art-controls/versions piece)))
          pages (into [] (partition-all 3) all-pages)
          page (max 0 (min page (dec (count pages))))
          versions (nth pages page)]
         [:div
          [:div
           (map
            (fn [version]
              [:div {:class (css :m-16)}
               [:div {:id (str piece "-" version)}]
               (graft "art" :prev-sibling {:piece piece :version version})
               [:h3 [:a {:href (str "/art/p/" piece "/" version)}
                     [:strong "brownians #" version]]]])
            versions)]
          (let [$btn
                (css
                  :cursor-pointer
                  :text-center
                  :bg-gray-200
                  :p-4
                  :mx-2
                  :my-8
                  {:text-decoration "none"})
                $btn-allowed (css :bg-orange-200 :rounded)
                $btn-not-allowed (css :bg-gray-200 :cursor-not-allowed)]
            [:div
             {:class
              (css
                :flex :justify-center :w-full :mb-4
                :items-center
                :text-center)}
             (let [enabled? (not (zero? page))]
               [:div {:class (str $btn " " (if enabled? $btn-allowed $btn-not-allowed))}
                (when enabled?
                  [:a {:href (str "/art/g/" piece "?page=" (dec page))} "previous page"])])
             [:strong (str (inc page) "/" (count pages))]
             (let [enabled? (not (= page (dec (count pages))))]
               [:div
                {:class
                 (str $btn " " (if enabled? $btn-allowed $btn-not-allowed))}
                (when enabled?
                  [:a
                   {:href (str "/art/g/" piece "?page=" (inc page))} "next page"])])])]))))

;; seed wold be cool

(defmethod ig/init-key :router/routes [_ _]
  [["/" {:get {:handler (fn [_] (page-resp [:div "hi"]))}}]
   ["/art/g/:piece" {:get {:handler art-gallery :parameters
                           {:query [:map
                                    [:page :int]]}}}]
   ["/art/p/:piece/:version" {:get {:handler art}}]])

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

(comment

  (reitit.core/match-by-path
   (let [routes [["/" {:get {:handler (fn [_] (page-resp [:div "hi"]))}}]
   ["/art/g/:piece" {:get {:handler art-gallery}}]
   ["/art/p/:piece/:version" {:get {:handler art}}]]]
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
         reitit.ring.middleware.defaults/defaults-middleware)}}))
   "/art/g/brownians")
  ;; http://localhost:8095/art
  )
