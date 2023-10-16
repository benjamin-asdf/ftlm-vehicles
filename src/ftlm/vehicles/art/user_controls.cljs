(ns ftlm.vehicles.art.user-controls
  (:require
   [ftlm.vehicles.art.controls :as controls]
   [leva.core :as leva]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [clojure.walk :as walk]))

(defonce !app (r/atom nil))

(defmulti action-button identity)

(def leva-controls
  {"fear_and_aggression"
     {:schema
      {:love! (leva/button
               (fn []
                 (action-button
                  :ftlm.vehicles.art.vehicles.fear-and-aggression/love)))
       :restart
       (leva/button
        (fn []
          (action-button
           :ftlm.vehicles.art.vehicles.fear-and-aggression/restart)))
       ;; {:schema
       ;;  (leva/)
       ;;  }
       }

      }
   "getting-around"
     {:schema {:dart! (leva/button
                        (fn []
                          (action-button
                            :ftlm.vehicles.art.vehicles.getting-around/dart!)))
               :restart
                 (leva/button
                   (fn []
                     (action-button
                       :ftlm.vehicles.art.vehicles.getting-around/restart)))}}})

(defn ui
  [{:keys [version piece more-controls]}]
  (when @!app
    [:<>
     [leva/Controls
      {:atom !app :folder {:name (str piece " #" version)}}]
     (when more-controls
       [leva/Controls
        more-controls])]))

(defn setup! [c]
  (reset! !app (into (sorted-map) c)))

(defn view
  [{:as opts :keys [version piece]} place]
  (let [opts (assoc opts
                    :more-controls
                    (-> leva-controls (get piece)))]
    (setup! (merge (controls/default-versions piece)
                   (get-in controls/versions [piece version])))
    (rdom/render [ui opts] place)))
