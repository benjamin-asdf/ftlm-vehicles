(ns ftlm.vehicles.art.user-controls
  (:require
   [ftlm.vehicles.art.controls :as controls]
   [leva.core :as leva]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [clojure.walk :as walk]))

(defonce !app (r/atom nil))
(defonce !state (atom {}))

(defn sub-controls->controls [m]
  (-> m :atom deref))

(defn controls []
  (merge
   (into
    @!app
    (update-vals (:sub-controls @!state) sub-controls->controls))))

(defn ->sub-control [m]
  {:atom (r/atom m)})

(defmulti action-button (fn [e & _args] e))

(def leva-controls
  {"fear_and_aggression"
     {:schema
        {:restart
           (leva/button
             (fn []
               (action-button
                 :ftlm.vehicles.art.vehicles.fear-and-aggression/restart)))}
      :sub-schemas
      (let [->spawn-btn
            (fn [k]
              {:spawn!
               (leva/button
                (fn []
                  (action-button
                   :ftlm.vehicles.art.vehicles.fear-and-aggression/spawn
                   k)))})]
        {:love
         {:schema (->spawn-btn :love)}
         :fear
         {:schema (->spawn-btn :fear)}
         :aggression
         {:schema (->spawn-btn :aggression)}
         :explore
         {:schema (->spawn-btn :explore)}})}
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
  (fn []
    @!app
    [:<> [leva/Controls {:atom !app :folder {:name (str piece " #" version)}}]
     (for [[k v] (:sub-controls @!state)]
       ^{:key k}
       [leva/Controls
        {:schema
         (-> more-controls :sub-schemas k :schema)
         :atom (:atom v) :folder {:name (name k)
                                  :settings
                                  {:collapsed true}}}])
     (when more-controls [leva/Controls more-controls])]))

(defn setup!
  [c]
  (let [sub-control? (:sub-controls c)
        sub-controls (into {} (filter (comp sub-control? key)) c)
        sub-controls (update-vals sub-controls ->sub-control)
        _ (reset! !state {:sub-controls sub-controls})
        c (into (sorted-map)
                (remove (comp (conj sub-control? :sub-controls) key))
                c)]
    (reset! !app c)))

(defn view
  [{:as opts :keys [version piece]} place]
  (let [opts (assoc opts
                    :more-controls
                    (-> leva-controls (get piece)))]
    (setup! (merge (controls/default-versions piece)
                   (get-in controls/versions [piece version])))
    (rdom/render [ui opts] place)))
