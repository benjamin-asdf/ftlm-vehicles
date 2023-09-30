(ns ftlm.vehicles.art.user-controls
  (:require
   [ftlm.vehicles.art.controls :as controls]
   [leva.core :as leva]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [clojure.walk :as walk]))

(defonce !app (r/atom nil))

(defmulti action-button identity)

(defn ui
  [{:keys [version piece more-controls]}]
  (when @!app
    [:<> [leva/Controls {:atom !app :folder {:name (str piece " #" version)}}]
     (when more-controls
       [leva/Controls
        (walk/postwalk
         (fn [e]
           (if (and (vector? e) (= :leva/button (first e)))
             (leva/button (fn [] (action-button (second e))))
             e))
         (do (def more-controls more-controls) more-controls))])]))

(defn setup! [c]
  (reset! !app (into (sorted-map) c)))

(defn view
  [{:as opts :keys [version piece]} place]
  (let [opts (assoc opts
                    :more-controls
                    (-> controls/leva-controls
                        (get piece)))]
    (setup! (merge (controls/default-versions piece)
                   (get-in controls/versions [piece version])))
    (rdom/render [ui opts] place)))
