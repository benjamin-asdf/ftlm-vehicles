(ns ftlm.vehicles.art.user-controls
  (:require
   [ftlm.vehicles.art.controls :as controls]
   [leva.core :as leva]
   [reagent.core :as r]
   [reagent.dom :as rdom]))

(defonce !app (r/atom nil))
(def restart-fn (atom nil))

(defn ui [{:keys [version piece]}]
  (when @!app
    [:<>
     [leva/Controls
      {:folder {:name (str piece " #" version)}
       :atom !app}]
     [leva/Controls
      {:schema
       {:restart
        (leva/button (fn [] (some-> @restart-fn (apply nil))))}}]]))

(defn setup! [c]
  (reset! !app (into (sorted-map) c)))

(defn view [{:keys [version piece] :as opts} place]
  (setup!
   (merge
    (controls/default-versions piece)
    (get-in controls/versions [piece version])))
  (rdom/render [ui opts] place))
