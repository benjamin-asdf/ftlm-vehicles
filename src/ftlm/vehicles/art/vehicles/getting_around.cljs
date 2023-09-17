(ns ftlm.vehicles.art.vehicles.getting-around
  (:require
   [ftlm.vehicles.art.lib :as lib]
   [ftlm.vehicles.art :as art]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [ftlm.vehicles.art.controls :refer [versions]]
   [ftlm.vehicles.art.user-controls :as user-controls]))

(def default-controls {})

(defn draw-state [state]
  (q/background 230)
  (q/stroke-weight 1)
  (q/stroke 0.3)
  (doseq [{:keys [color hidden?] :as entity} (:entities state)]
    (when-not hidden?
      (lib/draw-color color)
      (lib/draw-entity entity))))

(defn setup [controls]
  {})

(defn update-state [state] state)

(defn sketch [host controls]
  (q/sketch
   :host host
   :size [600 600]
   :setup (partial setup controls)
   :update update-state
   :draw draw-state
   :features [:keep-on-top]
   :middleware [m/fun-mode]))

(defmethod art/view "getting-around"
  [{:keys [place version]}]
  (sketch
   place
   (merge
    default-controls
    (get-in versions ["getting-around" version]))))
