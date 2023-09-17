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

(defn ->cart
  []
  (merge
   (lib/->entity :rect)
   {:color 100 :transform (lib/->transform [200 200] 40 80 1)
    :cart? true}))

(defn setup
  [controls]
  (q/rect-mode :center)
  {:entities [(->cart)] :last-tick (q/millis)})

(defn update-entity [entity dt]
  (-> entity
      (update-in
       [:transform :rotation] (fnil (fn [r] (+ r (* 1 dt))) 0))))

(defn update-state
  [state]
  (let [current-tick (q/millis)
        dt (/ (- current-tick (:last-tick state)) 1000)]
    (-> state
        (assoc :last-tick current-tick)
        (update :entities #(map (fn [e] (update-entity e dt)) %)))))


(defn window-dimensions []
  (let [w (.-innerWidth js/window)
        h (.-innerHeight js/window)]
    {:width w :height h}))

(defn sketch
  [host controls]
  (let [{:keys [width height]} (window-dimensions)]
    (q/sketch :host host
              :size [width height]
              :setup (partial setup controls)
              :update update-state
              :draw draw-state
              :features [:keep-on-top]
              :middleware [m/fun-mode])))

(defmethod art/view "getting-around"
  [{:keys [place version]}]
  (sketch
   place
   (merge
    default-controls
    (get-in versions ["getting-around" version]))))
