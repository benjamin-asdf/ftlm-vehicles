(ns ftlm.vehicles.art.vehicles.getting-around
  (:require

   [ftlm.vehicles.art.lib :as lib]
   [ftlm.vehicles.art :as art]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [ftlm.vehicles.art.controls :refer [versions]]
   [ftlm.vehicles.art.user-controls :as user-controls]))

(def default-controls {})

(def ^:dynamic *dt* nil)

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
  (merge (lib/->entity :rect)
         {:acceleration 0
          :angular-acceleration 0
          :angular-vlocity 0
          :cart? true
          :color 100
          :mass 1
          :motors {:left {:force 0 :pos :left} :right {:force 0 :pos :right}}
          :rotation 0
          :transform (lib/->transform [200 200] 40 80 1)
          :velocity 0}))

(defn zero-force [e] (assoc e :force 0))

(defn update-force [cart]
  (let [mass (:mass cart)
        motors (:motors cart)
        total-force (reduce + (map :force (vals motors)))]
    (-> cart
        (assoc :acceleration (/ total-force mass)
               :velocity (+ (:velocity cart) (* (:acceleration cart) *dt*)))
        (update :motors update-vals zero-force))))

(defmulti ->moment-of-inertia :kind)
(defmethod ->moment-of-inertia :rect
  [{:keys [transform mass]}]
  (let [{:keys [width height]} transform]
    (* mass (+ (* width width) (* height height)) 12)))

(defn update-angular-force [cart]
  (let [moment-of-inertia (->moment-of-inertia cart)
        force (:force cart)]
    (assoc cart
           :angular-acceleration (/ force moment-of-inertia)
           :angular-velocity (+ (:angular-velocity cart) (* (:angular-acceleration cart) *dt*)))))

(defn update-position
  [entity]
  (let [velocity (:velocity entity)
        rotation (:rotation entity)
        x (* *dt* velocity (Math/cos rotation))
        y (* *dt* velocity (Math/sin rotation))]
    (-> entity
        (update-in [:transform :pos]
                   (fn [position]
                     (vector (+ (first position) x)
                             (+ (second position) y)))))))

(defn update-entity [entity]
  (-> entity
      update-force
      update-angular-force
      update-position))

(defn update-state
  [state]
  (let [current-tick (q/millis)
        dt (/ (- current-tick (:last-tick state)) 1000.0)]
    (binding [*dt* dt]
      (-> state
          (assoc :last-tick current-tick)
          (update :entities (fn [ents] (doall (map update-entity ents))))))))

(defn window-dimensions []
  (let [w (.-innerWidth js/window)
        h (.-innerHeight js/window)]
    {:width w :height h}))

(defn setup
  [controls]
  (q/rect-mode :center)
  {:entities [(->cart)] :last-tick (q/millis)})

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

(comment
  ()

  )
