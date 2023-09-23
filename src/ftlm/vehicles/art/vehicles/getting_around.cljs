(ns ftlm.vehicles.art.vehicles.getting-around
  (:require
   [ftlm.vehicles.art.lib :as lib]
   [ftlm.vehicles.art :as art]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [ftlm.vehicles.art.controls :refer [versions]]
   [ftlm.vehicles.art.user-controls :as user-controls]))

(def default-controls {})

(defn print-it-every-ms [& entity]
  (q/print-every-n-millisec 200 entity)
  entity)


(def ^:dynamic *dt* nil)

;; env
;; body
;; sensors
;; effectors
;; brain (connections)


;; I try a cartoon physics,
;; where the force of a motor makes something move. When the force is gone, the movement is gone

;; velocity is a 1 frame concept, overriden by the force every frame

(defn draw-state [state]
  (q/background 230)
  (q/stroke-weight 1)
  (q/stroke 0.3)
  (doseq [{:keys [color hidden?] :as entity} (:entities state)]
    (when-not hidden?
      (lib/draw-color color)
      (lib/draw-entity entity))))

(defn friction [velocity] (* velocity 0.9))

(defn velocity-friction [cart]
  (-> cart
      (update :velocity friction)
      (update :angular-velocity friction)))

;; (defn motor-friction [cart]
;;   (update cart :motors update-vals #(update % :vigor friction)))

(defn ->motor [pos vigor]
  {:pos :middle :vigor vigor})

(defn cart-1 []
  {:motors {:middle (->motor :left 20)}})

(defn effectors [m] (-> m :motors vals))
(defn env [] {})
(def body identity)
(def sensors :sensors)
(def brain :brain)

(comment
  (motor-friction (cart-1)))

(defn ->cart []
  (merge
   (lib/->entity :rect)
   {:acceleration 0
    :angular-acceleration 0
    :angular-velocity 0 ;; 100
    :angular-force 0
    :cart? true
    :color 100
    :mass 1
    :transform (assoc (lib/->transform [200 200] 40 80 1) :rotation 0)
    :velocity 0}
   (cart-1)))

;; say motor vigor makes more velocity
;; velocity goes down with friction

(def pos->angular-velocity-sign {:left 1 :right -1 :middle 0})

(defn update-body
  [cart]
  (let [effectors (effectors cart)]
    (-> cart
        (update :velocity + (reduce + (map :vigor effectors)))
        (update :angular-velocity
                +
                (reduce +
                  (map (fn [{:keys [vigor pos]}]

                         (print-it-every-ms (* vigor (pos->angular-velocity-sign pos)))
                         (* vigor (pos->angular-velocity-sign pos)))
                    effectors))))))




(defn brownian-motion
  [cart]
  (-> cart
      (update :velocity + (q/random-gaussian))
      (update :angular-velocity + (q/random-gaussian))))

(defn zero-force [e] (assoc e :vigor 0))

(defmulti ->moment-of-inertia :kind)

(defmethod ->moment-of-inertia :rect
  [{:keys [transform mass]}]
  (let [{:keys [width height]} transform]
    (* mass (+ (* width width) (* height height)) 12)))

(defn update-rotation [entity]
  (-> entity
      (update-in [:transform :rotation] #(+ % (* (:angular-velocity entity) *dt*)))))

(defn update-position
  [entity]
  (let [velocity (:velocity entity)
        rotation (-> entity :transform :rotation)
        x (* *dt* velocity (Math/cos rotation))
        y (* *dt* velocity (Math/sin rotation))]
    (-> entity
        (update-in [:transform :pos]
                   (fn [position]
                     (vector (+ (first position) x)
                             (+ (second position) y)))))))

(defn update-entity [entity]
  (-> entity
      update-body
      velocity-friction
      ;; brownian-motion
      ;; update-force
      update-position
      update-rotation))

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


(comment)
