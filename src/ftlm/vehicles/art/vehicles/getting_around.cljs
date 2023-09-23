(ns ftlm.vehicles.art.vehicles.getting-around
  (:require
   [ftlm.vehicles.art.lib :as lib]
   [ftlm.vehicles.art :as art]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [ftlm.vehicles.art.controls :refer [versions]]
   [ftlm.vehicles.art.user-controls :as user-controls]))

(def default-controls {})

(defn print-it-every-ms [entity]
  (q/print-every-n-millisec 200 entity)
  entity)

(def ^:dynamic *dt* nil)

;; env
;; body
;; sensors
;; effectors
;; brain (connections)

;; I try a cartoon physics,
;; vigor makes your velocity go up, friction removes it

(defn draw-state [state]
  (q/background 230)
  (q/stroke-weight 1)
  (q/stroke 0.3)
  (doseq [{:keys [color hidden?] :as entity} (:entities state)]
    (when-not hidden?
      (lib/draw-color color)
      (lib/draw-entity entity))))

(defn rotate-point [rotation [x y]]
  [(+ (* x (Math/cos rotation)) (* -1 y (Math/sin rotation)))
   (+ (* x (Math/sin rotation)) (* y (Math/cos rotation)))])

(defn translate-point [x y dx dy]
  [(+ x dx) (+ y dy)])

(defn scale-point [[x y] scale]
  [(x * scale) (y * scale)])

(defn sensor-pos-absolute [cart sensor]
  (let [{:keys [pos rotation]} (cart :transform)
        rel-pos [0 0] ;; (sensor :pos)
        scale (cart :scale)
        rot-pos (rotate-point rotation rel-pos)
        abs-pos (translate-point (first rot-pos) (second rot-pos) (first pos) (second pos))
        final-pos (scale-point abs-pos scale)]
    final-pos))


(defn friction [velocity] (max 0 (- velocity 0.1)))

(defn velocity-friction [cart]
  (-> cart
      (update :velocity friction)
      (update :angular-velocity friction)))

;; (defn motor-friction [cart]
;;   (update cart :motors update-vals #(update % :vigor friction)))

(defn ->motor [pos vigor]
  {:pos pos :vigor vigor})

(defn cart-1 []
  {:motors [(->motor :left 4)]})

(defn env [] {})
(def body identity)
(def sensors :sensors)
(def brain :brain)
(def effectors :motors)

(defn ->cart []
  (let [sensor
        (assoc
         (lib/->entity :circle)
         :transform
         (lib/->transform [0 0] 20 20 1)
         :relative-position [0 40])]
      [(merge
        (lib/->entity :rect)
        {:acceleration 0
         :angular-acceleration 0
         :angular-velocity 0
         :angular-force 0
         :cart? true
         :color 100
         :mass 1
         :transform (assoc (lib/->transform [200 200] 40 80 1) :rotation q/PI)
         :velocity 0
         :components (into [] (map :id [sensor]))}
        (cart-1))
       sensor]))

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
                         (* vigor (pos->angular-velocity-sign pos)))
                    effectors))))))

(defn brownian-motion
  [cart]
  (-> cart
      (update :velocity + (q/random-gaussian))
      (update :angular-velocity + (q/random-gaussian))))

(defn update-rotation [entity]
  (-> entity
      (update-in
       [:transform :rotation]
       #(+ % (* (:angular-velocity entity) *dt*)))))

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

(defn track-components
  [state]
  (let [parent-by-id (into {}
                           (mapcat (fn [ent]
                                     (map (juxt identity (constantly ent))
                                          (:components ent)))
                                   (filter :components (lib/entities state))))]
    (-> state
        (update :entities
                (fn [ents]
                  (doall (map (fn [{:as ent :keys [id]}]
                                (if-let [parent (parent-by-id id)]
                                  (let [{:keys [relative-position]} ent
                                        parent-rotation (-> parent :transform :rotation)]
                                    (assoc-in ent
                                              [:transform :pos]
                                              [(+ (first (rotate-point parent-rotation relative-position))
                                                  (first (-> parent
                                                             :transform
                                                             :pos)))
                                               (+ (second (rotate-point parent-rotation relative-position))
                                                  (second (-> parent
                                                              :transform
                                                              :pos)))]))
                                  ent))
                              ents)))))))


(defn update-entity [entity]
  (-> entity
      update-body
      velocity-friction
      ;; print-it-every-ms
      ;; brownian-motion
      update-rotation
      update-position))

(defn update-state
  [state]
  (let [current-tick (q/millis)
        dt (/ (- current-tick (:last-tick state)) 1000.0)]
    (binding [*dt* dt]
      (-> state
          (assoc :last-tick current-tick)
          (update :entities (fn [ents] (doall (map update-entity ents))))
          track-components))))

(defn window-dimensions []
  (let [w (.-innerWidth js/window)
        h (.-innerHeight js/window)]
    {:width w :height h}))

(defn setup
  [_controls]
  (q/rect-mode :center)
  {:entities (->cart) :last-tick (q/millis)})

(defn sketch
  [host controls]
  (let [{:keys [width height]} (window-dimensions)]
    (q/sketch :host host
              :size [width height]
              :setup (partial setup controls)
              :update update-state
              :draw draw-state
              :features [:keep-on-top]
              :middleware [m/fun-mode]
              :frame-rate 49)))

(defmethod art/view "getting-around"
  [{:keys [place version]}]
  (sketch
   place
   (merge
    default-controls
    (get-in versions ["getting-around" version]))))

(comment



  )
