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

(defn env [] {})
(def body identity)
(def sensors :sensors)
(def brain :brain)
(def effectors :motors)

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

(defn friction [velocity] (max 0 (- velocity 0.1)))

(defn velocity-friction [cart]
  (-> cart
      (update :velocity friction)
      (update :angular-velocity friction)))

(defn transform [e] (:transform e))
(defn position [e] (-> e transform :pos))

(defn v* [[a b] [a' b']]
  [(* a a')
   (* b b')])

(def anchor->trans-matrix
  {:top-right [1 -1]
   :top-left [-1 -1]
   :top-middle [0 -1.3]
   :bottom-left [-1 1]
   :bottom-right [1 1]
   :bottom-middle [0 1]})

;; parent is always a rect with draw mode :center right now

(defn relative-position [parent ent]
  (let [{:keys [width height]} (transform parent)
        m (anchor->trans-matrix (:anchor ent))]
    (v* m [(/ width 2) (/ height 2)])))

(defn ->sensor [anchor]
  (assoc
   (lib/->entity :circle)
   :transform (lib/->transform [0 0] 20 20 1)
   :anchor anchor
   :modality :temp
   :sensor? true
   :color 100))

(defn ->motor [pos vigor]
  (merge
   (lib/->entity :rect)
   {:motor? true
    :pos pos
    :activation vigor
    :transform (lib/->transform [0 0] 20 35 1)
    :anchor :bottom-middle
    :color 0}))

;; synonyms
(def signal-strengh :activation)
(def vigor :activation)
(def activation :activation)

(defn temperature-zone [])

(defmulti update-sensor (fn [sensor _env] (:modality sensor)))

;; dummy, dinstance from origin
(defn ->temp [pos env]
  (mod (Math/sqrt (+ (Math/pow (first pos) 2) (Math/pow (second pos) 2))) 10))

(defmethod update-sensor :temp
  [sensor env]
  (let [temp (->temp (position sensor) env)
        sensitivity 1]
    (assoc sensor signal-strengh (* sensitivity temp))))

(defn ->connection-model
  ([a b] (->connection-model a b identity))
  ([a b f] {:a a :b b :f f}))

(defn ->connection [entity-a entity-b]
  (merge
   (lib/->connection-line entity-a entity-b)
   {:connection-model (->connection-model entity-a entity-b)
    :connection? true}))

(defn transduce-signal [entity-a entity-b {:keys [f]}]
  (update entity-b :activation + (f (:activation entity-a))))

(defn activation-decay [{:keys [activation] :as entity}]
  (if activation
    (let [sign (lib/signum activation)
          activation (* sign (- (abs activation) 0.1))]
      (assoc entity :activation activation))
    entity))

(defn transduce-signals [state]
  (let [connection-by-a
        (into {}
              (juxt :a identity)
              (filter :connection? (lib/entities state)))
        ent-lut (lib/entities-by-id state)]
    (update
     state
     :entities
     (fn [ents]
       (map (fn [e]
              (if-let [conn (connection-by-a (:id e))]
                (transduce-signal e (ent-lut (:b conn)) conn)
                e))
            ents)))))

(defn ->brain [& connections] connections)
(defn ->body [])

(defn ->cart [spawn-point]
  (merge
   (lib/->entity :rect)
   {:angular-velocity 0
    :cart? true
    :color 30
    :transform (assoc (lib/->transform spawn-point 30 80 1) :rotation 0)
    :velocity 0}))

(defn cart-1
  []
  (let [sensor (->sensor :top-middle)
        motor (->motor :middle 0)
        line (->connection sensor motor)
        body (assoc (->cart [200 200])
                    :components (map :id [motor sensor])
                    :motors [motor]
                    :sensors [sensor])]
    [body sensor motor line]))

;; say motor vigor makes more velocity
;; velocity goes down with friction

(def pos->angular-velocity-sign {:left 1 :right -1 :middle 0})

(defn update-body
  [cart]
  (let [effectors (effectors cart)]
    (-> cart
        (update :velocity + (reduce + (map vigor effectors)))
        (update :angular-velocity
                +
                (reduce +
                  (map (fn [{:keys [pos] :as e}]
                         (* (vigor e) (pos->angular-velocity-sign pos)))
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
                                  (let [relative-position (relative-position parent ent)
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


(defn actication-shine
  [{:as entity :keys [activation]}]
  entity
  ;; (if activation
  ;;   (assoc entity :color (min 0 (max (* 100 activation) 360)))
  ;;   entity)
  )

(defn update-sensors [entity]
  (if (:sensor? entity) (update-sensor entity) entity))

(defn update-entity [entity state]
  (-> entity
      update-body
      velocity-friction
      ;; print-it-every-ms
      ;; brownian-motion
      update-rotation
      update-position
      (lib/update-conn-line state)
      update-sensors
      activation-decay
      actication-shine))

(defn update-state
  [state]
  (let [current-tick (q/millis)
        dt (/ (- current-tick (:last-tick state)) 1000.0)]
    (binding [*dt* dt]
      (-> state
          (assoc :last-tick current-tick)
          (update :entities (fn [ents] (doall (map #(update-entity % state) ents))))
          track-components))))

(defn window-dimensions []
  (let [w (.-innerWidth js/window)
        h (.-innerHeight js/window)]
    {:width w :height h}))

(defn setup
  [_controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  {:entities (cart-1) :last-tick (q/millis)})

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

(comment)
