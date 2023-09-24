(ns ftlm.vehicles.art.vehicles.getting-around
  (:require
   [ftlm.vehicles.art.lib :as lib]
   [ftlm.vehicles.art :as art]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [ftlm.vehicles.art.controls :refer [versions]]
   ;; [ftlm.vehicles.art.user-controls :as user-controls]
   ))

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

(defn env [state]
  {:temperature-zones
   (->> state lib/entities (filter :temp-zone?))})

(def body identity)
(def sensors :sensors)
(def brain :brain)

(defn effectors [entity state]
  (->> entity :motors (map (lib/entities-by-id state))))

;; I try a cartoon physics,
;; :activation makes your velocity go up, friction-1 removes it

(defn draw-state
  [state]
  (q/background 230 ;; (q/color 330 100 100 255)
                )
  (q/stroke-weight 1)
  (q/stroke 0.3)
  (doseq [{:as entity :keys [color hidden?]} (:entities state)]
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

(defn friction-1 [velocity]
  (* velocity 0.9))

(defn friction [cart]
  (-> cart
      (update :velocity friction-1)
      (update :acceleration friction-1)
      (update :angular-velocity friction-1)
      (update :angular-acceleration friction-1)))

(defn transform [e] (:transform e))
(defn position [e] (-> e transform :pos))

(defn v* [[a b] [a' b']]
  [(* a a')
   (* b b')])

(def anchor->trans-matrix
  {:top-right [1 -1]
   :top-left [-1 -1]
   :top-middle [0 -1.2]
   :bottom-left [-1 1]
   :bottom-right [1 1]
   :bottom-middle [0 1]})

(def anchor->rot-influence
  {;; :top-right -1
   ;; :top-left 1
   ;; :top-middle 0
   :bottom-left 1
   :bottom-right -1
   :bottom-middle 0})

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

(defn ->motor [anchor]
  (merge
   (lib/->entity :rect)
   {:motor? true
    :transform (lib/->transform [0 0] 20 35 1)
    :anchor anchor
    :color 0}))

(defn normalize-value-1
  [min max value]
  (let [old-min min
        old-max max
        new-min 0
        new-max 1]
    (+ new-min (* (/ (- value old-min) (- old-max old-min)) (- new-max new-min)))))

(defn temperature-zone
  [pos d temp]
  (assoc
   (lib/->entity :circle)
   :transform (lib/->transform pos d d 1)
   :color (q/lerp-color
           (q/color 240 100 100 25)
           (q/color 0 255 255 25)
           (normalize-value-1 -1 1 temp))
   :temp-zone? true
   :d d
   :temp temp))

(defmulti update-sensor (fn [sensor _env] (:modality sensor)))

(defn distance
  [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2))))

(defn normalize-value
  [min max value]
  (let [range (- max min)]
    (+ min (mod (/ (- value min) range) 1))))

;; (defn point-inside-circle? [[x y] origin radius])

(defn point-inside-circle?
  [[x y] [ox oy] d]
  (let [radius (/ d 2)]
    (<= (+ (Math/pow (- x ox) 2)
           (Math/pow (- y oy) 2))
        (Math/pow radius 2))))

(defn ->temp [sensor-pos env]
  (->> env
       :temperature-zones
       (filter
        (fn [{:keys [temp d] :as e}]
          (let
              [origin (position e)]
              (point-inside-circle? sensor-pos origin d))))
       (map :temp)
       (reduce +)))

(defmethod update-sensor :temp
  [sensor env]
  (let [temp (->temp (position sensor) env)
        sensitivity 1
        activation (* sensitivity (max 0 temp))]
    (assoc sensor :activation activation)))

(defn ->connection-model
  ([a b] (->connection-model a b identity))
  ([a b f] {:a a :b b :f f}))

(defn ->connection [entity-a entity-b]
  (merge
   (lib/->connection-line entity-a entity-b)
   {:connection-model (->connection-model (:id entity-a) (:id entity-b))
    :connection? true}))

(def connection->source (comp :a :connection-model))
(def connection->destination (comp :b :connection-model))

(defn transduce-signal [destination source {:keys [f]}]
  (update destination :activation + (f (:activation source))))

(defn activation-decay [{:keys [activation] :as entity}]
  (if activation
    (let [sign (lib/signum activation)
          activation (* sign (- (abs activation) 0.2) 0.8)]
      (assoc entity :activation activation))
    entity))

(defn transduce-signals
  [state]
  (let [connection-by-destination (into {}
                                        (comp (filter :connection?)
                                              (map (juxt connection->destination
                                                         identity)))
                                        (lib/entities state))
        ent-lut (lib/entities-by-id state)]
    (update state
            :entities
            (fn [ents]
              (map (fn [e]
                     (if-let [conn-e (connection-by-destination (:id e))]
                       (let [source (ent-lut (connection->source conn-e))]
                         (transduce-signal e source (:connection-model conn-e)))
                       e))
                ents)))))

(defn ->brain [& connections] connections)
(defn ->body [])

(defn ->cart [spawn-point]
  (merge (lib/->entity :rect)
         {:cart? true
          :color 30
          :transform
          (assoc
           (lib/->transform spawn-point 30 80 0.4)
           :rotation q/HALF-PI)}))

(defn cart-1 [pos]
  (let [sensor (->sensor :top-middle)
        motor (->motor :bottom-middle)
        line (->connection sensor motor)
        body (assoc (->cart pos)
                    :components (map :id [motor sensor])
                    :motors (map :id [motor])
                    :sensors (map :id [sensor]))]
    [body sensor motor line]))

;; say motor :activation makes more velocity
;; velocity goes down with friction-1
;;
;; angular-accelartion is something like max 5 and below 0.1 it does nothing
;; 5 is fast
;; positive is clockwise

(defn update-body
  [cart state]
  (if-not
      (:cart? cart) cart
      (let [effectors (effectors cart state)
            ;; [{:activation 3 :anchor :bottom-right}
            ;;  {:activation 4 :anchor :bottom-left}]
            ]
        (-> cart
            (update :acceleration + (reduce + (map :activation effectors)))
            (assoc
             :angular-acceleration
             (reduce +
                     (map (fn [{:keys [anchor] :as e}]
                            (* 0.2 (:activation e) (anchor->rot-influence anchor)))
                          effectors)))))))

(defn brownian-motion
  [cart]
  (if-not
      (:cart? cart)
      cart
      (-> cart
          (update :acceleration + (* 30 (q/random-gaussian)))
          (update :angular-acceleration + (* 0.3 (q/random-gaussian))))))

(defn update-rotation [entity]
  (let [velocity
        (+ (:angular-velocity entity 0)
           (* *dt* (:angular-acceleration entity 0)))]
    (-> entity
        (update-in [:transform :rotation] #(+ % velocity))
        (assoc :angular-velocity velocity))))

(defn update-position
  [{:keys [velocity acceleration] :as entity}]
  (let [velocity (+ velocity (* *dt* acceleration))
        rotation (-> entity :transform :rotation)
        x (* *dt* velocity (Math/sin rotation))
        y (* *dt* velocity (- (Math/cos rotation)))]
    (-> entity
        (assoc :velocity velocity)
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
                                        parent-rotation (-> parent :transform :rotation)
                                        scale (-> parent :transform :scale)]
                                    (->
                                     ent
                                     (assoc-in
                                      [:transform :pos]
                                      [(+ (first
                                           (v* [scale scale]
                                              (rotate-point parent-rotation relative-position)))
                                          (first (-> parent :transform :pos)))
                                       (+
                                        (second
                                         (v* [scale scale] (rotate-point parent-rotation relative-position)))
                                        (second (-> parent :transform :pos)))])
                                     (assoc-in
                                      [:transform :rotation]
                                      (-> parent :transform :rotation))
                                     (assoc-in [:transform :scale] scale)))
                                  ent))
                              ents)))))))

(defn track-conn-lines [state]
  (update state :entities (fn [ents] (map #(lib/update-conn-line % state) ents))))

(defn actication-shine
  [{:as entity :keys [activation shine]}]
  (if activation
    (let [shine (+ shine (* *dt* activation))]
      (assoc entity
             :shine shine
             :color
             (q/lerp-color
              (q/color 40 96 255 255)
              (q/color 100 255 255)
              (normalize-value-1 0 1 (Math/sin shine)))))
    entity))

(defn update-sensors [entity state]
  (if (:sensor? entity) (update-sensor entity (env state)) entity))

(defn clamp-velocity [entity] (update entity :acceleration #(max % 0)))

(defn rand-on-canvas [] [(rand-int (q/width)) (rand-int (q/height))])

(defn random-temp-zone []
  (temperature-zone
   (rand-on-canvas)
   (lib/normal-distr 300 80)
   (+ -1 (rand 2))))

(defn update-entity [entity state]
  ;; (when (:sensor? entity)
  ;;   (print-it-every-ms (:activation entity)))
  (-> entity
      (update-body state)
      friction
      ;; print-it-every-ms
      brownian-motion

      update-rotation
      update-position

      (update-sensors state)
      activation-decay
      actication-shine))

(defn update-state
  [state]
  (let [current-tick (q/millis)
        dt (* 3 (/ (- current-tick (:last-tick state)) 1000.0))]
    (binding [*dt* dt]
      (-> state
          (assoc :last-tick current-tick)
          (update :entities (fn [ents] (doall (map #(update-entity % state) ents))))
          transduce-signals
          track-components
          track-conn-lines))))

(defn window-dimensions []
  (let [w (.-innerWidth js/window)
        h (.-innerHeight js/window)]
    {:width w :height h}))

(defn setup
  [_controls]

  (q/rect-mode :center)
  (q/color-mode :hsb)
  (-> {:entities
       (concat
        (repeatedly 10 random-temp-zone)
        (mapcat identity (repeatedly 20 #(cart-1 (rand-on-canvas))))
        ;; [(assoc (lib/->entity :circle)
        ;;         :color 0
        ;;         :transform (lib/->transform [400 400] 20 20 1))]
        )
       :last-tick (q/millis)}
      track-components
      track-conn-lines))

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
              :frame-rate 30)))

(defmethod art/view "getting-around"
  [{:keys [place version]}]
  (sketch
   place
   (merge
    default-controls
    (get-in versions ["getting-around" version]))))

(comment)
