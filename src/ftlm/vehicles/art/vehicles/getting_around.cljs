(ns ftlm.vehicles.art.vehicles.getting-around
  (:require [ftlm.vehicles.art.lib :as lib]
            [ftlm.vehicles.art :as art]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [ftlm.vehicles.art.controls :refer [versions]]
            [ftlm.vehicles.art.user-controls :as user-controls]
            [ftlm.vehicles.art.controls :as controls]))

(defn print-it-every-ms [entity]
  (q/print-every-n-millisec 200 entity)
  entity)

(def ^:dynamic *dt* nil)

(def event-queue (atom []))

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

(defn draw-state
  [state]
  (q/background (-> state :controls :background-color))
  (q/stroke-weight 1)
  (q/stroke 0.3)
  (doseq [{:as entity :keys [color hidden?]} (:entities state)]
    (when-not hidden? (lib/draw-color color) (lib/draw-entity entity))))

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

(defn ->sensor
  [anchor]
  (assoc (lib/->entity :circle)
    :transform (lib/->transform [0 0] 20 20 1)
    :anchor anchor
    :modality :temp
    :sensor? true
    :color (q/color 40 96 255 255)))

(defn ->motor
  [anchor]
  (merge (lib/->entity :rect)
         {:anchor anchor
          :color (q/color 40 96 255 255)
          :motor? true
          :transform (lib/->transform [0 0] 20 35 1)}))

(defn normalize-value-1
  [min max value]
  (let [old-min min
        old-max max
        new-min 0
        new-max 1]
    (+ new-min (* (/ (- value old-min) (- old-max old-min)) (- new-max new-min)))))

(defn temperature-zone
  [pos d temp max-temp controls]
  (assoc
   (lib/->entity :circle)
   :transform (lib/->transform pos d d 1)
   :color  (q/lerp-color
            (apply q/color (-> controls :temperature-colors first))
            (apply q/color (-> controls :temperature-colors first))
            (normalize-value-1 0 max-temp temp))
   :temp-zone? true
   :d d
   :temp temp
   :particle? true
   :draggable? true
   :darts? true
   :always-darts? (:temp-zones-always-dart controls)))

(defmulti update-sensor (fn [sensor _env] (:modality sensor)))

(defn distance
  [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2))))

(defn normalize-value
  [min max value]
  (let [range (- max min)]
    (+ min (mod (/ (- value min) range) 1))))

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
        sensitivity 10
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

(defn ->trail [pos size color]
  (assoc
   (lib/->entity :circle)
   :transform (lib/->transform pos size size 0.2)
   :trail? true
   :particle? true
   :lifetime 20
   :color color))

(defn ->brain [& connections] connections)
(defn ->body [])

(defn ->cart
  [spawn-point scale rot color]
  (merge (lib/->entity :rect)
         {:cart? true
          :color color
          :transform (assoc (lib/->transform spawn-point 30 80 scale)
                       :rotation rot)}))

(defn cart-1
  [pos scale rot color]
  (let [sensor (->sensor :top-middle)
        motor (->motor :bottom-middle)
        line (->connection sensor motor)
        body (assoc (->cart pos scale rot color)
                    :components (map :id [motor sensor])
                    :motors (map :id [motor])
                    :sensors (map :id [sensor])
                    :particle? true
                    :darts? true
                    :makes-trail? true
                    ;; :lifetime 500
                    )]
    [body sensor motor line]))

;; say motor :activation makes more velocity
;; velocity goes down with friction-1
;;
;; angular-accelartion is something like max 5 and below 0.1 it does nothing
;; 5 is fast
;; positive is clockwise

(defn update-body
  [cart state]
  (if-not (:cart? cart)
    cart
    (let [effectors (effectors cart state)
          ;; [{:activation 3 :anchor :bottom-right}
          ;;  {:activation 4 :anchor :bottom-left}]
         ]
      (-> cart
          (update :acceleration + (reduce + (map :activation effectors)))
          (assoc :angular-acceleration (reduce +
                                         (map (fn [{:as e :keys [anchor]}]
                                                (* 0.2
                                                   (:activation e)
                                                   (anchor->rot-influence
                                                     anchor)))
                                              effectors)))))))
(defn brownian-motion
  [e]
  (if-not (:particle? e)
    e
    (-> e
        (update :acceleration
                +
                (* 30 (q/random-gaussian) (:brownian-factor (lib/controls))))
        (update
          :angular-acceleration
          +
          (* 0.3 (q/random-gaussian) (:brownian-factor (lib/controls)))))))

(defn update-rotation
  [entity]
  (let [angular-velocity (+ (:angular-velocity entity 0)
                    (* *dt* (:angular-acceleration entity 0)))]
    (-> entity
        (update-in [:transform :rotation] #(+ % angular-velocity))
        (assoc :angular-velocity angular-velocity))))

(defn move-dragged
  [entity]
  (if (:dragged? entity)
    (assoc-in entity [:transform :pos] [(q/mouse-x) (q/mouse-y)])
    entity))

(defn update-position
  [{:as entity :keys [velocity acceleration]}]
  (let [velocity (+ velocity (* *dt* acceleration))
        rotation (-> entity
                     :transform
                     :rotation)
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
    (->
     state
     (update
      :entities
      (fn [ents]
        (doall
         (map (fn [{:as ent :keys [id]}]
                (if-let [parent (parent-by-id id)]
                  (let [relative-position (relative-position parent ent)
                        parent-rotation (-> parent
                                            :transform
                                            :rotation)
                        scale (-> parent
                                  :transform
                                  :scale)]
                    (-> ent
                        (assoc-in
                         [:transform :pos]
                         [(+ (first (v* [scale scale]
                                        (rotate-point parent-rotation
                                                      relative-position)))
                             (first (-> parent
                                        :transform
                                        :pos)))
                          (+ (second (v* [scale scale]
                                         (rotate-point parent-rotation
                                                       relative-position)))
                             (second (-> parent
                                         :transform
                                         :pos)))])
                        (assoc-in [:transform :rotation]
                                  (-> parent
                                      :transform
                                      :rotation))
                        (assoc-in [:transform :scale] scale)))
                  ent))
              ents)))))))

(defn track-conn-lines
  [state]
  (update state
          :entities
          (fn [ents] (map #(lib/update-conn-line % state) ents))))

(defn activation-shine
  [{:as entity :keys [activation shine]}]
  (if activation
    (let [shine (+ shine (* *dt* activation))]
      (assoc entity
        :shine shine
        :color (q/lerp-color (q/color 40 96 255 255)
                             (q/color 100 255 255)
                             (normalize-value-1 0 1 (Math/sin shine)))))
    entity))

(defn update-sensors
  [entity state]
  (if (:sensor? entity) (update-sensor entity (env state)) entity))

(defn rand-on-canvas [] [(rand-int (q/width)) (rand-int (q/height))])
(defn rand-on-canvas-gauss
  [distr]
  [(lib/normal-distr (/ (q/width) 2) (* distr (/ (q/width) 2)))
   (lib/normal-distr (/ (q/height) 2) (* distr (/ (q/height) 2)))])


(defn random-temp-zone [controls]
  (temperature-zone
   (rand-on-canvas)
   (lib/normal-distr 300 80)
   (rand (:max-temp controls))
   (:max-temp controls)
   controls))

(defn dart-one [entity]
  (-> entity
      (update :angular-velocity + (rand-nth [-1 1]))
      (update :acceleration + 3000)))

(defn dart-a-few
  [state]
  (let [rands (into #{}
                    (take 5
                          (shuffle (sequence (comp (filter :darts?) (map :id))
                                             (lib/entities state)))))]
    (update state
            :entities
            (fn [ents]
              (doall (map (fn [e] (if (rands (:id e)) (dart-one e) e))
                       ents))))))


(defmulti event! (fn [e _] e))

(defmethod event! :dart
  [_ state]
  (dart-a-few state))

(defn apply-events
  [state]
  (reduce (fn [s e] (event! e s))
    state
    (let [r @event-queue]
      (reset! event-queue [])
      r)))

(defn mid-point [] [(/ (q/width) 2)
                    (/ (q/height) 2)])


(defn angle-between
  [[x1 y1] [x2 y2]]
  (Math/atan2 (- y1 y2)
              (- x1 x2)))

(defn orient-towards
  [entity target]
  (let [desired-angle (angle-between (position entity) target)]
    (assoc-in entity [:transform :rotation] (- desired-angle q/HALF-PI))))

(defn dart-to-middle
  [{:as entity :keys [darts?]}]
  (if (and darts?
           ;; (:everbody-darts? (q/state :controls))
           (<
            (lib/normal-distr 1000 200)
            (- (q/millis) (get entity :last-darted -500))))
    (-> entity
        (orient-towards (mid-point))
        (assoc :acceleration 1000)
        (assoc :last-darted (q/millis)))
    entity))

(defn inside-screen?
  [[x y]]
  (and (< 0 x (q/width))
       (< 0 y (q/height))))

(defn dart-distants-to-middle
  [{:as entity :keys [darts?]}]
  (if (and
       darts?
       (not (inside-screen? (position entity))))
    (dart-to-middle entity)
    entity))

(defn dart-always
  [{:as entity :keys [darts? always-darts?]}]
  (if (and darts? always-darts?)
    (dart-to-middle entity)
    entity))

(defn dart-everyboy
  [{:as entity :keys [darts?]}]
  (if (and darts?
           (:everbody-darts? (q/state :controls))
           (< 1000
              (- (q/millis)
                 (get entity :last-darted -500))))
    (-> ;; (dart-one entity)
     entity
     (orient-towards (mid-point))
     (assoc :acceleration (lib/normal-distr 1000 1))
     (assoc :last-darted (q/millis)))
    entity))

(defn update-entity [entity state]
  (-> entity
      (update-body state)
      friction
      ;; print-it-every-ms
      brownian-motion

      dart-distants-to-middle
      dart-always
      ;; dart-everyboy
      move-dragged
      update-rotation
      update-position

      (update-sensors state)
      activation-decay
      activation-shine))

(defn make-trails
  [state]
  (let [make-trail
        (into
         {}
         (comp
          (filter :makes-trail?)
          (filter (fn [e] (< 500 (- (q/millis) (:made-trail e)))))
          (map (juxt :id identity)))
         (lib/entities state))
        new-trial
        (map
         (fn [e]
           (->trail (position e)
                    (-> state
                        :controls
                        :trail-size)
                    (-> state
                        :controls
                        :trail-color)))
         (vals make-trail))]
    (->
     state
     (lib/append-ents new-trial)
     (lib/update-ents (fn [e]
                        (if (make-trail (:id e))
                          (assoc e :made-trail (q/millis))
                          e))))))

(defn update-state
  [state]
  (let [current-tick (q/millis)
        state (update state :controls merge @user-controls/!app)
        dt (* (:time-speed (lib/controls)) (/ (- current-tick (:last-tick state)) 1000.0))]
    (binding [*dt* dt]
      (let [state (apply-events state)]
        (-> state
            (assoc :last-tick current-tick)
            (update :entities
                    (fn [ents] (doall (map #(update-entity % state) ents))))
            transduce-signals
            track-components
            track-conn-lines
            make-trails
            lib/update-lifetime
            lib/cleanup-connections)))))

(defn window-dimensions []
  (let [w (.-innerWidth js/window)
        h (.-innerHeight js/window)]
    {:width w :height h}))

(defn setup
  [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (q/background (-> controls
                    :background-color))
  (let [controls (if-not (:color-palatte controls)
                   (assoc controls
                     :color-palatte (lib/generate-palette
                                      (:palette-base-color controls)
                                      (:num-random-colors controls)))
                   controls)]
    (-> {:controls controls
         :entities (concat
                     (when (controls :middle-temp-zone?)
                       [(temperature-zone [(/ (q/width) 2) (/ (q/height) 2)]
                                          (-> controls
                                              :middle-temp-zone
                                              :diameter)
                                          (:max-temp controls)
                                          (:max-temp controls)
                                          controls)])
                     (repeatedly (:temp-zone-count controls)
                                 #(random-temp-zone controls))
                     (mapcat identity
                       (repeatedly
                        (:spawn-amount controls)
                       #(cart-1 (rand-on-canvas-gauss (:spawn-spread
                                                        controls))
                                 (-> controls
                                     :cart-scale)
                                 (* q/TWO-PI (rand))
                                 (rand-nth (-> controls
                                               :color-palatte)))))
                     )
         :last-tick (q/millis)}
        track-components
        track-conn-lines)))

(defn find-closest-draggable
  [state]
  (let [mouse-position [(q/mouse-x) (q/mouse-y)]]
    (->> state
         lib/entities
         (filter :draggable?)
         (sort-by (comp (fn [b] (distance mouse-position b)) lib/position))
         (first))))

(defn mouse-pressed
  [state]
  (let [draggable (find-closest-draggable state)]
    (-> state
        (assoc :pressed true)
        (update :entities
                (fn [ents]
                  (doall
                   (map
                    (fn [e]
                      (if (= draggable e) (assoc e :dragged? true) e))
                    ents)))))))

(defn mouse-released
  [state]
  (let [draggable (find-closest-draggable state)]
    (-> state
        (assoc :pressed false)
        (update :entities
                (fn [ents]
                  (doall
                   (map
                    (fn [e]
                      (dissoc e :dragged?))
                    ents)))))))

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
              :mouse-pressed mouse-pressed
              :mouse-released mouse-released
              :frame-rate 30)))

(let [restart-fn (atom nil)]
  (defmethod art/view "getting-around"
    [{:keys [place version]}]
    (let
        [f (fn []
             (sketch
              place
              (merge
               (controls/default-versions "getting-around")
               (get-in versions ["getting-around" version])
               @user-controls/!app)))]
        (reset! restart-fn f)
        (f)))
  (defmethod user-controls/action-button ::restart
    [_]
    (some-> @restart-fn (apply nil)))
  )

(defmethod user-controls/action-button ::dart!
  [_]
  (swap! event-queue conj :dart))

(comment)
