(ns ftlm.vehicles.art.gaus-circles
  (:require
   [ftlm.vehicles.art :as art]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]))

(def default-controls
  {:spread 1
   :background 230
   :base-color 7.790258368269614
   :rand-color-count 8
   :speed 2
   :spread-spead 1
   :brownian-factor 0.1
   :infected-rate (/ 1 10)
   :circle-wobble 1
   :infectiousness (/ 1 5)
   :infected-color 255})

(defonce controls (atom default-controls))

(defn ->entity [kind]
  {:kind kind :id (random-uuid) :spawn-time (q/millis)})

(defn ->transform [pos width height scale]
  {:pos pos :width width :height height :scale scale})

(def entities :entities
  ;; (comp vals :entities)
  )

(def entities-by-id (comp #(into {} (map (juxt :id identity) %)) entities))

(defn ->connection-line [entity-a entity-b]
  (merge
   (->entity :line)
   {:connection-line? true
    :entity-a entity-a
    :entity-b entity-b}))

(def connection->infected :entity-a)
(def connection->non-infected :entity-b)

;; in ms
(defn age [entity] (- (q/millis) (:spawn-time entity)))

;; assume hsb
(defn shine [entity _dt]
  (update entity
          :color
          (fn [c]
            (mod (+ c (/ (* 1 (@controls :speed)) 2)) 255))))

(defn signum [x]
  (cond
    (< x 0) -1
    (> x 0) 1
    :else 0))

(defn sine-wave [frequency time-in-millis]
  (* (Math/sin (* 2 Math/PI (/ time-in-millis 1000) frequency))))

(defn generate-palette [base num-colors]
  (into [] (map (fn [_] (mod (+ base (rand 50)) 360)) (range num-colors))))

(defn move [{[xv yv] :velocity :as entity}]
  (update-in entity [:transform :pos] (fn [[x y]] [(+ x xv) (+ y yv)])))

(defn friction [{:keys [velocity friction] fr :friction :as entity}]
  (if (or (not velocity) (not friction))
    entity
    (update entity :velocity
            (fn [[xv yv]]
              [(* (signum xv) (max (abs (- (abs xv) fr)) 0))
               (* (signum yv) (max (abs (- (abs yv) fr)) 0))]))))

(defn wobble [{:keys [wobble spawn-time] :as entity}]
  (if-not wobble
    entity
    (update-in
     entity
     [:transform :scale]
     (fn [s]
       (+
        s
        (* wobble (/ (q/sin (* q/TWO-PI (/ (mod (- (q/millis) spawn-time) 1000) 1000))) 30)))))))

(defn brownian-motion [entity]
  (let [brownian-factor (:brownian-factor @controls)]
    (update entity :velocity (fnil (fn [[x y]] [(+ x
                                                   (* brownian-factor (q/random-gaussian)))
                                                (+ y
                                                   (* brownian-factor (q/random-gaussian)))])
                                   [0 0]))))

(defn update-infected [{:keys [infected?] :as entity}]
  (if infected? (assoc entity :color (-> @controls :infected-color)) entity))

(defn infection-jump [{:keys [entities] :as state}]
  (let [ents (filter :infectable? entities)
        non-infected
        (first
         (shuffle
          (remove :infected? ents)))
        infected
        (first
         (shuffle
          (filter :infected? ents)))]
    (if-not
        (and infected non-infected)
        state
        (update
         state
         :entities
         (fnil conj [])
         (merge
          (->connection-line
           (:id infected)
           (:id non-infected))
          {:color (get infected :color) :transform (->transform nil nil 1 1)
           :lifetime 20})))))

(defn entity-pos [state eid]
  (-> state entities-by-id (get eid) :transform :pos))

(defn update-conn-line [{:keys [connection-line? entity-b entity-a] :as entity} state]
  (if-not connection-line?
    entity
    (-> entity
        (assoc-in [:transform :pos] (entity-pos state entity-a))
        (assoc :end-pos (entity-pos state entity-b)))))

(defn update-entities [entity state]
  (-> entity
      move
      wobble
      friction
      brownian-motion
      update-infected
      (update-conn-line state)))

(defn rand-circle []
  (->
   (let [{:keys [spread]} @controls
         cx (/ (q/width) 2)
         cy (/ (q/height) 2)
         ;; angle (/ (mod (/ (q/millis) 200) 360) 1)
         radius 20

         px (+ cx (* 50 (* (q/random-gaussian) spread)))
         py (+ cy (* 50 (* (q/random-gaussian) spread)))

         ;; px (+ cx (* radius (q/cos angle)))
         ;; py (+ cy (* radius (q/sin angle)))
         dx (- px cx)
         dy (- py cy)
         dist (/ (Math/sqrt (+ (* dx dx) (* dy dy))) 1.3)
         ux (/ dx dist)
         uy (/ dy dist)
         {:keys [spread-speed circle-wobble color-palatte infected-rate]} @controls]
     (merge
      (->entity :circle)
      {:color (rand-nth color-palatte)
       :wobble circle-wobble
       :transform
       (->transform [px py] radius radius 1.5)
       :infected? (< (q/random 1) infected-rate)
       :friction (/ 1 (- 40 (rand 10)))
       :velocity [(* ux spread-speed) (* uy spread-speed)]
       :lifetime (+ 100 (rand 20))
       :infectable? true}))
   update-infected))

(defn random-cirlces [state n]
  (update state :entities (fn [ents] (concat ents (repeatedly n rand-circle)))))

(defn normal-distr [mean std-deviation]
  (+ mean (* std-deviation (q/random-gaussian))))

(defn infect? [exposure-time infectiousness]
  (< (/ (normal-distr 500 200) infectiousness) exposure-time))

(defn infect-connected-sometimes [{:keys [entities] :as state}]
  (let [infect? (let [i (-> @controls :infectiousness)]
                  #(infect? (age %) i))
        new-infected
        (into
         #{}
         (comp
          (filter :connection-line?)
          (filter infect?)
          (map connection->non-infected))
         entities)]
    (update
     state
     :entities
     (fn [ents]
       (map
        (fn [{:keys [infected? id] :as e}]
          (assoc e :infected? (boolean (or infected? (new-infected id)))))
        ents)))))

(defn alive? [state eid] (boolean (-> state entities-by-id (get eid))))
(def dead? (complement alive?))

(defn cleanup-connections [state]
  (update
   state
   :entities
   (fn [ents]
     (remove
      (fn [{:keys [connection-line?] :as e}]
        (when
            connection-line?
            (some #(dead? state %) [(connection->non-infected e) (connection->infected e)])))
      ents))))

(defn update-lifetime [state]
  (update state :entities
   (fn [circles]
     (into
      []
      (comp
       (map (fn [{:keys [lifetime] :as e}] (if lifetime (update e :lifetime dec) e)))
       (remove (comp (fn [lf] (when lf (< lf 1))) :lifetime)))
      circles))))

(defn update-state [state]
  (let [state
        (-> state
            (random-cirlces (max (sine-wave 2 (q/millis)) 0))
            infection-jump
            infect-connected-sometimes)
        state
        (-> state
            (update :entities #(map (fn [e] (update-entities e state)) %))
            update-lifetime
            cleanup-connections)]
    state))

(defn setup-controls [{:keys [rand-color-count base-color color-palatte] :as controls}]
  (merge
   controls
   (when-not color-palatte
     (when
         rand-color-count
         {:color-palatte
          (generate-palette base-color rand-color-count)}))))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  {})

(defn ->hsb [color] (if (sequential? color) color [color 255 255]))

(def draw-color (comp #(apply q/fill %) ->hsb))

(defmulti draw-entity :kind)

(defmethod draw-entity :circle [{:keys [transform]}]
  (let [[x y] (:pos transform)
        {:keys [width height scale]} transform]
    (q/ellipse x y (* scale width) (* scale height))))

(defmethod draw-entity :line [{:keys [transform end-pos]}]
  (let [[x y] (:pos transform)
        {:keys [_scale color]} transform]
    (q/stroke-weight 2)
    (q/with-stroke [color 255 255]
      (q/line [x y] end-pos))
    (q/stroke-weight 1)))

(defn draw-state [state]
  (q/background (-> @controls :background))
  (q/stroke-weight 1)
  (q/stroke 0.3)
  (doseq [{:keys [color] :as entity} (:entities state)]
    (draw-color color)
    ;; (q/fill 255)
    (draw-entity entity)))

(defn sketch [host]
  (q/sketch
   :host host
   :size [600 600]
   :setup setup
   :update update-state
   :draw draw-state
   :features [:keep-on-top]
   :middleware [m/fun-mode]))

(def versions
  {"1"
   {:spread 1
    :background 230
    :base-color 7.790258368269614
    :rand-color-count 8
    :speed 2
    :spread-spead 1
    :brownian-factor 0.1
    :infected-rate 0 ;; (/ 1 10)
    :circle-wobble 1}
   "2"
   {:spread 1
    :background 0
    ;; :color-palatte [120 150]
    :base-color 100
    :rand-color-count 4
    :speed 2
    :spread-spead 1
    :brownian-factor 0.1
    :infected-rate 0.5
    :infected-color 255
    :circle-wobble 1}})

(defmethod art/view "brownians"
  [{:keys [place version]}]
  (reset! controls (setup-controls (merge default-controls (get versions version))))
  (sketch place))

(comment
  (setup-controls default-controls)
  (swap! controls assoc :spread 1)
  (swap! controls assoc :brownian-factor 0.2)
  (swap! controls assoc :spread-spead 1)
  (swap! controls assoc :base-color (rand 360)))
