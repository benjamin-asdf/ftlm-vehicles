(ns ftlm.vehicles.art.gaus-circles
  (:require
   [ftlm.vehicles.art :as art]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]))

(defn normal-distr [mean std-deviation]
  (+ mean (* std-deviation (q/random-gaussian))))

(def default-controls
  {:spread 1
   :background 230
   :base-color 7.790258368269614
   :rand-color-count 8
   :speed 2
   :spread-speed 0
   :brownian-factor 0.1
   :infected-rate (/ 1 10)
   :circle-wobble 1
   :friction [(/ 1 43) (/ 1 50)]
   :infectiousness (/ 1 5)
   :infected-color 255
   :spawn-rate {:base 1 :freq 2 :pow 1}
   :circle-shine 0
   :circle-scale 1.5
   :hide-lines? false})

(defn controls []
  (q/state :controls))

(defn ->entity [kind]
  {:kind kind :id (random-uuid) :spawn-time (q/millis)})

(defn ->transform [pos width height scale]
  {:pos pos :width width :height height :scale scale})

(def entities :entities)

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

(defn ->hsb [color]
  (cond
    (map? color) [(:h color) (:s color) (:b color)]
    (sequential? color) color
    :else [color 255 255]))

(defn shine [{:keys [shine] :as entity}]
  (if-not
      shine entity
      (update
       entity
       :color
       (fn [c]
         (update-in (->hsb c) [0] (comp #(mod % 255) (partial + shine)))))))

(defn signum [x]
  (cond
    (< x 0) -1
    (> x 0) 1
    :else 0))

(defn sine-wave [frequency time-in-millis]
  (* (Math/sin (* 2 Math/PI (/ time-in-millis 1000) frequency))))

(defn spawn-rate-pow [frequency time-in-millis pow]
  (max (Math/pow (sine-wave frequency time-in-millis) pow) 0))

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
  (let [brownian-factor (:brownian-factor (controls))]
    (update entity :velocity (fnil (fn [[x y]] [(+ x
                                                   (* brownian-factor (q/random-gaussian)))
                                                (+ y
                                                   (* brownian-factor (q/random-gaussian)))])
                                   [0 0]))))

(defn *transform [t1 trsf]
  (merge-with * t1 trsf))

(defn update-infected [{:keys [infected? transformed?] :as entity}]
  (if
      (or (not infected?)
          transformed?)
      entity
      (-> entity
          (assoc :transformed? true)
          (assoc :color (-> (controls) :infected-color))
          (update :transform *transform (-> (controls) :infected-transform)))))

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
          {:color (get infected :color)
           :transform
           (->transform nil 1 1 1)
           :lifetime 20
           :hidden? ((controls) :hide-lines?)})))))

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
      shine
      wobble
      friction
      brownian-motion
      update-infected
      (update-conn-line state)))

(defn rand-circle []
  (->
   (let [{:keys [spread]} (controls)
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
         {:keys [circle-scale
                 circle-shine
                 friction
                 spread-speed
                 circle-wobble
                 color-palatte
                 infected-rate]} (controls)]
     (merge
      (->entity :circle)
      {:color (rand-nth color-palatte)
       :wobble circle-wobble
       :transform (->transform [px py] radius radius circle-scale)
       :infected? (< (q/random 1) infected-rate)
       :friction (apply normal-distr friction)
       :velocity [(* ux spread-speed) (* uy spread-speed)]
       :lifetime (+ 100 (rand 20))
       :infectable? true
       :shine circle-shine}))
   update-infected))

(defn random-cirlces [state n]
  (update state :entities (fn [ents] (concat ents (repeatedly n rand-circle)))))

(defn infect? [exposure-time infectiousness]
  (< (/ (normal-distr 500 200) infectiousness) exposure-time))

(defn infect-connected-sometimes [{:keys [entities] :as state}]
  (let [infect? (let [i (-> (controls) :infectiousness)]
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
            (random-cirlces
             (let [{:keys [base freq pow]} (:spawn-rate (controls))]
               (* base (spawn-rate-pow freq (q/millis) pow))))
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

(defn setup [controls]
  (q/frame-rate 30)
  (q/color-mode :hsb)
  {:controls controls})

(def draw-color (comp #(apply q/fill %) ->hsb))

(defmulti draw-entity :kind)

(defmethod draw-entity :circle [{:keys [transform]}]
  (let [[x y] (:pos transform)
        {:keys [width height scale]} transform]
    (q/ellipse x y (* scale width) (* scale height))))

(defmethod draw-entity :line [{:keys [transform end-pos color]}]
  (let [[x y] (:pos transform)
        {:keys [_scale]} transform]
    (q/stroke-weight 2)
    (q/with-stroke (->hsb color) ;; (->hsb color)
      (q/line [x y] end-pos))
    (q/stroke-weight 1)))

(defn draw-state [state]
  (q/background (-> (controls) :background))
  (q/stroke-weight 1)
  (q/stroke 0.3)
  (doseq [{:keys [color hidden?] :as entity} (:entities state)]
    (when-not hidden?
      (draw-color color)
      (draw-entity entity))))

(defn sketch [host controls]
  (q/sketch
   :host host
   :size [600 600]
   :setup (partial setup controls)
   :update update-state
   :draw draw-state
   :features [:keep-on-top]
   :middleware [m/fun-mode]))

(def
  cyberpunk-palette-hsb
  [{:h 180 :s 255 :b 255}
   ;; Neon Blue
   {:h -20 :s 255 :b 255}
   ;; Hot Pink
   {:h -53 :s 255 :b 255}
   ;; Vibrant Purple
   {:h 117 :s 191.25 :b 204}
   ;; Acid Green
   {:h 0 :s 255 :b 139}
   ;; Deep Red
   ])

(def white [0 0 255])

(def versions
  {"1"
   {:spread 1
    :background 230
    :base-color 7.790258368269614
    :rand-color-count 8
    :spread-speed 1
    :brownian-factor 0.1
    :infected-rate 0
    :circle-wobble 1}
   "2"
   {:spread 1
    :background 230
    :base-color 150
    :rand-color-count 4
    :spread-speed 1
    :brownian-factor 0.1
    :infected-rate (/ 1 10)
    :infected-color white
    :circle-wobble 1}
   "3"
   {:spread 1
    :background 0
    :base-color 30
    :rand-color-count 8
    :spread-speed 1
    :brownian-factor 0.1
    :infected-rate (/ 1 10)
    :infected-color (->hsb white)
    :circle-wobble 1}
   "4"
   {:spread 2
    :background 130
    :base-color 30
    :rand-color-count 8
    :spread-speed 1
    :brownian-factor 0.1
    :infected-rate (/ 1 10)
    :infected-color 0
    :circle-wobble 0}
   "5"
   {:spread 0.4
    :background 230
    :rand-color-count 8
    :spread-speed 10
    :brownian-factor 0.2
    :infected-rate 0
    :infected-color 0
    :circle-wobble 0
    :friction [(/ 1 100) (/ 1 50)] }
   "6"
   {:spread 0.4
    :background 230
    :rand-color-count 8
    :spread-speed 3
    :brownian-factor 0
    :infected-rate 0
    :infected-color 0
    :circle-wobble 0
    :friction [0 0]
    :spawn-rate {:base 1 :freq 0.4 :pow 5}}
   "7"
   {:spread 1
    :background 230
    :rand-color-count 8
    :spread-speed 0
    :brownian-factor 0.05
    :infected-rate 0
    :infected-color 0
    :circle-wobble 0.4
    :circle-shine 2
    :spawn-rate {:base 1 :freq 1 :pow 1}}
   "8"
   {:spread 1
    :background 0
    :rand-color-count 8
    :speed 2
    :spread-speed 0
    :brownian-factor 0.05
    :infected-rate 0
    :infected-color 0
    :circle-wobble 0.4
    :circle-shine 2
    :spawn-rate {:base 1 :freq 1 :pow 1}}
   "9"
   {:spread 1.5
    :background 0
    :rand-color-count 2
    :spread-speed 1
    :brownian-factor 0.05
    :infected-rate 0
    :infected-color 0
    :circle-wobble 0.4
    :circle-shine 0.2
    :spawn-rate {:base 1 :freq 1 :pow 1}}
   "10"
   {:spread 1.5
    :background 0
    :rand-color-count 2
    :spread-speed 1
    :brownian-factor 0.05
    :infected-rate (/ 1 3)
    :infected-color white
    :circle-wobble 0.4
    :circle-shine 0.2
    :spawn-rate {:base 1 :freq 1 :pow 1}}
   "11"
   {:spread 1.5
    :background 0
    :rand-color-count 2
    :spread-speed 1
    :brownian-factor 0.05
    :infected-rate (/ 1 3)
    :infected-color white
    :circle-wobble 0.4
    :circle-shine 0.2
    :infected-transform {:scale 0.8}
    :friction [(/ 4 100) (/ 1 100)]
    :spawn-rate {:base 1 :freq 1 :pow 1}}
   "12"
   {:spread 1.5
    :background 0
    :rand-color-count 2
    :spread-speed 1
    :brownian-factor 0.05
    :infected-rate (/ 1 3)
    :infected-color 15
    :circle-wobble 0.4
    :circle-shine 0.2
    :infected-transform {:scale 1.8}
    :friction [(/ 4 100) (/ 1 100)]
    :spawn-rate {:base 1 :freq 1 :pow 1}}
   "13"
   {:spread 1.5
    :background 80
    :rand-color-count 2
    :spread-speed 1
    :brownian-factor 0.1
    :infected-rate (/ 1 3)
    :infected-color 15
    :circle-wobble 0.4
    :circle-shine 0.2
    :infected-transform {:scale 1.8}
    :friction [(/ 4 100) (/ 1 100)]
    :spawn-rate {:base 1 :freq 1 :pow 1}}
   "14"
   {:spread 1.5
    :background 0
    :rand-color-count 2
    :spread-speed 1
    :brownian-factor 0.1
    :infected-rate (/ 1 3)
    :color-palatte [white]
    :infected-color white
    :circle-wobble 0.4
    :circle-shine 0.2
    :infected-transform {:scale 10}
    :friction [(/ 4 100) (/ 1 100)]
    :circle-scale 0.1
    :spawn-rate {:base 2 :freq 1.4 :pow 1}}
   "15"
   {:spread 1.5
    :background 0
    :rand-color-count 2
    :base-color 360
    :spread-speed 1.4
    :brownian-factor 0.4
    :infected-rate (/ 1 10)
    :infected-color white
    :circle-wobble 0.4
    :circle-shine 0.2
    :infected-transform {:scale 3}
    :friction [(/ 4 100) (/ 1 100)]
    :circle-scale 0.2
    :spawn-rate {:base 4 :freq 1.4 :pow 1}
    :hide-lines? true}
   "16"
   {:spread 1.5
    :background 0
    :rand-color-count 2
    :base-color 360
    :spread-speed 1.4
    :brownian-factor 0.4
    :infected-rate 0
    :infected-color white
    :circle-wobble 0.4
    :circle-shine 0.2
    :infected-transform {:scale 3}
    :friction [(/ 4 100) (/ 1 100)]
    :circle-scale 0.2
    :spawn-rate {:base 4 :freq 1.4 :pow 1}
    :hide-lines? true}
   "17"
   {:spread 1
    :background 0
    :rand-color-count 1
    :base-color 360
    :spread-speed 0.8
    :brownian-factor 0.3
    :infected-rate 0
    :infected-color white
    :circle-wobble 0.4
    :circle-shine 0.2
    :infected-transform {:scale 3}
    :friction [(/ 4 100) (/ 1 100)]
    :circle-scale 0.2
    :spawn-rate {:base 4 :freq 1.4 :pow 1}
    :hide-lines? true}
   "18"
   {:spread 5
    :background 0
    :rand-color-count 8
    :base-color 360
    :spread-speed 0.3
    :brownian-factor 0.3
    :infected-rate 0
    :infected-color white
    :circle-wobble 2
    :circle-shine 0.2
    :infected-transform {:scale 3}
    :friction [(/ 4 100) (/ 1 100)]
    :circle-scale 0.2
    :spawn-rate {:base 4 :freq 1.4 :pow 1}
    :hide-lines? true}
   "19"
   {:spread 5
    :background 230
    :rand-color-count 8
    :base-color 360
    :spread-speed 0.3
    :brownian-factor 0.3
    :infected-rate 0
    :infected-color white
    :circle-wobble 2
    :circle-shine 0.2
    :infected-transform {:scale 3}
    :friction [(/ 4 100) (/ 1 100)]
    :circle-scale 0.2
    :spawn-rate {:base 4 :freq 1.4 :pow 1}
    :hide-lines? true}
   "20"
   {:spread 5
    :background 230
    :rand-color-count 8
    :base-color 360
    :spread-speed 0.3
    :brownian-factor 0.3
    :infected-rate (/ 1 6)
    :infected-color (rand (inc 360))
    :circle-wobble 2
    :circle-shine 0.2
    :infected-transform {:scale 1.1}
    :friction [(/ 4 100) (/ 1 100)]
    :circle-scale 0.2
    :spawn-rate {:base 4 :freq 1.4 :pow 1}}})

(defmethod art/view "brownians"
  [{:keys [place version]}]
  (sketch place (setup-controls (merge default-controls (get versions version)))))

(comment
  (setup-controls default-controls)
  )
