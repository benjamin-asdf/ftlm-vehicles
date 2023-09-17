(ns ftlm.vehicles.art.lib
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]))

(defn normal-distr [mean std-deviation]
  (+ mean (* std-deviation (q/random-gaussian))))

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
  (into [] (repeatedly num-colors #(mod (normal-distr base 50) 360))))

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



(defn update-lifetime
  [state]
  (update state
          :entities
          (fn [circles]
            (into []
                  (comp (map (fn [{:as e :keys [lifetime]}]
                               (if lifetime (update e :lifetime dec) e)))
                        (remove (comp (fn [lf] (when lf (< lf 1))) :lifetime)))
                  circles))))

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
