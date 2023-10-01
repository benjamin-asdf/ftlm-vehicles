(ns ftlm.vehicles.art.lib
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]))

(def ^:dynamic *dt* nil)

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

(defn update-ents [state f]
  (update state :entities (fn [ents] (doall (map f ents)))))

(defn append-ents [state ents]
  (update state :entities concat ents))

(defn transform [e] (:transform e))
(defn position [e] (-> e transform :pos))

(defn ->connection-line [entity-a entity-b]
  (merge
   (->entity :vine)
   {:connection-line? true
    :entity-a (:id entity-a)
    :entity-b (:id entity-b)
    :transform (->transform (position entity-a) 1 1 1)
    :color (:color entity-a)
    :end-pos (position entity-b)}))

(def connection->infected :entity-a)
(def connection->non-infected :entity-b)

;; in ms
(defn age [entity] (- (q/millis) (:spawn-time entity)))

(defn ->hsb
  [color]
  (apply
   q/color
   (cond
     (and (map? color) (every? #(contains? color %) [:h :s :b]))
     [(:h color) (:s color) (:b color)]
     (and (map? color) (every? #(contains? color %) [:h :s :v :a]))
     [(* (/ (:h color) 360) 255)
      (* 255 (/ (:s color) 100))
      (* 255 (/ (:v color) 100))
      (* 255 (:a color))]
     (and (map? color) (every? #(contains? color %) [:h :s :v]))
     [(* (/ (:h color) 360) 255)
      (* 255 (/ (:s color) 100))
      (* 255 (/ (:v color) 100))]
     (sequential? color) color
     (number? color) [color 255 255]
     :else [color])))

(defn shine
  [{:as entity :keys [shine shinyness]}]
  (if-not shinyness
    entity
    (let [shine (mod (+ shine (* *dt* shinyness)) 255)]
      (-> entity
          (assoc :shine shine)
          (update :color (fn [c] (q/color shine 255 255)))))))

(defn signum [x]
  (cond
    (<= x 0) -1
    (> x 0) 1))

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
          (fn [entities]
            (let [entities
                  (map (fn [{:as e :keys [lifetime]}]
                         (if lifetime (update e :lifetime - *dt*) e))
                       entities)
                  dead-entities
                  (into #{}
                        (filter #(some-> % :lifetime (<= 0)))
                        entities)
                  dead? (into #{} (map :id) dead-entities)
                  components-of-dead (into #{}
                                           (mapcat :components dead-entities))]
              (into []
                    (comp
                     ;; (map (fn [e]
                     ;;        (if (components-of-dead (:id e))
                     ;;          (assoc e :lifetime 10)
                     ;;          e)))
                     (remove (comp dead? :id))
                     (remove (comp components-of-dead :id)))
                    entities)))))

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

(defn update-conn-line [{:keys [connection-line? entity-b entity-a] :as entity} state]
  (if-not connection-line?
    entity
    (let [e-lut (entities-by-id state)
          dest (e-lut entity-b)
          source (e-lut entity-a)]
      (-> entity
          (assoc-in [:transform :pos] (position source))
          (assoc :end-pos (position dest) )
          (assoc :color (:color source))))))

(def draw-color (comp #(q/fill %) ->hsb))

(defmulti draw-entity :kind)

(defmethod draw-entity :circle [{:keys [transform]}]
  (let [[x y] (:pos transform)
        {:keys [width height scale]} transform]
    (q/ellipse x y (* scale width) (* scale height))))

(defmethod draw-entity :vine
  [{:keys [transform end-pos color]}]
  (let [[x y] (:pos transform)
        {:keys [_scale]} transform]
    (q/stroke-weight 2)
    (q/with-stroke (->hsb color)
      (q/line [x y] end-pos))
    (q/stroke-weight 1)))

(defmethod draw-entity :rect [{:keys [transform]}]
  (let [[x y] (:pos transform)
        {:keys [width height scale rotation]} transform]
    (q/with-translation [x y]
      (q/rotate rotation)
      (q/rect 0 0 (* width scale) (* height scale) 25))))
