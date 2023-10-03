(ns ftlm.vehicles.art.lib
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]))

(def ^:dynamic *dt* nil)

(defn normal-distr [mean std-deviation]
  (+ mean (* std-deviation (q/random-gaussian))))

(defn controls []
  (q/state :controls))

(let [c (atom 0)
      ->eid #(swap! c inc)]
  (defn ->entity [kind] {:id (->eid) :kind kind :spawn-time (q/millis)}))

(defn ->transform [pos width height scale]
  {:pos pos :width width :height height :scale scale})

(def entities (comp vals :eid->entity))
(def entities-by-id :eid->entity)

(defn destroy [state eid]
  (update state :eid->entity dissoc eid))

(defn update-ents [state f]
  (update state
          :eid->entity
          (fn [s]
            (into (sorted-map) (update-vals s f)))))

(defn append-ents [state ents]
  (-> state
      (update :eid->entity (fnil (fn [s] (into s (map (juxt :id identity)) ents)) (sorted-map)))))

(defn update--entities
  [state f]
  (update state :eid->entity (fn [s] (into (sorted-map) (f s)))))

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
    :end-pos (position entity-b)
    :children [(:id entity-b)
               (:id entity-b)]}))

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

(defn update-lifetime-1
  [{:as entity :keys [lifetime]}]
  (if-not lifetime entity (update entity :lifetime - *dt*)))

(defn kill-from-lifetime
  [{:as entity :keys [lifetime]}]
  (if (some-> lifetime (< 0)) (assoc entity :kill? true) entity))

(def update-lifetime (comp kill-from-lifetime update-lifetime-1))

(defn kill-components
  [state]
  (let [kill?
        (into #{}
                (comp
                 (filter :kill?)
                 (mapcat :components))
                (entities state))]
    (update-ents state
                 (fn [{:as e :keys [id]}]
                   (if (kill? id)
                     (assoc e :kill? true)
                     e)))))

(defn kill-connections
  [state]
  (let [kill? (into #{}
                    (comp (filter :connection-line?)
                          (filter
                           (fn [{:keys [entity-b entity-a]}]
                             (or (:kill? ((entities-by-id state) entity-a))
                                 (:kill? ((entities-by-id state) entity-b)))))
                          (map :id))
                    (entities state))]
    (update-ents state
                 (fn [{:as e :keys [id]}]
                   (if (kill? id) (assoc e :kill? true) e)))))

(defn kill-entities-1
  [state]
  (update--entities state (fn [ents] (remove (comp :kill? val) ents))))

(def kill-entities (comp kill-entities-1 kill-connections kill-components))

(defn update-conn-line
  [{:as entity :keys [connection-line? entity-b entity-a]} state]
  (if-not connection-line?
    entity
    (let [e-lut (entities-by-id state)
          dest (e-lut entity-b)
          source (e-lut entity-a)]
      (-> entity
          (assoc-in [:transform :pos] (position source))
          (assoc :end-pos (position dest))
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
