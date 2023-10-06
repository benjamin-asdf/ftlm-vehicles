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
  (update state :eid->entity (fn [s] (update-vals s f))))

(defn append-ents [state ents]
  (-> state
      (update :eid->entity merge (into {} (map (juxt :id identity)) ents))))

(defn update--entities
  [state f]
  (update state :eid->entity (fn [s] (into {} (f s)))))

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

(defn generate-palette [base num-colors]
  (into [] (repeatedly num-colors #(mod (normal-distr base 50) 360))))

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

(defmethod draw-entity :rect [{:keys [transform corner-r]}]
  (let [[x y] (:pos transform)
        {:keys [width height scale rotation]} transform]
    (q/with-translation [x y]
      (q/rotate rotation)
      (q/rect 0 0 (* width scale) (* height scale) corner-r))))

(defn window-dimensions []
  (let [w (.-innerWidth js/window)
        h (.-innerHeight js/window)]
    [w h]))

(defn rand-on-canvas [] [(rand-int (q/width)) (rand-int (q/height))])
(defn rand-on-canvas-gauss
  [distr]
  [(normal-distr (/ (q/width) 2) (* distr (/ (q/width) 2)))
   (normal-distr (/ (q/height) 2) (* distr (/ (q/height) 2)))])

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

(defn ->sensor
  [anchor modality]
  (assoc (->entity :circle)
    :transform (->transform [0 0] 20 20 1)
    :anchor anchor
    :modality modality
    :sensor? true
    :color (q/color 40 96 255 255)))

(defn ->motor
  [anchor]
  (merge (->entity :rect)
         {:anchor anchor
          :color (q/color 40 96 255 255)
          :motor? true
          :transform (->transform [0 0] 20 35 1)}))

(defn normalize-value-1
  [min max value]
  (let [old-min min
        old-max max
        new-min 0
        new-max 1]
    (+ new-min (* (/ (- value old-min) (- old-max old-min)) (- new-max new-min)))))

(defn relative-position [parent ent]
  (let [{:keys [width height]} (transform parent)
        m (anchor->trans-matrix (:anchor ent))]
    (v* m [(/ width 2) (/ height 2)])))

(defn rotate-point [rotation [x y]]
  [(+ (* x (Math/cos rotation)) (* -1 y (Math/sin rotation)))
   (+ (* x (Math/sin rotation)) (* y (Math/cos rotation)))])

(defn translate-point [x y dx dy]
  [(+ x dx) (+ y dy)])

(defn scale-point [[x y] scale]
  [(x * scale) (y * scale)])

(defn friction-1 [velocity]
  (* velocity 0.9))

(defn friction [e]
  (-> e
      (update :velocity friction-1)
      (update :acceleration friction-1)
      (update :angular-velocity friction-1)
      (update :angular-acceleration friction-1)))

(defn track-components
  [state]
  (let [parent-by-id (into {}
                           (mapcat (fn [ent]
                                     (map (juxt identity (constantly ent))
                                          (:components ent)))
                                   (filter :components (entities state))))]
    (-> state
        (update-ents
         (fn [{:as ent :keys [id]}]
           (if-let [parent (parent-by-id id)]
             (let [relative-position (relative-position parent ent)
                   parent-rotation (-> parent
                                       :transform
                                       :rotation)
                   scale (-> parent :transform :scale)]
               (-> ent
                   (assoc-in [:transform :pos]
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
                             (-> parent :transform :rotation))
                   (assoc-in [:transform :scale] scale)))
             ent))))))

(defn draw-entities
  [state]
  (doseq [{:as entity :keys [color hidden?]} (entities state)]
    (when-not hidden? (draw-color color) (draw-entity entity))))
