;; extended lib.
;; file was getting too big
(ns ftlm.vehicles.art.extended
  (:require [quil.core :as q :include-macros true]
            [ftlm.vehicles.art.lib :as lib]
            [ftlm.vehicles.art.controls]))

(defn ->brownian-lump
  [{:keys [spread particle-size pos togethernes-threshold
           count colors]
    :or {colors [[0 255 255]]
         count 10
         particle-size 10
         spread 8
         togethernes-threshold (or spread (* 2 spread))}}]
  (let [lump (assoc (lib/->entity :lump)
                    :hidden? true
                    :lump? true
                    :position pos
                    :spread spread)]
    (into [lump]
          (map
           (fn []
             (let [spawn-pos
                   [(lib/normal-distr (first pos) spread)
                    (lib/normal-distr (second pos) spread)]]
               (->
                (merge
                 (lib/->entity :circle)
                 {:color (rand-nth colors)
                  :draggable? false
                  :kinetic-energy 0.2
                  :on-update
                  [(fn [e]
                     (let [threshold
                           togethernes-threshold
                           dist (lib/distance (lib/position e)
                                          pos)]
                       (if (< threshold dist)
                         (assoc (lib/orient-towards e pos)
                                :acceleration 2
                                :angular-acceleration 0)
                         e)))]
                  :particle? true
                  :transform
                  (assoc (lib/->transform spawn-pos
                                      particle-size
                                      particle-size
                                      1)
                         :rotation (lib/angle-between spawn-pos
                                                  pos))
                  :z-index 10})))))
          (range count))))

(defn ->oxygen
  [opts]
  (lib/flatten-components
    [(merge
       (lib/->odor-source
         (merge opts {:fragrances #{:oxygen}} (:odor opts)))
       {:components (->brownian-lump
                      (assoc opts
                        :colors
                          (into []
                                (repeatedly
                                  4
                                  (fn []
                                    {:h 178
                                     :s (lib/normal-distr 20 10)
                                     :v 255})))
                        :spread 20
                        :count 15
                        :particle-size 8
                        :togethernes-threshold 50))
        :draggable? false
        :oxygen? true})]))

(defn ->organic-matter
  [opts]
  (lib/flatten-components
    [(merge (lib/->odor-source
              (merge opts {:fragrances #{:organic-matter}} (:odor opts)))
            {:components (->brownian-lump opts)
             :draggable? false
             :food? true
             :organic-matter? true})]))

(defn ->temperature-bubble-1
  [{:as opts
    :keys [pos d temp max-temp low-color high-color
           hot-or-cold]}]
  [(merge
    (assoc
     (lib/->entity :circle)
     :transform (lib/->transform pos d d 1)
     :no-stroke? true
     :color
     (q/lerp-color
      (lib/->hsb low-color)
      (lib/->hsb high-color)
      (lib/normalize-value-1 0 max-temp temp))
     :temperature-bubble? true
     :hot-or-cold hot-or-cold
     :d d
     :temp temp
     :z-index -10
     :particle? true
     :draggable? true)
    opts)])

(defn ->temperature-bubble [opts]
  (fn [opts-1]
    (->temperature-bubble-1 (merge opts opts-1))))

(defn ->breath
  [initial-scale size speed]
  (let [mystate (atom {:speed speed :time 0})]
    {[:breath :play-with-speed]
     (lib/every-n-seconds
      speed
      (fn [_ _ _]
        (swap! mystate update
               :speed
               (constantly (lib/normal-distr speed (/ speed 2))))
        nil))
     [:breath :rotate]
     (lib/every-n-seconds
      speed
      (fn [e _ _]
        (update e
                :angular-acceleration
                +
                (* (/ speed 3)
                   (lib/normal-distr 0
                                     (/ (mod (:time @mystate)
                                             q/TWO-PI)))))))
     [:breath :scale]
     (fn [e _ _]
       (-> e
           (update-in
            [:transform :scale]
            (fn [_scale]
              (let [progress (/ (:time @mystate) 1)]
                (q/lerp
                 initial-scale
                 (* initial-scale size)
                 (+ 1 (q/sin (* q/PI progress)))))))))
     [:breath :time]
     (fn [_ _ _]
       (let [t (fn [{:as s :keys [speed]}]
                 (-> s
                     (update :time + (* speed lib/*dt*))))]
         (swap! mystate t))
       nil)}))

(defn ->plasma-balls
  [{:keys [from to color start-entity]
    :or {color {:a 0.8 :h 0 :s 100 :v 100}}}]
  (map-indexed
   (fn [_ _]
     (let [pos from]
       (->
        (merge
         (lib/->entity :circle)
         {:acceleration 150
          :color color
          ;; :kinetic-energy 0.1
          :lifetime (lib/normal-distr 5 5)
          :on-update-map
          {:kill (fn [e _ _]
                   (if (<= (lib/distance (lib/position e) to)
                           10)
                     (assoc e :lifetime 0)
                     e))
           :target (lib/every-n-seconds
                    1.5
                    (fn [e _ _]
                      (let [mag (lib/distance (lib/position e)
                                          to)]
                        (-> (lib/orient-towards e to)
                            (update :acceleration
                                    +
                                    (lib/normal-distr
                                     (* mag 3)
                                     (* mag 2)))))))}
          :particle? true
          :transform
          (lib/->transform (lib/position start-entity) 10 10 1)
          :z-index -1})
        (lib/orient-towards pos))))
   (range 1)))

(defn ->color-back-and-forth-zagged
  [duration high low]
  (let [s (atom {:time-since 0})]
    (fn [e _ _]
      (swap! s update :time-since + lib/*dt*)
      (let [progress (lib/normalize-value-1 0 duration (mod (:time-since @s) duration))]
        (assoc e :color (q/lerp-color high low progress))))))


(defn ->activation-burst
  [state-atom id]
  (fn [_ _ _]
    (let [s @state-atom
          e ((lib/entities-by-id s) id)]
      (when e
        (swap! state-atom update-in [:eid->entity id :activation] + 100)))
    nil))

(defn with-electrode-sensitivity
  [e]
  (assoc-in e
    [:on-late-update-map :electrode-sensitivity]
    (fn [e _s _k]
      (if-let [electrode-input (:electrode-input e)]
        (->
         e
         (dissoc :electrode-input)
         (update :activation + electrode-input))
        e))))

(defn assembling-multi-line
  [{:keys [source dest]}]
  (fn [e s _]
    (let [source-e ((lib/entities-by-id s) source)
          dest-e ((lib/entities-by-id s) dest)
          start-pos (lib/position source-e)
          end-pos (lib/position dest-e)
          update-pos
          (fn [e]
            (let [start-pos (update start-pos 1 (fn [v] (+ v (rand-nth (range -10 10 5)))))
                  end-pos (update end-pos 0 (fn [v] (+ v (rand-nth (range -10 10 5)))))]
              (assoc e
                     :vertices [start-pos [(first end-pos) (second start-pos)] end-pos])))
          e (assoc e
                   :hidden? (some :dragged? [source-e dest-e]))
          e (cond-> e
              (or (not= start-pos (get-in e [:vertices 0]))
                  (not= end-pos (get-in e [:vertices 2])))
              update-pos)]
      e)))

(defn rect-multi-line-vertices
  [source-e dest-e]
  (let [start-pos (lib/position source-e)
        end-pos (lib/position dest-e)
        start-pos (update
                    start-pos
                    1
                    (fn [v]
                      (+ v (rand-nth (range -20 20 5)))))
        end-pos (update end-pos
                        0
                        (fn [v]
                          (+ v
                             (rand-nth (range -20 20 5)))))]
    [start-pos [(first end-pos) (second start-pos)]
     end-pos]))

(defn rect-line-vertices-1
  [start-pos end-pos]
  (let [start-pos (update
                   start-pos
                   1
                   (fn [v]
                     (+ v (rand-nth (range -10 10 5)))))
        end-pos (update end-pos
                        0
                        (fn [v]
                          (+ v
                             (rand-nth (range -10 10 5)))))]
    [start-pos [(first end-pos) (second start-pos)]
     end-pos]))


(defn ->fade
  [speed]
  (fn [e s _]
    (update e
            :color
            (fn [c]
              (let [c (lib/->hsb c)]
                (lib/with-alpha c
                                (* (- 1 (* lib/*dt* speed))
                                   (q/alpha c))))))))

(defn from-right [amount]
  (- (q/width) amount))

(defn from-bottom [amount]
  (- (q/height) amount))
