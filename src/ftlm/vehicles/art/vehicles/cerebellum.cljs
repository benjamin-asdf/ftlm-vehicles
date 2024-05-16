
(ns ftlm.vehicles.art.vehicles.cerebellum
  (:require
    [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
    [ftlm.vehicles.art :as art]
    [quil.core :as q :include-macros true]
    [quil.middleware :as m]
    [ftlm.vehicles.art.extended :as elib]
    [ftlm.vehicles.art.controls :as controls :refer
     [versions]]
    [ftlm.vehicles.art.user-controls :as user-controls]
    [ftlm.vehicles.art.grid]
    [goog.style]
    [ftlm.vehicles.hdv]
    [tech.v3.datatype.argops :as argops]
    [tech.v3.datatype.functional :as dtype-fn]
    [ftlm.vehicles.assembly-calculus :as ac]
    [ftlm.vehicles.art.neuronal-area :as na]
    [ftlm.vehicles.art.vehicles.cerebellum.purkinje :as pc]
    ["mathjs" :as mathjs]))

;;
;; https://www.sciencedirect.com/science/article/abs/pii/S0079612308609711?via%3Dihub
;;
;; Based on Braitenberg about the arrangment of Cerebellum.
;;
;;

(defn env [_state] {})

(defn draw-state
  [state]
  (q/background (lib/->hsb (-> state :controls :background-color)))
  (q/stroke-weight 1)
  (q/stroke 0.3)
  ;; (lib/grid)
  (lib/draw-entities state))

(defn update-entity
  [entity state env]
  (-> entity
      (lib/update-body state)
      lib/brownian-motion
      lib/friction
      lib/dart-distants-to-middle
      lib/move-dragged
      lib/update-rotation
      lib/update-position
      (lib/update-sensors env)
      lib/activation-decay
      lib/activation-shine
      lib/shine
      lib/update-lifetime))

(defn update-state-inner
  [state]
  ;; state
  (let [current-tick (q/millis)
        state (update state
                      :controls
                      merge
                      (user-controls/controls))
        dt (* (:time-speed (lib/controls))
              (/ (- current-tick (:last-tick state))
                 1000.0))
        state (binding [*dt* dt]
                (-> state
                    (assoc :last-tick current-tick)
                    lib/apply-update-events
                    lib/update-update-functions
                    lib/update-state-update-functions
                    lib/apply-events
                    ;; (lib/apply-events (:event-q state))
                    (lib/update-ents
                     #(update-entity % state (env state)))
                    lib/update-late-update-map
                    lib/transduce-signals
                    lib/track-components
                    lib/track-conn-lines
                    lib/ray-source-collision-burst
                    lib/kill-entities))]
    state))

(defn update-state
  [_]
  (let [state @lib/the-state
        state (update-state-inner state)]
    (reset! lib/the-state state)
    state))

(def other-side {:left :right :right :left})

(defn make-motor-movement
  [e dir]
  (println "make-motor-movement" dir)
  (-> e
      (update :flashing-fins conj dir)
      (update
       :angular-velocity
       (constantly
        (get {:left 5 :right -1} dir)))
      (assoc-in [:on-update-map :scale]
                (elib/->tiny-breath
                 {:speed 2 :start 1 :stop 2.0}))
      (assoc-in [:on-update-map :stop-scale]
                (lib/every-n-seconds
                 3
                 (fn [e s k]
                   (-> e
                       (update :on-update-map dissoc
                               :scale :stop-scale)
                       (assoc :flashing-fins #{})))))))


(defn ->fish
  []
  (->
   (lib/->entity
    :fish
    {:color (:woodsmoke controls/color-map)
     :color-left (lib/->hsb (:orange controls/color-map))
     :color-right (lib/->hsb (:orange controls/color-map))
     :draggable? true
     :draw-functions
     {:1 (fn [e]
           (q/stroke-weight (:stroke-weight e))
           (q/with-translation
             (lib/position e)
             (q/with-fill
               (lib/->hsb (:color e))
               (q/with-rotation
                 [(lib/rotation e)]
                 (let [{:keys [scale width height]}
                       (:transform e)
                       [w h] [(* scale width)
                              (* scale height)]
                       [x1 y1] [0 (- (/ h 2))]
                       [x2 y2] [(- (/ w 2)) (+ (/ h 2))]
                       [x3 y3] [(+ (/ w 2))
                                (+ (/ h 2))]]
                   (q/with-fill
                     (lib/->hsb (if (-> e
                                        :flashing-fins
                                        :left)
                                  (:heliotrope
                                   controls/color-map)
                                  (:color e)))
                     (q/with-translation
                       [15 0]
                       (q/with-rotation [q/HALF-PI]
                         (q/triangle
                          x1
                          y1
                          x2
                          y2
                          x3
                          y3))))
                   (q/with-fill
                     (lib/->hsb (if (-> e
                                        :flashing-fins
                                        :left)
                                  (:heliotrope
                                   controls/color-map)
                                  (:color e)))
                     (q/with-translation
                       [-15 0]
                       (q/with-rotation [(- q/HALF-PI)]
                         (q/triangle
                          x1
                          y1
                          x2
                          y2
                          x3
                          y3))))
                   (q/with-translation
                     [0 -40]
                     (q/with-rotation
                       [q/TWO-PI]
                       (q/triangle x1 y1 x2 y2 x3 y3))))
                 (q/stroke-weight (:stroke-weight e))
                 (q/with-stroke (:stroke e)
                   (q/ellipse
                    0
                    0
                    (-> e
                        :transform
                        :width)
                    (-> e
                        :transform
                        :height)))))))}
     :flashing-fins #{}
     :stroke (lib/->hsb (:orange controls/color-map))
     :stroke-weight 4
     :transform
     (lib/->transform (lib/mid-point) 80 100 1)})
   (lib/live [:fade (lib/->fade-pulse-2 10 :stroke)])))

(defn water
  [e]
  (lib/live e
            [:water
             (lib/every-n-seconds
              1
              (fn [e]
                (update
                 e
                 :angular-acceleration
                 (fnil + 0)
                 (lib/normal-distr
                  0
                  (get (lib/controls)
                       :water-force
                       0.02)))))]))

(defn random-motor-move [e]
  (lib/live e [:motor-move (lib/every-n-seconds 4 (fn [e] (make-motor-movement e (rand-nth [:left :right]))))]))

(defn ->vestibular-neurons
  [fish side]
  (let [e (na/->neurons
            {:->activations (fn [e]
                              (-> e
                                  :neurons
                                  ac/read-activations))
             :grid-width 5
             :n-neurons 50
             :neurons {:activations #js []}
             :spacing 15
             :transform (lib/->transform
                         [(({:left - :right +} side)
                           (first (lib/position fish))
                           200) 400]
                         10
                         10
                         1)
             :side side})]
    {:e e
     :update
       (fn [s]
         (let [rotation (lib/rotation ((lib/entities-by-id s) (:id fish)))
               rotation (/ (lib/normalize-angle rotation) q/TWO-PI)
               rotation
                 (if (= :left side) (- 1 rotation) rotation)
               active-part (/ rotation 0.5)
               active (if-not (< 0 active-part 1.0)
                        0
                        (* rotation (:n-neurons e)))]
           (assoc-in s
             [:eid->entity (:id e) :neurons :activations]
             (mathjs/range 0 active))))}))

(defn initiate-stability-move
  [fish state _]
  (if (:is-moving state)
    state
    (let [vestibular (into []
                           (comp (map :e)
                                 (map :id)
                                 (map (lib/entities-by-id
                                       state)))
                           (vals (:vestibular-nuclei fish)))
          side->neuron-count
          (into {}
                (map (juxt :side
                           (comp mathjs/count
                                 (fn [e]
                                   ((:->activations e)
                                    e)))))
                vestibular)]
      (let [[side neuron-count]
            (last (sort-by val side->neuron-count))]
        (if (< 10 neuron-count)
          ;; 1. start moving the muscles
          ;; 2. send a signal to the purkinje to start an
          ;; alarm clock, corresponding to the neuron
          ;; count
          ;; 3. send some kind of readiness or make some
          ;;    kind of state, that says ones the alarm
          ;;    is over, the equivalent motor vigor is to
          ;;    be applied on the other side
          ;; 4. actually, if you are *now* in an error
          ;; state, you can train the purkinjes to not
          ;; listen to you as much (?)
          ;; - if you undershooted, you need to allocate
          ;; more purkinje next time
          ;; - if you overshooted, you need to allocate
          ;; less purkinje next time.
          ;;   (and this needs to be on 1 side of the
          ;;   purkinje segment)
          ;;
          ;;
          {:updated-state
           (-> state
               (assoc :is-moving true)
               (update-in
                [:eid->entity (:purkinje-cells state)]
                pc/purkinje-alarm-clock
                neuron-count
                (fn [s]
                  (-> s
                      (update-in [:eid->entity (:id fish)]
                                 make-motor-movement
                                 (other-side side))
                      (dissoc :is-moving))))
               (update-in [:eid->entity (:id fish)]
                          make-motor-movement
                          side))}
          {:updated-state state})))))

(defmethod lib/setup-version :cerebellum1
  [state]
  (-> state
      (lib/append-ents [(-> (->fish)
                            random-motor-move)])))

(defmethod lib/setup-version :cerebellum2
  [state]
  (let [fish (-> (->fish)
                 water)
        vestibular {:left (->vestibular-neurons fish :left)
                    :right (->vestibular-neurons fish
                                                 :right)}
        fish (-> (assoc fish :vestibular-nuclei vestibular)
                 (lib/live [:stability-move
                            (lib/every-n-seconds
                              5
                              initiate-stability-move)]))
        purkinje-cells
          (pc/->single-row-purkinjes
            {:->activations (fn [e]
                              (-> e
                                  :neurons
                                  ac/read-activations))
             :draw-i (fn [_ active?]
                       (q/with-fill
                         (if active?
                           (lib/->hsb (:cyan
                                        controls/color-map))
                           (lib/->hsb [0 0 0]))
                         (q/rect 0 0 4 30 3)))
             :grid-width 100
             :neurons {:activations (mathjs/matrix
                                      (mathjs/range 0 50))
                       :n-neurons 50}
             :spacing 10
             :transform (lib/->transform
                          [50 (elib/from-bottom 200)]
                          50
                          50
                          1)})]
    (-> state
        (assoc :purkinje-cells (:id purkinje-cells))
        (assoc :on-update-map
                 {:vestibular (fn [s _]
                                (reduce (fn [s op] (op s))
                                  s
                                  (map :update
                                    (vals vestibular))))})
        (lib/append-ents (map :e (vals vestibular)))
        (lib/append-ents [fish purkinje-cells]))))


(defn setup
  [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (q/background (lib/->hsb (-> controls
                               :background-color)))
  (let [state {:controls controls :on-update []}
        state (-> state lib/setup-version)]
    (reset! lib/the-state state)))

(defn sketch
  [host {:keys [width height]} controls]
  (let [[screen-width screen-height] (lib/window-dimensions)
        _width (cond (= width "max") screen-width width width :else screen-width)
        _height (cond (= height "max") screen-height height height :else screen-height)
        width 1000
        height 800]
    (q/sketch :host host
              :size [width height]
              :setup (partial setup controls)
              :update update-state
              :draw draw-state
              :features [:keep-on-top]
              :middleware [m/fun-mode]
              :mouse-pressed (comp #(reset! lib/the-state %) lib/mouse-pressed)
              :mouse-released (comp #(reset! lib/the-state %) lib/mouse-released)
              :mouse-wheel (comp #(reset! lib/the-state %) lib/mouse-wheel)
              :frame-rate 30)))

(defonce restart-fn (atom nil))
(defmethod art/view "cerebellum"
  [{:as opts :keys [place version]}]
  (let [f (fn []
            (let [controls
                  (merge
                   (controls/default-versions "cerebellum")
                   (get-in versions ["cerebellum" version])
                   @user-controls/!app)]
              (sketch place opts controls)))]
    (reset! restart-fn f)
    (f)))

(defmethod user-controls/action-button ::restart
  [_]
  (some-> @restart-fn (apply nil)))


(comment
  (swap! lib/the-state update-in [:eid->entity 3 :neurons] pc/allocate-segment 20 :left)
  (swap! lib/the-state update-in [:eid->entity 3 :neurons] pc/allocate-segment 20 :right))
