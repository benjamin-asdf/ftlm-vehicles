(ns ftlm.vehicles.art.neuronal-area
  (:require
   [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
   [quil.core :as q :include-macros true]
   [ftlm.vehicles.art.extended :as elib]
   [ftlm.vehicles.art.controls :as controls :refer
    [versions]]
   [ftlm.vehicles.art.user-controls :as
    user-controls]
   [clojure.set :as set]
   [ftlm.vehicles.art.grid]
   [goog.style]
   [ftlm.vehicles.hdv]
   [ftlm.vehicles.audio :as audio]
   [ftlm.vehicles.assembly-calculus :as ac]))

(defn ->neurons
  [{:as opts
    :keys [spacing grid-width draw-i ->activations]}]
  (lib/->entity
   :neurons
   (merge
    {:color (:cyan controls/color-map)
     :draw-functions
     {:1 (fn [e]
           (let [neurons (.valueOf (->activations e))
                 i->pos (fn [i] ((e :i->pos) e i))
                 i->color
                 (if (:i->color e)
                   ((:i->color e) e)
                   (constantly (lib/->hsb (:color e))))]
             (q/with-stroke
               nil
               (doall
                (for [i neurons
                      :let [pos (i->pos i)]]
                  (q/with-fill
                    (i->color i)
                    (q/with-translation
                      pos
                      (if draw-i
                        (draw-i i)
                        (q/rect 0 0 15 15 3)))))))))}
     :i->pos (fn [{:keys [transform]} i]
               (let [[x y] (:pos transform)
                     coll (mod i grid-width)
                     row (quot i grid-width)
                     x (+ x (* coll spacing))
                     y (+ y (* row spacing))]
                 [x y]))
     :spacing spacing}
    opts)))

(defn ->neuronal-area-ac-1
  [opts]
  (->neurons
    (assoc opts
      :next-color (constantly (:cyan controls/color-map))
      :->activations
      (fn [e] (ac/read-activations (:ac-area e))))))

(defn ->neuronal-area-ac
  [{:as opts :keys [frequency]}]
  (assoc
   (->neuronal-area-ac-1 opts)
   :on-update-map
   {:update-neurons
    (lib/every-n-seconds
     (/ 1 frequency)
     (fn [e _s _]
       (if-not (-> e
                   :ac-area
                   :inhibition-model)
         e
         (-> e
             (update :ac-area ac/update-neuronal-area)
             (assoc :color ((:next-color e)))))))}))

(defn ->flash-lines
  [s indices {:as e :keys [color]} n-area]
  (lib/append-ents s
                   (for [i indices]
                     (assoc (elib/->flash-of-line
                             (lib/position e)
                             ((:i->pos n-area) n-area i))
                            :color color))))

(defn ->stimulus [color]
  (lib/->entity
   :circle
   {:stimulus? true
    :color color
    :draggable? true
    :transform (lib/->transform [100 100] 30 30 1)}))



;; -------------------------------------
;; Little happy sensoric field
;; ----------------------------------
;; a place, when you move a stimulus inside it,
;; we say that the system can percieve the stimulus
;; The model is a stimulus bag, a is set of currently percieved stimuli
;;
(defn ->sensoric-field
  [pos]
  (->
    (lib/->entity
      :rect
      {:color (lib/with-alpha (lib/->hsb controls/white) 0)
       :corner-r 5
       :draw-functions
         {:center (fn [e]
                    (q/with-fill (lib/->hsb (:stroke e))
                                 (q/with-translation
                                   (lib/position e)
                                   (q/ellipse 0 0 10 10))))
          :stimuli (fn [e]
                     (let [stimuli (take 5 (:stimuli e))]
                       (doall
                        (for [[idx {:keys [color]}]
                              (map-indexed
                               vector
                               (map (lib/entities-by-id
                                     (q/state))
                                    stimuli))]
                          (q/with-fill
                            (lib/->hsb color)
                            (q/with-stroke
                              (lib/->hsb color)
                              (q/with-translation
                                (lib/position e)
                                (q/ellipse
                                 (+ 20
                                    (* -1
                                       (/ (-> e
                                              :transform
                                              :width)
                                          2))
                                    (* idx 20))
                                 (* 0.8
                                    (/ (-> e
                                           :transform
                                           :height)
                                       2))
                                 10
                                 10))))))))}
       :on-excite
         {:1
            (fn [e s]
              (when (< (count
                         (filter
                           :sensoric-field-excite-shadow?
                           (lib/entities s)))
                       6)
                {:updated-state
                   (->
                     s
                     (lib/append-ents
                       (into
                         []
                         (repeatedly
                           3
                           #(merge
                              (lib/clone-entity e)
                              {:lifetime 1
                               :on-update-map
                                 {:dance
                                    (lib/every-n-seconds
                                      0.1
                                      (fn [e _ _]
                                        (update-in
                                          e
                                          [:transform
                                           :scale]
                                          +
                                          (rand-nth
                                            [-0.05
                                             0.05]))))}
                               :sensoric-field-excite-shadow?
                                 true
                               :stroke-weight 1})))))}))}
       :sensoric-field? true
       :stimuli #{}
       :stroke controls/white
       :stroke-weight 2
       :transform (lib/->transform pos 100 200 1)
       :z-index -10})
    (lib/live
      [:sensitivity
       (lib/every-n-seconds
         1
         (fn [e s _]
           (let [current (:stimuli e)
                 next-stimuli
                   (into #{}
                         (comp (filter :stimulus?)
                               (filter #(lib/point-inside?
                                          e
                                          (lib/position %)))
                               (map :id))
                         (lib/entities s))
                 new (set/difference next-stimuli current)]
             {:updated-state
                (cond-> s
                  :next (assoc-in [:eid->entity (:id e)
                                   :stimuli]
                          next-stimuli)
                  (seq new)
                    (lib/append-ents
                      (map (fn [id]
                             (let [color
                                     (:color
                                       ((lib/entities-by-id
                                          s)
                                         id))]
                               (merge
                                 (lib/clone-entity e)
                                 {:lifetime 10
                                  :on-update-map
                                    {:fade
                                       (lib/->fade-pulse-2
                                         2.0
                                         :stroke)}
                                  :stroke color
                                  :stroke-weight 2})))
                           new)))})))])))


;; Makes a neuron step when you tab it
;; Makes neuron time steps, when it is inside the sensory area
(defn ->touch-me-gate
  [pos
   ;; sensoric-excite
  ]
  (lib/->entity
    :circle
    {:clickable? true
     :z-index 10
     :color (lib/with-alpha (lib/->hsb controls/white) 0)
     :draggable? true
     :last-clicked nil
     :on-click-map
       {:1 (fn [e s _]
             (when-not (when-let [last-clicked
                                    (:last-clicked e)]
                         (< (- (:last-tick s) last-clicked)
                            200))
               {:updated-state
                  ((lib/update-update-functions-map-1
                     :on-excite)
                   s)}))
        :2 (fn [e s _]
             (when-not (when-let [last-clicked
                                    (:last-clicked e)]
                         (< (- (:last-tick s) last-clicked)
                            200))
               {:updated-state
                  (-> s
                      (update-in
                        [:eid->entity (:id e)]
                        (fn [e]
                          (-> e
                              (assoc :last-clicked
                                       (:last-tick s))
                              (assoc :color controls/white)
                              (assoc-in
                                [:on-update-map :flash]
                                (lib/for-n-seconds
                                  1.0
                                  (lib/->fade-pulse-2 0.1)
                                  (fn [e _ _]
                                    (assoc e
                                      :color
                                        (lib/with-alpha
                                          (lib/->hsb
                                            controls/white)
                                          0)))))))))}))}
     ;; :on-update-map
     ;; {:flashing-stimuli
     ;;  (lib/every-n-seconds
     ;;   5.0
     ;;   (fn [e s _]
     ;;     (when-let [st (first
     ;;                  (shuffle
     ;;                   (sequence
     ;;                    (comp (keep :stimuli)
     ;;                          (mapcat identity)
     ;;                          (map
     ;;                          (lib/entities-by-id
     ;;                                s)))
     ;;                    (lib/entities s))))]
     ;;       (-> e
     ;;           (assoc :color (lib/with-alpha (:color
     ;;           st) 0))
     ;;           (assoc-in
     ;;            [:on-update-map :flash]
     ;;            (lib/for-n-seconds
     ;;             2.0
     ;;             (lib/->fade-pulse-2 2.0)
     ;;             (fn [e _ _]
     ;;               (assoc e
     ;;                      :color
     ;;                      (lib/with-alpha
     ;;                        (lib/->hsb
     ;;                         controls/white)
     ;;                        0))))))
     ;;       e)))}
     :stroke controls/white
     :stroke-weight 2
     :transform (lib/->transform pos 30 30 1)}))

(defn shiny-projection-1
  [state indices id-area color]
  (let [n-area ((lib/entities-by-id @lib/the-state) id-area)
        i->pos (fn [i] ((n-area :i->pos) n-area i))]
    (->
     state
     (lib/append-ents
      [(lib/->entity
        :blinking-neurons
        {:color color
         :draw-functions
         {:1 (fn [ble]
               (q/stroke-weight 2)
               (q/with-fill
                 nil
                 (q/with-stroke
                   (lib/->hsb (:color ble))
                   (doall
                    (for [i (:indices ble)
                          :let [pos (i->pos i)]]
                      (q/with-translation
                        pos
                        (q/rect 0 0 15 15 3)))))))}
         :indices indices
         :lifetime 10.5
         :on-update-map {:fade (lib/->fade-pulse-2
                                3.0)}})]))))


(defn ->shiny-projection
  [->indices id-area]
  (fn [e state _]
    (let [indices (->indices e state)]
      {:updated-state
       (shiny-projection-1 state indices id-area (:color e))})))

(defn ->flash-some-connections
  [n-area]
  (fn [_ s _]
    (let [e ((lib/entities-by-id s) (:id n-area))]
      {:updated-state
       (let [i->pos (fn [i] ((:i->pos e) e i))]
         (-> s
             (lib/append-ents
              (let [neurons (partition
                             2
                             (into #{}
                                   (repeatedly
                                    3
                                    #(rand-int
                                      (:n-neurons
                                       n-area)))))]
                (for [[i j] neurons]
                  (assoc (elib/->flash-of-line (i->pos i)
                                               (i->pos
                                                j))
                         :color controls/white))))))})))


(defn ->n-area-knob
  [n-area opts]
  (->
   (lib/->entity
    :circle
    (merge
     {:clickable? true
      :color controls/white
      :on-double-click-map
      {:remember-neurons
       (fn [knob s _]
         (let [n-area ((lib/entities-by-id s)
                       (:id n-area))
               currently-active (ac/read-activations
                                 (:ac-area n-area))]
           {:updated-state
            (-> s
                (assoc-in [:eid->entity (:id n-area)
                           :lol]
                          :lol)
                (update-in
                 [:eid->entity (:id n-area)
                  :remembered-neurons]
                 (fnil merge {})
                 (into {}
                       (map (juxt
                             identity
                             (constantly
                              (lib/->hsb
                               controls/white))))
                       currently-active))
                (lib/append-ents
                 (repeatedly
                  10
                  #(assoc (lib/clone-entity
                           (assoc-in knob
                                     [:transform :scale]
                                     0.4))
                          :particle? true
                          :kinetic-energy 1.5
                          :lifetime 4.5))))}))}
      :stroke (lib/with-alpha (lib/->hsb controls/white) 0)
      :stroke-weight 6
      :transform (lib/->transform [150 250] 15 15 0.5)}
     opts))
   (lib/live [:flash (lib/->fade-pulse-2 10.0 :stroke)])
   (lib/live
    [:decide-to-flash
     (lib/every-n-seconds
      10.0
      (fn [e s _]
        (let [ne ((lib/entities-by-id s) (:id n-area))]
          (-> e
              (assoc-in [:on-update-map :flash]
                        (lib/->fade-pulse-2
                         (if (< 0
                                (ac/count-activation (ac/read-activations (:ac-area ne))))
                           5.0
                           10.0)
                         :stroke))))))])))

(defn eye-ball
  [{:keys [pos]}]
  (lib/->entity
    :circle
    {:clickable? true
     :color (lib/with-alpha (lib/->hsb controls/white) 0)
     :draggable? true
     :stroke controls/white
     :stroke-weight 2
     :transform (lib/->transform pos 25 25 1)
     :z-index 10}))

(defn stimuli-at-eye-ball
  [eye stimuli state]
  (let [pos (lib/position ((lib/entities-by-id state)
                            (:id eye)))]
    (for [e (map (lib/entities-by-id state) stimuli)]
      (when (< (lib/distance (lib/position e) pos) 25) e))))


;; ---
;; perspective-flower
;; Conceptually a landscape modulator
;; p-lines (perspective lines) are a hypothetical mechanism that dynamically shapes
;; the memetic landscape.
;; i.e. you say what kinds of attractor states the system will fall into.
;; This is conceptually very similar to 'attention' in machine learning.
;;
;; Something similar might be an implementation of 'commitment' or 'inner muscles', too.
;; Basically say dynamically what kind of stuff should be acitve,
;; by composing this temporarily, that is one way to make thought-sequence kinda things.
;;
;;
;;

(defn perspective-flower
  [{:as opts :keys [count i->fill n-neurons p-probability]}]
  (let [p-lines
        (into []
              (for [i (range count)]
                (ac/->rand-projection
                 n-neurons
                 p-probability)))]
    (merge
     (elib/->clock-flower opts)
     {:active-p-lines #{} :p-lines p-lines})))

(defn perspective-inputs [{:keys [active-p-lines p-lines]}]
  (vals (select-keys p-lines active-p-lines)))

(defn reset-n-area [s id]
  (-> s (update-in [:eid->entity id :ac-area] ac/set-input #js[])))


;;
;; s-listener-wires
;;
;;              s-listener-wires
;;
;;                                stability-detector
;;
;;     +--------+              +-----+            +---+
;;     |        |              |     |            |   |
;;     |  --O---+--------------+ off | ...  off   | O |
;;     |        |              |     |            |   |
;;     |  ---X--+--------------+ on  | ...  on    | X |
;;     |        |              |     |            |   |
;;     |--X----X+--------------+ on  | ...  on    | X |
;;     |        |              |     |            |   |
;;     +--------+              +-----+            |   |
;;         neurons            ... n-wires         +---+
;;
;;                               t0,       t1,     t2
;;
;; You make random wires through the neurons =s-listener-wires=.
;; At each time step, you can look at which =s-listener-wires= are on. The set of hot wires is a proxy of what ensembles are active in the neurons.
;;
;; If the ensembles move around, your wires will certainly change.
;;
;; Make =stability-detector= a population of neurons, make it postsynaptic of your s-listener-wires.
;; Use biochemistry or equivalent to say that the detector neurons need say 3 /on/ inputs in a row to fire.
;;
;; Then, your stability detector is a population of neurons that represents the current /amount of stable activity/ (of ensembles) in the neurons.
;;
;; (In absolute terms).
;;
;; This can also be used to simply visualize what ensembles are /on/ in the neurons.

(defn ->stability-detector
  [opts n-area]
  (let [wires (ac/->one-neuron-per-wire (:n-neurons n-area)
                                        (:n-wires opts))
        e (->neurons
            (merge
              opts
              {:->activations (fn [e]
                                (ac/read-activations
                                  (:neuron-model e))
                                ;; (range 6)
                              )
               :activation->beep
                 (into {}
                       (map (juxt identity
                                  (fn [_]
                                    (let [base 50]
                                      (audio/->beep
                                        {:durr 200
                                         :hz (* 2
                                                (rand-int
                                                  10)
                                                base)
                                         :pan (- (rand 2) 1)
                                         :volume 0.2})))))
                       (range (:n-wires opts)))
               :neuron-model {:activations #js []
                              :inputs (ac/->neurons
                                        (:n-wires opts))
                              :n-neurons (:n-wires opts)
                              :threshold 2
                              :tick-window 4
                              :wires wires}}))
        e (lib/live
            e
            (let [playing (atom #{})]
              [:make-sounds
               (lib/every-n-seconds
                 0.1
                 (fn [e _ _]
                   (let [activations (ac/read-activations
                                       (:neuron-model e))
                         activations (remove @playing
                                       activations)
                         _ (swap! playing into activations)]
                     (run! (fn [i]
                             (-> (audio/beep!
                                   ((e :activation->beep)
                                     i))
                                 (.then #(swap! playing disj
                                           i))))
                           activations))))]))
        id (:id e)]
    {:e e
     :update (fn [s]
               (update-in
                 s
                 [:eid->entity id :neuron-model]
                 ac/fixed-window-accumulate-and-fire
                 (ac/synaptic-input
                   wires
                   (ac/read-activations
                     (:ac-area ((lib/entities-by-id s)
                                (:id n-area)))))))}))

(def update-neuron-tick-map-functions (lib/update-update-functions-map-1 :on-neuron-tick-map))

(defn ->input-model []

  )
