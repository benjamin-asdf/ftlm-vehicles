(ns ftlm.vehicles.art.vehicles.develop
  (:require
   [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
   [ftlm.vehicles.art :as art]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [ftlm.vehicles.art.extended :as elib]
   [ftlm.vehicles.art.controls :as controls :refer
    [versions]]
   [ftlm.vehicles.art.user-controls :as
    user-controls]
   [ftlm.vehicles.art.grid]
   [goog.style]
   [ftlm.vehicles.hdv]
   [tech.v3.datatype.argops :as argops]
   [tech.v3.datatype.functional :as dtype-fn]
   [ftlm.vehicles.assembly-calculus :as ac]
   [ftlm.vehicles.art.neuronal-area :as na]))

(defn rand-cap-k-threshold-device
  [numbers]
  (fn [{:as state :keys [synaptic-input]}]
    (assoc state
      :activations (ac/cap-k (rand-nth numbers)
                             synaptic-input))))

(defn stimuli-inputs
  []
  (fn [s]
      ;; (let [stimuli []]
      ;;   (map stimulus->recptive-field stimuli))
  ))

(defn ->visual-field
  []
  (-> (lib/->entity
        :rect
        {:color (lib/with-alpha (lib/->hsb controls/white)
                                0)
         :corner-r 5
         :stroke (lib/->hsb controls/white)
         :stroke-weight 1
         :transform (lib/->transform [631 400] 100 150 1)})
      ;; (lib/live [:fade (lib/->fade-pulse-2 6.0
      ;; :stroke)])
  ))


(defn ->eye-movement
  [{:keys [n-neurons stimuli sensoric-field n-area]}]
  ;; make a few 'motor neurons' that 'pull' the
  ;; stimuliw with some force
  (let [effector-projections
          (into {}
                (map (juxt :id
                           (fn [_]
                             (ac/->projection
                               n-neurons
                               (fn [i]
                                 (when (< 15 (mod i 20))
                                   0.05)))))
                  stimuli))]
    {:shiny-projection
     (fn [s]
       (na/shiny-projection-1
        s
        (.valueOf
         (rand-nth
          (into [] (map second effector-projections))))
        (:id n-area)
        (lib/->hsb (:red controls/color-map))))
     :effector-projections effector-projections
     :update
     (lib/every-n-seconds
      0.2
      (fn [s]
        (let [sensoric-field ((lib/entities-by-id s)
                              (:id sensoric-field))
              activations (ac/read-activations
                           (:ac-area
                            ((lib/entities-by-id s)
                             (:id n-area))))]
          (reduce
           (fn [s [stimulus value]]
             ;; if some high value then flash
             (let
                 [s (if-not (< 0 value)
                      s
                      (lib/append-ents
                       s
                       (let [stimulus-e
                             ((lib/entities-by-id s)
                              stimulus)]
                         [(->
                           (lib/->entity
                            :multi-line
                            {:color (:color stimulus-e)
                             :lifetime 1
                             :stroke-weight 1
                             :transform
                             (lib/->transform
                              (lib/position
                               sensoric-field)
                              1
                              1
                              1)
                             :vertices
                             [(lib/position
                               sensoric-field)
                              (lib/position
                               stimulus-e)]
                             :z-index -2})
                           (lib/live
                            [:track-pos
                             (fn [e s _]
                               (let
                                   [stimulus-e
                                    ((lib/entities-by-id
                                      s)
                                     stimulus)]
                                   (assoc e
                                          :vertices
                                          [(lib/position
                                            sensoric-field)
                                           (lib/position
                                            stimulus-e)])))])
                           (lib/live [:fade
                                      (elib/->fade
                                       1)]))])))]
                 (-> s
                     (update-in [:eid->entity stimulus]
                                lib/orient-towards
                                (lib/position
                                 sensoric-field))
                     (update-in [:eid->entity stimulus
                                 :acceleration]
                                +
                                (* value 120)))))
           s
           (for [[stimulus proj] effector-projections]
             [stimulus
              (/ (ac/count-intersection proj
                                        activations)
                 (ac/count-projection proj))])))))}))

(defn ->perspective-flower
  [{:as opts :keys [n-neurons]}]
  (let [count 4
        p-lines (into []
                      (repeatedly
                       count
                       (fn []
                         (ac/->projection
                          n-neurons
                          (fn [i]
                            (when (< 5 (mod i 20) 10)
                              0.02))))))]
    (->
      (elib/->clock-flower
        {:active-p-lines #{}
         :color (lib/->hsb (:navajo-white
                             controls/color-map))
         :count count
         :i->fill (fn [e i]
                    (when ((:active-p-lines e) i)
                      (lib/->hsb (:navajo-white
                                   controls/color-map))))
         :n-neurons n-neurons
         :p-lines p-lines
         :p-probability 0.01
         :pos [180 600]
         :radius 60})
      (lib/live

 [:p-rand
         (lib/every-n-seconds
           ;; p-rhythm
           1
           (fn [e s _]
             (update e
                     :active-p-lines
                     (fn [curr]
                       (into #{}
                             (take
                               (rand-int (:count e))
                               (shuffle
                                 (into
                                   curr
                                   (repeatedly
                                     3
                                     #(rand-int
                                        (:count
                                         e)))))))))))]))))


(defmethod lib/setup-version :develop
  [state]
  (let
      [controls (:controls state)
       n-neurons (:n-neurons controls)
       n-area (na/->neuronal-area-ac-1
               {:grid-width 20
                :n-neurons n-neurons
                :spacing 15
                :transform
                (lib/->transform [50 50] 20 20 1)})
       n-area
       (->
        n-area
        (assoc
         :ac-area
         {:activations #js []
          :attenuation-decay 0.1
          :attenuation-malus-factor 0

          :density 0.1
          :excitability-decay 0.1
          :excitability-growth 1.8

          :inhibition-model
          (comp ac/intrinsic-firing-rate
                ac/neuron-skip-inhibition
                (rand-cap-k-threshold-device
                 [25 50])
                ac/intrinsic-excitability
                ac/attenuation)
          :intrinsic-firing-rate 0.01
          :n-neurons n-neurons
          :plasticity 0.1
          :plasticity-model ac/binary-hebbian-plasticity
          :skip-rate 0.2
          :weights (ac/->random-directed-graph n-neurons 0.1)})

        (assoc-in
         [:on-update-map :normalize-weights]
         (lib/every-n-seconds
          10
          (fn [e _s _]
            (update e
                    :ac-area
                    ac/prune-synapses-fixed-density)))))
       id-area (:id n-area)
       stimuli
       (into
        []
        (map (fn [color]
               (->
                (na/->stimulus (color controls/color-map))
                (lib/put (lib/rand-on-canvas-gauss 0.2))
                (lib/live
                 [:breath
                  (elib/->tiny-breath
                   {:speed 1 :start 1.0 :stop 1.075})])
                (assoc :particle? true)
                (assoc :kinetic-energy 0.8))))
        ;; (take 21 (shuffle (keys
        ;; controls/color-map)))
        [:orange :green-yellow :heliotrope])
       stimulus->recptive-field
       (into {}
             ;; this is a simple topological projection
             (map (fn [stimulus] [(:id stimulus)
                                  (ac/->projection
                                   n-neurons
                                   (fn [i]
                                     ;; 1 'coll' is 20
                                     ;; here atm
                                     ;; now I would say
                                     ;; that the first
                                     ;; layer made from
                                     ;; 5 is input layer
                                     (when (< (mod i 20) 5)
                                       0.05)))])
                  stimuli))
       append-inputs (fn [s inputs]
                       (-> s
                           (update-in [:eid->entity
                                       (:id n-area) :ac-area]
                                      ac/append-input-2
                                      inputs)))
       eye-ball
       (->
        (na/eye-ball {:pos (lib/rand-on-canvas-gauss 0.1)})
        (assoc-in [:on-drag-start-map :reset-n-area]
                  (fn [_ s _]
                    {:updated-state
                     (na/reset-n-area s (:id n-area))})))
       flash-some-connections (na/->flash-some-connections
                               n-area)
       n-area-knob (na/->n-area-knob
                    n-area
                    {:on-click-map
                     {:flash-some-connections
                      flash-some-connections}})
       stimuli
       (into
        []
        (map
         (fn [e]
           (->
            e
            (assoc-in
             [:on-double-click-map :append-input]
             (fn [{:as e :keys [id]} s _k]
               (let [neurons (stimulus->recptive-field
                              (:id e))
                     indices (ac/read-projection
                              (stimulus->recptive-field
                               id))
                     n-area ((lib/entities-by-id
                              @lib/the-state)
                             id-area)
                     i->pos
                     (fn [i]
                       ((n-area :i->pos) n-area i))]
                 {:updated-state
                  (->
                   s
                   (append-inputs [neurons])
                   (lib/append-ents
                    (map
                     (fn [i]
                       (let [end-pos (i->pos i)]
                         (elib/->flash-line-tracking
                          e
                          end-pos)))
                     indices)))})))
            (assoc-in [:on-click-map :shiny-projection]
                      (na/->shiny-projection
                       (fn [e _]
                         (ac/read-projection
                          (stimulus->recptive-field
                           (:id e))))
                       (:id n-area))))))
        stimuli)
       neuron-tick
       (fn [s]
         (-> s
             (update-in [:eid->entity (:id n-area) :ac-area]
                        ac/update-neuronal-area-2)))
       touch-me-gate (assoc-in (na/->touch-me-gate [300 300])
                               [:on-click-map :clear-area]
                               (fn [e s _]
                                 {:updated-state
                                  (update-in s
                                             [:eid->entity
                                              (:id n-area) :ac-area]
                                             ac/set-input
                                             #js [])}))
       ;; perspective-flower  (->perspective-flower
       ;; {:n-neurons n-neurons})
       append-perspective-input (fn [s]
                                  s
                                  ;; (append-inputs s
                                  ;;                (na/perspective-inputs
                                  ;;                 ((lib/entities-by-id
                                  ;;                   s)
                                  ;;                  (:id
                                  ;;                   perspective-flower))))
                                  )
       sensoric-field (na/->sensoric-field [631 400])
       append-stimuli-inputs
       (fn [s]
         (append-inputs s
                        (keep stimulus->recptive-field
                              (:stimuli
                               ((lib/entities-by-id s)
                                (:id sensoric-field))))))


       eye-movement (->eye-movement {:n-area n-area
                                     :n-neurons n-neurons
                                     :sensoric-field sensoric-field
                                     :stimuli stimuli})


       n-area
       (-> n-area
           (assoc
            :i->color
            (fn [e]
              (let [stimuli (filter :stimulus?
                                    (lib/entities
                                     @lib/the-state))
                    stimuli->neurons
                    (map (juxt
                          identity
                          (comp
                           stimulus->recptive-field
                           :id))
                         stimuli)
                    m (into
                       {}
                       (mapcat
                        (fn [[{:keys [color]} neurons]]
                          (let [c (lib/->hsb color)]
                            (for [i (ac/read-projection neurons)]
                              [i c]))))
                       stimuli->neurons)
                    p-flower-neurons (into
                                      {}
                                      ;; (for [pline
                                      ;; (:p-lines
                                      ;;              perspective-flower)
                                      ;;       i
                                      ;;       (.valueOf
                                      ;;       pline)]
                                      ;;   [i (:color
                                      ;;   perspective-flower)])
                                      )
                    ;; i -> color
                    remembered-neurons
                    (or (:remembered-neurons e)
                        (constantly nil))]
                (some-fn remembered-neurons
                         m
                         p-flower-neurons
                         (constantly
                          (lib/->hsb
                           (:cyan
                            controls/color-map)))))))
           (assoc :clickable? true))



       stability-detector
       (na/->stability-detector
        {:grid-width 5
         :n-wires 50
         :color (lib/->hsb (:misty-rose controls/color-map))
         ;; :i->color
         :spacing 25
         :transform
         (lib/->transform [400 200] 25 25 1)}
        n-area)

       state (-> state
                 (assoc :neuronal-area (:id n-area))
                 (lib/append-ents [n-area
                                   ;; input-space
                                   ;; big-theta
                                   ])
                 (lib/append-ents stimuli)
                 (lib/append-ents [sensoric-field
                                   touch-me-gate n-area-knob
                                   (:e stability-detector)
                                   ;; perspective-flower
                                   ]))]
      (->
       state
       (assoc-in [:on-update-map :eye-movement]
                 (:update eye-movement))
       (assoc-in
        [:on-update-map :show-lines]
        (lib/every-n-seconds
         0.1
         (fn [s _]
           (let [show-lines
                 (fn [s]
                   (let [activations
                         (ac/read-activations
                          (:ac-area
                           ((lib/entities-by-id s)
                            id-area)))
                         e ((lib/entities-by-id s)
                            id-area)
                         i->pos (fn [i]
                                  ((e :i->pos) e i))]
                     (for [[i j] (partition-all
                                  2
                                  (take (* 3 2)
                                        (shuffle
                                         activations)))
                           :when (and i j)]
                       (elib/->flash-of-line (i->pos i)
                                             (i->pos
                                              j)))))]
             (-> s
                 (lib/append-ents (show-lines s)))))))
       (assoc-in
        [:eid->entity (:id n-area) :on-update-map
         :neuron-tick]
        (lib/every-n-seconds
         0.1
         (fn [e s _]
           {:updated-state
            (let [s (update s :neuron-tick (fnil inc 0))
                  s ((:update stability-detector) s)
                  wipe-inputs?
                  (zero? (mod (:neuron-tick s) 10))
                  append-inputs? (when-not wipe-inputs?
                                   (even? (:neuron-tick
                                           s)))]


              (cond-> s
                (odd? (:neuron-tick s)) neuron-tick
                wipe-inputs? (update-in [:eid->entity
                                         (:id n-area)
                                         :ac-area]
                                        ac/set-input
                                        #js [])
                append-inputs? append-stimuli-inputs
                append-inputs?
                append-perspective-input))}))))))




;;
;; Thus, a few strong connections could drive local excitation
;; of the majority of neurons with weak connections (Cossell et al., 2015).
