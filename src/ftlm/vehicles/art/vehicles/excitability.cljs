(ns ftlm.vehicles.art.vehicles.excitability
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
   ["mathjs" :as mathjs]
   [ftlm.vehicles.assembly-calculus :as ac]
   [ftlm.vehicles.art.neuronal-area :as na]))

(defn rand-cap-k-threshold-device
  [numbers]
  (fn [{:as state :keys [synaptic-input]}]
    (assoc state
           :activations
           (ac/cap-k
            (rand-nth numbers) synaptic-input))))

(defn
  stimuli-inputs []
  (fn [s]
    ;; (let [stimuli []]
    ;;   (map stimulus->recptive-field stimuli))
    ))

(defn ->visual-field
  []
  (->
   (lib/->entity
    :rect
    {:color (lib/with-alpha (lib/->hsb controls/white) 0)
     :stroke (lib/->hsb controls/white)
     :stroke-weight 1
     :corner-r 5
     :transform (lib/->transform [631 400] 200 300 1)})
   (lib/live
    [:fade (lib/->fade-pulse-2 6.0 :stroke)])))

(defn ->eye-movement-effectors
  [{:keys [n-neurons eye visual-field n-area]}]
  ;;
  ;;
  ;; model eye movement in terms of a collection of 4
  ;; vectors can be mixed to yield all kinds of
  ;; diagonal results
  ;;
  ;; Say that the neuron projection is encoding a fixed
  ;; position
  ;;
  ;;
  ;;      ^
  ;;   <--+-->
  ;;      v
  ;;
  ;; Let's say we make an eye saccade every 200 ms
  ;;
  ;;
  ;;
  (let [dirs
          ;; [:up :down :left :right]
          ;; (into
          ;;  []
          ;;  )
          [[1 0] [0 1] [-1 0] [0 -1]]
        dirs (concat
               dirs
               dirs
               (map (fn [[x y]] [(* 5 x) (* 5 y)]) dirs)
               (map (fn [[x y]] [(* 10 x) (* 10 y)]) dirs))
        effector-projections
          ;; (ac/gaussian
          ;; 1.0
          ;; (rand-int
          ;; n-neurons)
          ;; 100
          ;; i)
          (map (fn [dir]
                 (let [proj (ac/->projection
                              n-neurons
                              (fn [i]
                                (when (< 15 (mod i 20))
                                  0.05)))]
                   [dir proj]))
            dirs)]
    {:shiny-projection
       (fn [s]
         (na/shiny-projection-1
          s
          (.valueOf (rand-nth
                     (into [] (map second effector-projections))))
          (:id n-area)
          (lib/->hsb (:red controls/color-map))))
     :update
       (lib/every-n-seconds
         0.2
         (fn [s]
           (let [activations (ac/read-activations
                               (:ac-area
                                 ((lib/entities-by-id s)
                                   (:id n-area))))
                 eye-pos
                   ;; calc the position from what
                   ;; neurons are active what is
                   ;; maximally in one direction?
                   (reduce
                     (fn [acc [dir value]]
                       (mathjs/add
                         acc
                         (mathjs/dotMultiply
                           #js [(/ (:width (lib/transform
                                             visual-field))
                                   2)
                                (/ (:height (lib/transform
                                              visual-field))
                                   2)]
                           (mathjs/multiply value dir))))
                     (clj->js (lib/position visual-field))
                     (for [[dir proj] effector-projections]
                       [(clj->js dir)
                        (/ (ac/count-intersection
                             proj
                             activations)
                           (ac/count-projection proj))]))]
             (-> s
                 ;; (na/reset-n-area (:id n-area))
                 (update-in [:eid->entity (:id eye)
                             :transform :pos]
                            (constantly eye-pos))))))}))


(defmethod lib/setup-version :excitability
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
     n-area (->
             n-area
             (assoc :ac-area
                    {:activations #js []
                     :inhibition-model
                     (comp
                      (rand-cap-k-threshold-device
                       [ ;; (* 0.1 n-neurons)
                        (* 0.05 n-neurons)])
                      ;; ac/neuron-skip
                      ac/intrinsic-excitability
                      ac/attenuation
                      )
                     :skip-rate 1.0
                     :attenuation-decay 0.1
                     :attenuation-malus-factor 0.6
                     :excitability-growth 0.6
                     :excitability-decay 0.1
                     :n-neurons n-neurons
                     :weights
                     (ac/->directed-graph-with-geometry
                      n-neurons
                      (ac/lin-gaussian-geometry-wrap
                       {:amplitude 0.4
                        :n-neurons n-neurons
                        :std-deviation 50}))
                     ;; (ac/->random-directed-graph n-neurons 0.1)
                     ;; (ac/->directed-graph-with-geometry
                     ;;  n-neurons
                     ;;  (ac/lin-gaussian-geometry-wrap
                     ;;   {:amplitude 0.8
                     ;;    ;;
                     ;;    (:connectivity-amplitude
                     ;;    ;; controls)
                     ;;    :n-neurons n-neurons
                     ;;    :std-deviation 150 ;;
                     ;;    (:connectivity-std-deviation
                     ;;    ;; controls)
                     ;;    }))
                     })
             ;; (assoc-in [:on-update-map
             ;; :normalize-weights]
             ;;           (lib/every-n-seconds
             ;;            5
             ;;            (fn [e _s _]
             ;;              (update-in e
             ;;                         [:ac-area
             ;;                         :weights]
             ;;                         ac/normalize))))
             )
     id-area (:id n-area)
     ;; big-theta (->dynamic-threshold-thought-pump
     ;; n-area)
     stimuli (into []
                   (map
                    (fn [color]
                      (-> (na/->stimulus
                           (color controls/color-map))
                          (lib/put
                           (lib/rand-on-canvas-gauss 0.2))
                          (lib/live [:breath
                                     (elib/->tiny-breath
                                      {:speed 1
                                       :start 1.0
                                       :stop 1.075})]))))
                   [:orange :green-yellow :heliotrope])
     stimulus->recptive-field
     (into {}
           ;; this is a simple topological projection
           (map
            (fn [stimulus]
              [(:id stimulus)
               (ac/->projection
                n-neurons
                (fn [i]
                  ;; 1 'coll' is 20 here atm
                  ;; now I would say that the first layer made from 5 is input layer
                  (when (< (mod i 20) 5)
                    0.05))
                )

               ])
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
     append-stimuli-inputs
     (fn [s]
       (append-inputs s
                      (keep stimulus->recptive-field
                            (map :id
                                 (na/stimuli-at-eye-ball
                                  eye-ball
                                  (map :id stimuli)
                                  s)))))
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
     perspective-flower
     (->
      (na/perspective-flower
       {:color (lib/->hsb (:navajo-white
                           controls/color-map))
        :count 12
        :i->fill (fn [e i]
                   (when ((:active-p-lines e) i)
                     (lib/->hsb (:navajo-white
                                 controls/color-map))))
        :n-neurons n-neurons
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
                                  e)))))))))))]))
     append-perspective-input
     (fn [s]
       s
       ;; (append-inputs s
       ;;                (na/perspective-inputs
       ;;                 ((lib/entities-by-id
       ;;                 s)
       ;;                  (:id
       ;;                  perspective-flower))))
       )
     visual-field (->visual-field)
     n-area
     (->
      n-area
      (assoc
       :i->color
       (fn [e]
         (let [stimuli (filter :stimulus?
                               (lib/entities
                                @lib/the-state))
               stimuli->neurons
               (map (juxt identity
                          (comp
                           stimulus->recptive-field
                           :id))
                    stimuli)
               m (into
                  {}
                  (mapcat
                   (fn [[{:keys [color]} neurons]]
                     (let [c (lib/->hsb color)]
                       (for [i (.valueOf neurons)]
                         [i c]))))
                  stimuli->neurons)
               p-flower-neurons
               (into
                {}
                (for [pline (:p-lines
                             perspective-flower)
                      i (.valueOf pline)]
                  [i (:color perspective-flower)]))
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
     eye-movement (->eye-movement-effectors
                   {:eye eye-ball
                    :n-area n-area
                    :n-neurons n-neurons
                    :visual-field visual-field})
     eye-ball (-> eye-ball
                  (assoc-in [:on-click-map :shiny-projection]
                            (fn [_ s _]
                              {:updated-state
                               ((:shiny-projection eye-movement) s)})))

     state (-> state
               (assoc :neuronal-area (:id n-area))
               (lib/append-ents [n-area
                                 ;; input-space
                                 ;; big-theta
                                 ])
               (lib/append-ents stimuli)
               (lib/append-ents [visual-field touch-me-gate
                                 n-area-knob eye-ball
                                 perspective-flower]))]
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
                     append-inputs? (even? (:neuron-tick
                                             s))]
                 (cond-> s
                   (odd? (:neuron-tick s)) neuron-tick
                   append-inputs? append-stimuli-inputs
                   append-inputs?
                   append-perspective-input))}))))))




;;
;; Thus, a few strong connections could drive local excitation
;; of the majority of neurons with weak connections (Cossell et al., 2015).

(comment
  (filter :stimulus?  (lib/entities @lib/the-state)))
