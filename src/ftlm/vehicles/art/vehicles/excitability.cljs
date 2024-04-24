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
           (ac/cap-k (rand-nth numbers) synaptic-input))))

(defn
  stimuli-inputs []
  (fn [s]
    ;; (let [stimuli []]
    ;;   (map stimulus->recptive-field stimuli))
    ))

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
     n-area (-> n-area
                (assoc
                  :ac-area
                    {:activations #js []
                     :inhibition-model
                       (comp (rand-cap-k-threshold-device
                               [(* 0.1 n-neurons)
                                (* 0.05 n-neurons)])
                             ac/intrinsic-excitability)
                     :excitability-growth 0.2
                     :excitability-decay 0.1
                     ;; (-> state :controls
                     ;; :excitability-decay)
                     :n-neurons n-neurons
                     :weights
                       ;; (ac/->random-directed-graph
                       ;; n-neurons 0.1)
                       (ac/->directed-graph-with-geometry
                         n-neurons
                         (ac/lin-gaussian-geometry-wrap
                           {:amplitude 0.5
                            ;; (:connectivity-amplitude
                            ;; controls)
                            :n-neurons n-neurons
                            :std-deviation 100
                            ;; (:connectivity-std-deviation
                            ;; controls)
                           }))})
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
             (let [sectors (count stimuli)
                   sector-lenght (/ n-neurons sectors)]
               (map-indexed
                 (fn [stim-idx stimulus]
                   [(:id stimulus)
                    (ac/->projection
                      n-neurons
                      (fn [i]
                        (when (< (* stim-idx sector-lenght)
                                 i
                                 (* (inc stim-idx)
                                    sector-lenght))
                          0.1)))])
                 stimuli)))
     append-inputs (fn [s inputs]
                     (-> s
                         (update-in [:eid->entity
                                     (:id n-area) :ac-area]
                                    ac/append-input-2
                                    inputs)))
     eye-ball (-> (na/eye-ball
                    {:pos (lib/rand-on-canvas-gauss 0.1)})
                  (assoc-in
                    [:on-drag-start-map :reset-n-area]
                    (fn [_ s _]
                      {:updated-state
                       (-> s
                           (update-in [:eid->entity (:id n-area) :ac-area] ac/set-input #js []))}



                      )))
     append-stimuli-inputs
       (fn [s]
         (append-inputs s
                        (keep stimulus->recptive-field
                              (map :id
                                (na/stimuli-at-eye-ball
                                  eye-ball
                                  (map :id stimuli)
                                  s)))))
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
                                 (for [i (.valueOf neurons)]
                                   [i c]))))
                           stimuli->neurons)
                       ;; i -> color
                       remembered-neurons
                         (or (:remembered-neurons e)
                             (constantly nil))]
                   (some-fn remembered-neurons
                            m
                            (constantly
                              (lib/->hsb
                                (:cyan
                                  controls/color-map)))))))
           (assoc :clickable? true))
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
       (na/perspective-flower
         {:count 12
          :i->fill (fn [e i]
                     (when (< 3 (rand-int 5))
                       (lib/->hsb controls/white)))
          :pos [180 600]
          :radius 60})
     state (-> state
               (assoc :neuronal-area (:id n-area))
               (lib/append-ents [n-area
                                 ;; input-space
                                 ;; big-theta
                                ])
               (lib/append-ents stimuli)
               (lib/append-ents [touch-me-gate n-area-knob
                                 eye-ball
                                 perspective-flower]))]
    (-> state
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
                                        (take
                                          (* 3 2)
                                          (shuffle
                                            activations)))
                                :when (and i j)]
                            (elib/->flash-of-line (i->pos i)
                                                  (i->pos
                                                    j)))))]
                (-> s
                    (lib/append-ents (show-lines s)))))))
        (assoc-in [:eid->entity (:id n-area) :on-update-map
                   :neuron-tick]
                  (lib/every-n-seconds
                    0.1
                    (fn [_ s _]
                      {:updated-state
                         (-> s
                             append-stimuli-inputs
                             neuron-tick)}))))))






;;
;; Thus, a few strong connections could drive local excitation
;; of the majority of neurons with weak connections (Cossell et al., 2015).
