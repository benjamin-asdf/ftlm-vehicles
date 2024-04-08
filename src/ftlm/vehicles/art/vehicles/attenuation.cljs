(ns ftlm.vehicles.art.vehicles.attenuation
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
   ;; [tech.v3.dataset :as ds]
   [tech.v3.datatype :as dtype]
   ["mathjs" :as mathjs]
   [ftlm.vehicles.assembly-calculus :as ac]
   [ftlm.vehicles.art.neuronal-area :as na]))

(defn rand-cap-k-threshold-device [numbers]
  (fn [_ synaptic-input]
    (ac/cap-k (rand-nth numbers) synaptic-input)))

(defmethod lib/setup-version :attenuation
  [state]
  (let
    [n-neurons 1000
     n-area (na/->neuronal-area-ac-1
             {:density 0.1
              :frequency 10
              :grid-width 20
              :n-neurons n-neurons
              :spacing 15
              :transform
              (lib/->transform [50 50] 20 20 1)})
     n-area
     (-> n-area
         (assoc :ac-area
                {:activations #js []
                 :inhibition-model
                 (rand-cap-k-threshold-device
                  [(* 0.1 n-neurons)
                   (* 0.05 n-neurons)])
                 :plasticity 0.1
                 :plasticity-model ac/hebbian-plasticity
                 :weights
                 (ac/->directed-graph-with-geometry
                  n-neurons
                  (ac/lin-gaussian-geometry-wrap
                   {:amplitude 0.1
                    :n-neurons n-neurons
                    :std-deviation 50}))})
         (assoc-in [:on-update-map :normalize-weights]
                   (lib/every-n-seconds
                    5
                    (fn [e _s _]
                      (update-in e
                                 [:ac-area :weights]
                                 ac/normalize)))))
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
           (map (juxt :id
                      #(ac/->rand-projection n-neurons
                                             0.05))
                stimuli))
     append-inputs (fn [s inputs]
                     (-> s
                         (update-in [:eid->entity
                                     (:id n-area) :ac-area]
                                    ac/append-input-2
                                    inputs)))
     sensoric-field (na/->sensoric-field [400 200])
     stimuli-inputs
     (fn [s]
       (let [stimuli (:stimuli ((lib/entities-by-id s)
                                (:id sensoric-field)))]
         (map stimulus->recptive-field stimuli)))
     append-stimuli-inputs
     (fn [s] (append-inputs s (stimuli-inputs s)))
     n-area
     (-> n-area
         (assoc
          :i->color
          (fn [e]
            (let [stimuli (filter :stimulus?
                                  (lib/entities @lib/the-state))
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
     flash-some-connections
     (fn [_ s _]
       (let [e ((lib/entities-by-id s) (:id n-area))]
         {:updated-state
          (let [i->pos (fn [i] ((:i->pos e) e i))]
            (-> s
                (lib/append-ents
                 (let [neurons (partition
                                2
                                (into
                                 #{}
                                 (repeatedly
                                  3
                                  #(rand-int
                                    n-neurons))))]
                   (for [[i j] neurons]
                     (assoc (elib/->flash-of-line
                             (i->pos i)
                             (i->pos j))
                            :color controls/white))))))}))
     n-area-knob
     (->
      (lib/->entity
       :circle
       {:clickable? true
        :color controls/white
        :on-click-map {:flash-some-connections
                       flash-some-connections}
        :on-double-click-map
        {:remember-neurons
         (fn [knob s _]
           (let [n-area ((lib/entities-by-id s)
                         (:id n-area))
                 currently-active
                 (ac/read-activations (:ac-area
                                       n-area))]
             {:updated-state
              (-> s
                  (assoc-in [:eid->entity
                             (:id n-area) :lol]
                            :lol)
                  (update-in
                   [:eid->entity (:id n-area)
                    :remembered-neurons]
                   (fnil merge {})
                   (into
                    {}
                    (map
                     (juxt
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
                                       [:transform
                                        :scale]
                                       0.4))
                            :particle? true
                            :kinetic-energy 1.5
                            :lifetime 4.5))))}))}
        :on-update-map
        {:flash-some-connections
         (lib/every-n-seconds
          0.2
          (fn [_ s _]
            (let [e ((lib/entities-by-id s)
                     (:id n-area))]
              (when (< 0
                       (mathjs/count
                        (ac/read-activations
                         (:ac-area e))))
                (flash-some-connections nil s nil)))))}
        :stroke
        (lib/with-alpha (lib/->hsb controls/white) 0)
        :stroke-weight 6
        :transform
        (lib/->transform [150 250] 15 15 0.5)})
      (lib/live [:flash
                 (lib/->fade-pulse-2 10.0 :stroke)])
      (lib/live
       [:decide-to-flash
        (lib/every-n-seconds
         10.0
         (fn [e s _]
           (let [ne ((lib/entities-by-id s)
                     (:id n-area))]
             (-> e
                 (assoc-in
                  [:on-update-map :flash]
                  (lib/->fade-pulse-2
                   (if (< 0
                          (mathjs/count
                           (ac/read-activations
                            (:ac-area ne))))
                     5.0
                     10.0)
                   :stroke))))))]))
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
                       (->
                        (assoc
                         (elib/->flash-of-line
                          (lib/position e)
                          end-pos)
                         ;; :vertices
                         ;; (elib/rect-line-vertices-2
                         ;; (lib/position
                         ;; e) end-pos -5
                         ;; 5)
                         :color
                         (:color e))
                        (assoc-in
                         [:on-update-map
                          :find-pos]
                         (fn [line-e s _]
                           (let
                               [start-pos
                                (lib/position
                                 ((lib/entities-by-id
                                   s)
                                  (:id e)))]
                               (update-in
                                line-e
                                [:vertices]
                                (constantly
                                 (elib/rect-line-vertices-1
                                  start-pos
                                  end-pos)))))))))
                   indices)))})))
          (assoc-in
           [:on-click-map :shiny-projection]
           (fn [{:as e :keys [id]} state _]
             (let [indices (ac/read-projection
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
                 state
                 (lib/append-ents
                  [(lib/->entity
                    :blinking-neurons
                    {:color (:color e)
                     :draw-functions
                     {:1
                      (fn [ble]
                        (q/stroke-weight 2)
                        (q/with-fill
                          nil
                          (q/with-stroke
                            (lib/->hsb
                             (:color ble))
                            (doall
                             (for
                                 [i (:indices
                                     ble)
                                  :let
                                  [pos
                                   (i->pos
                                    i)]]
                                 (q/with-translation
                                   pos
                                   (q/rect
                                    0
                                    0 15
                                    15
                                    3)))))))}
                     :indices indices
                     :lifetime 10.5
                     :on-update-map
                     {:fade (lib/->fade-pulse-2
                             3.0)}})]))}))))))
      stimuli)
     neuron-tick (fn [s]
                   (-> s
                       (update-in [:eid->entity (:id n-area)
                                   :ac-area]
                                  ac/update-neuronal-area)))
     touch-me-gate
     (assoc-in (na/->touch-me-gate [300 300])
               [:on-click-map :clear-area]
               (fn [e s _]
                 {:updated-state
                  (update-in
                   s
                   [:eid->entity (:id n-area) :ac-area]
                   ac/set-input
                   #js [])
                  }))
     state (-> state
               (assoc :neuronal-area (:id n-area))
               (lib/append-ents [n-area
                                 ;; input-space
                                 ;; big-theta
                                 ])
               (lib/append-ents stimuli)
               (lib/append-ents [sensoric-field
                                 touch-me-gate
                                 n-area-knob]))]
    (-> state
        (assoc-in [:eid->entity (:id n-area) :on-update-map
                   :neuron-tick]
                  (lib/every-n-seconds
                    0.3
                    (fn [_ s _]
                      {:updated-state
                         (-> s
                             append-stimuli-inputs
                             neuron-tick)}))))))
