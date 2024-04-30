(ns ftlm.vehicles.art.vehicles.cell-assemblies-two
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
   [ftlm.vehicles.art.vehicles.attenuation]
   [ftlm.vehicles.art.vehicles.excitability]
   [ftlm.vehicles.art.vehicles.pulling-stimuli]
   [ftlm.vehicles.assembly-calculus :as ac]
   [ftlm.vehicles.art.neuronal-area :as na]))

;;
;; some button to make a neuron step,
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

(defmethod lib/setup-version :color-assemblies
  [state]
  (let
    [input-space-size 10
     n-neurons 500
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
                     (fn [state synaptic-input]
                       (ac/cap-k 50 synaptic-input))

                     :plasticity 0.1
                     :plasticity-model ac/hebbian-plasticity
                     :n-neurons n-neurons
                     :weights
                     (ac/->directed-graph-with-geometry
                      n-neurons
                      (ac/lin-gaussian-geometry
                       {:amplitude 0.3
                        :std-deviation 40}))})
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
                                               0.02))
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
     n-area-knob
       (->
         (lib/->entity
           :circle
           {:clickable? true
            :color controls/white
            :on-click-map
              {:flash-some-connections
                 (fn [_ s _]
                   (let [e ((lib/entities-by-id s)
                             (:id n-area))]
                     {:updated-state
                        (let [i->pos (fn [i]
                                       ((:i->pos e) e i))]
                          (->
                            s
                            (lib/append-ents
                              (let
                                [neurons
                                   (partition
                                     2
                                     (into
                                       #{}
                                       (repeatedly
                                         20
                                         #(rand-int
                                            n-neurons))))]
                                (for [[i j] neurons]
                                  (assoc
                                    (elib/->flash-of-line
                                      (i->pos i)
                                      (i->pos j))
                                    :color
                                      controls/white))))))}))}
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
     touch-me-gate (assoc-in (na/->touch-me-gate [300 300])
                     [:on-click-map :inputs]
                     (fn [e s _]
                       {:updated-state
                          (-> s
                              append-stimuli-inputs
                              neuron-tick)}))
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
    (-> state)))

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
(defmethod art/view "assembly-friends"
  [{:as opts :keys [place version]}]
  (let [f (fn []
            (let [controls
                  (merge
                   (controls/default-versions "assembly-friends")
                   (get-in versions ["assembly-friends" version])
                   @user-controls/!app)]
              (sketch place opts controls)))]
    (reset! restart-fn f)
    (f)))

(defmethod user-controls/action-button ::restart
  [_]
  (some-> @restart-fn (apply nil)))















;; thalamus
;; relay nuclei (first order) -> cortex
;; cortex -> derived / second order nuclei (very nice overview: https://www.youtube.com/live/aB2M1gg_1sU?si=i5CX2ALG_yInKmq4)
;; nice neuroanatomical overview: https://youtu.be/eGKUg1n1reo?si=htokWothaTil2AWU
;;


;; thalamus matrix system
;; (this is a HDV implementation)
;; or biologically intuitive: this is the context, which is a point in meaning space.

;; you make random inhibitory connections down into the relay nucleus



;; the claustrum could be the global threshold device
;; stimulating it -> consciousness gone, perhaps because you inhibit a large area

;; Francis Crick & Christof Koch (2005): What is the function of the claustrum? Phil. Trans. R. Soc. Lond. B. Biol. Sci. 360, nr. 1458, pp. 1271–1279 (PDF)

;; M. Z. Koubeissi, F. Bartolomei, A. Beltagy, F. Picard: Electrical stimulation of a small brain area reversibly disrupts consciousness. In: Epilepsy & behavior : E&B. [elektronische Veröffentlichung vor dem Druck] Juni 2014, ISSN 1525-5069. doi:10.1016/j.yebeh.2014.05.027. PMID 24967698.
