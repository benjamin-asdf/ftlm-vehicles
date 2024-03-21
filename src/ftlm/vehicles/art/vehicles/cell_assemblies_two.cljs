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
   [ftlm.vehicles.assembly-calculus :as ac]
   [ftlm.vehicles.art.neuronal-area :as na]))

;; -------------------------------------
;; Little happy sensoric field
;; ----------------------------------
;; a place, when you move a stimulus inside it,
;; we say that the system can percieve the stimulus
;; The model is a stimulus bag, a is set of currently percieved stimuli
;;
(defn ->sensoric-field
  [pos]
  (let [flash-inputs (atom {})]
    (->
      (lib/->entity
        :rect
        {:sensoric-field? true
         :stimuli #{}
         :transform (lib/->transform pos 100 200 1)
         :z-index -10
         :corner-r 5
         :color (lib/with-alpha (lib/->hsb controls/white)
                                0)
         :stroke controls/white
         :stroke-weight 2
         :draw-functions
           {:stimuli
              (fn [e]
                (let [stimuli (:stimuli e)]
                  (doall
                    (for [[idx {:keys [color]}]
                            (map-indexed
                              vector
                              (map (lib/entities-by-id (q/state)) stimuli))]
                      (q/with-fill
                        (lib/->hsb color)
                        (q/with-stroke
                          (lib/->hsb color)
                          (q/with-translation
                            (lib/position e)
                            (q/ellipse
                             (+ 20
                              (* -1 (/ (-> e :transform :width) 2))
                              (* idx 20))
                             (* 0.8 (/ (-> e :transform :height) 2))
                             10
                             10))))))))}})
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
                                            (lib/position
                                              %)))
                                 (map :id))
                           (lib/entities s))
                   new (clojure.set/difference next-stimuli
                                               current)]
               {:updated-state
                  (cond-> s
                    :next (assoc-in [:eid->entity (:id e)
                                     :stimuli]
                            next-stimuli)
                    (seq new) (assoc-in [:eid->entity
                                         (:id e) :stroke]
                                (:color
                                  ((lib/entities-by-id s)
                                    (first next-stimuli))))
                    (seq new)
                      (assoc-in [:eid->entity (:id e)
                                 :flashing-stroke]
                        (:color ((lib/entities-by-id s)
                                  (first
                                    next-stimuli)))))})))])
      (lib/live [:reset-color
                 (lib/every-n-seconds
                   5
                   (fn [e _ _]
                     (assoc e
                       :stroke (lib/->hsb controls/white)
                       :flashing-stroke nil)))])
      (lib/live [:flash-stroke
                 (lib/every-n-seconds
                   0.2
                   (fn [e _ _]
                     (if (:flashing-stroke e)
                       (assoc e
                         :stroke (:flashing-stroke e))
                       e)))])
      (lib/live [:flash-stroke-2
                 (lib/every-n-seconds
                   0.6
                   (fn [e _ _]
                     (assoc e
                       :stroke (lib/->hsb
                                controls/white))))]))))



;;
;; inputs -> sensor neurons 'receptive fields'
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

(defonce the-state (atom {:event-q (atom [])}))

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
  (let [state @the-state
        state (update-state-inner state)]
    (reset! the-state state)
    state))

(defmulti setup-version (comp :v :controls))

(defmethod setup-version :color-assemblies
  [state]
  (let
    [input-space-size 10
     n-neurons 500
     n-area (na/->neuronal-area-ac
              {:density 0.1
               :frequency 10
               :grid-width 20
               :n-neurons n-neurons
               :spacing 15
               :transform
                 (lib/->transform [50 50] 20 20 1)})
     ;; draw-neuron
     ;; (fn [i]
     ;;   (let [s (q/state)]
     ;;     ()
     ;;     )
     ;;   )
     n-area
       (-> n-area
           (assoc :ac-area
                    {:activations (mathjs/range 0 10)
                     :plasticity 0.1
                     :plasticity-model ac/hebbian-plasticity
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
     ;; n-area
     ;; (assoc-in
     ;;  n-area
     ;;  [:draw-functions :blinking-neurons]
     ;;  (fn [{:keys [blinking-neurons] :as e}]
     ;;    (let [i->pos (fn [i] ((e :i->pos) e i))]
     ;;      (for [[]])
     ;;      (q/with-stroke
     ;;        nil
     ;;        (doall
     ;;         (for [i neurons
     ;;               :let [pos (i->pos i)]]
     ;;           (q/with-translation
     ;;             pos
     ;;             (q/rect 0 0 15 15 3))))))))
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
     stimuli
       (into
         []
         (map
           (fn [e]
             (assoc-in e
               [:on-click-map :shiny-projection]
               (fn [{:as e :keys [id]} state _]
                 {:updated-state
                    (->
                      state
                      (lib/append-ents
                        [
                         (lib/->entity
                          :blinking-neurons
                          {:on-update-map
                           {:fade (lib/->fade-pulse-2 3.0)}
                           :color (:color e)
                           :lifetime 10.5
                           :draw-functions
                           {:1
                            (fn [ble]

                              (let
                                  [n-area
                                   ((lib/entities-by-id
                                     @the-state)
                                    id-area)
                                   i->pos (fn [i]
                                            ((n-area
                                              :i->pos)
                                             n-area
                                             i))]
                                  (q/stroke-weight 2)
                                  (q/with-fill
                                    nil
                                    (q/with-stroke
                                      (lib/->hsb (:color ble))
                                      (doall
                                       (for
                                           [i (:indices
                                               ble)
                                            :let
                                            [pos (i->pos
                                                  i)]]
                                           (q/with-translation
                                             pos
                                             (q/rect
                                              0
                                              0 15
                                              15
                                              3))))))))}
                           :indices
                           (ac/read-projection
                            (stimulus->recptive-field
                             id))
                           ;; :lifetime 10
                           })]))}))))
         stimuli)
     state (-> state
               (assoc :neuronal-area (:id n-area))
               (lib/append-ents [n-area
                                 ;; input-space
                                 ;; big-theta
                                ])
               (lib/append-ents stimuli)
               (lib/append-ents [(->sensoric-field
                                   [400 200])]))]
    (-> state
        (assoc-in
          [:on-update-map :time-tick]
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
                    (lib/append-ents (show-lines s))))))))))

(defn setup
  [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (q/background (lib/->hsb (-> controls
                               :background-color)))
  (println controls)
  (let [state {:controls controls :on-update []}
        state (-> state setup-version)]
    (reset! the-state state)))

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
              :mouse-pressed (comp #(reset! the-state %) lib/mouse-pressed)
              :mouse-released (comp #(reset! the-state %) lib/mouse-released)
              :mouse-wheel (comp #(reset! the-state %) lib/mouse-wheel)
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

(comment

  ;; (remove (comp :ac-area val) (:eid->entity @the-state))


  )
