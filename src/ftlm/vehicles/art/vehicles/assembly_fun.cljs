(ns ftlm.vehicles.art.vehicles.assembly-fun
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
   [goog.style]
   [ftlm.vehicles.hdv]))

;; -> The pyramidal cells
;; I decide it makes sense to think of pyramidal cell activity as the center
;; of explanation of cortex.
;; See Braitenberg 1986
;; --
;; There are good reasons to consider the most numerrous cell type, the pyramidal cells,
;; as the basic neuronal equipment of the cortex.
;; The great majoriyt of the synapses in the cerebral cortex have pyramidal neurons
;; on both the presynaptic and postsynaptic sides.
;; ---
;; He then goes on to explain why he thinks pyramidal cells are excitatory.
;; (Very insightful)
;; 1. Cerebral cortex and especially the hippocampal region is susceptible to epileptic activity
;; If you have most neurons in hiccopumppus that maps onto the most numerous cell type being excitatory.
;; 2. Corpus callosum is excitatory because you can prevent epileptic activity from spreading by cutting it.
;; 3. The axons of cortical pyramidal cells that go to distant places, such as the spinal cord, make excitatory connections.
;;
;; -> In my words, everything speaks for, and nothing speaks against, the pyramidal cells being excitatory.
;; I think it is expecially striking, that the main cell type makes connections to the outside motor neurons.
;; It just fits with the rest of how I would interpret the system, that whatever is making output is the driver
;; of cerebral function.
;;
;; (he was right, they are excitatory).
;; Another cool insight from Braitenberg:
;; Imagine you sprinkle a few inhibitory neurons in between, now you can describe the absence of something.
;;

;; I supoose McCulloch drew them as black triangles, because of the golgi stain of the time. This is how they looked.
;; But now we have this cool brainbow in our minds, so I am allowed to give my neurons all kinds of colors, too.
;; [youtube Heinz von Foerster 'Tanz Mit Der Welt' why I think McCulloch drew black triangles]

;; ==== assebly caclulus

;; 1. neuronal area
;;   -> n neurons / 'neuronal units'
;;   -> random directed graph ((with or without geometry))
;;   ->
;; 2. Inhibition model
;;   -> cap-k algorithm
;; 3. Discrite time
;;   - each time step:
;;   - 1. update the state of the neurons (fire or not),  (cap-k algorithm)
;;

(defn synaptic-input [neuron edges]
  )

;;
(defn update-neurons [edges neurons]

  )

(defn update-weight [edges neurons]
  )

;;   - 2. update the state of the synapses (update weights) (hebbian learning)
;;

;; 4. operations
;; - fire neurons in the assembly
;;


(defn env [state] {})

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

(defn ->grid [pos n-width n-height]

  )

;; (defn ->neuronal-area [n-neurons density]

;;   {
;;    ;; :graph
;;    ;; ;; matrix
;;    ;; ;; n-neurons/n-neurons
;;    ;; ;;
;;    ;; ;; making a directed graph
;;    ;; ;; - decide neurons going to themselves ?
;;    ;; (into []
;;    ;;       (for [i (range n-neurons)]
;;    ;;         (into []
;;    ;;               (for [j (range n-neurons)]
;;    ;;                 (if
;;    ;;                     (= i j)
;;    ;;                   false
;;    ;;                   (if (< (rand) density) true false))))))


;;    :graph
;;    ;; matrix
;;    ;; n-neurons/n-neurons
;;    ;;
;;    ;; making a directed graph
;;    ;; - decide neurons going to themselves ?
;;    (into []
;;          (for [i (range n-neurons)]
;;            (into []
;;                  (for [j (range n-neurons)]
;;                    (if
;;                        (= i j)
;;                        false
;;                        (if (< (rand) density) true false))))))


;;    })

(defn ->weight [{:keys [weights]} neuron-i neuron-j]
  (get-in  weights [neuron-i neuron-j]))

(defn ->edges [state neurons]
  (for [i (range (count neurons))
        j (range (count neurons))
        :let [w (->weight state i j)]
        :when (not (zero? w))]
    [i j w]))

(defn ->neuronal-area
  [n-neurons density {:keys [pos n-width spacing]}]
  (let [e (lib/->entity
            :neuronal-area
            {:hidden? true
             :weights (into []
                            (for [i (range n-neurons)]
                              (into
                                []
                                (for [j (range n-neurons)]
                                  (if (= i j)
                                    0
                                    (if (< (rand) density)
                                      ;; initial weights?
                                      1.0
                                      0.0))))))})
        neurons
          (time
            (into
              []
              (for [i (range n-neurons)]
                (let [coll (mod i n-width)
                      row (quot i n-width)
                      pos [(+ (first pos) (* coll spacing))
                           (+ (second pos)
                              (* row spacing))]]
                  (lib/->entity
                    :circle
                    {:active? false
                     :color (:cyan controls/color-map)
                     :hidden? true
                     :neuron-i i
                     :neuronal-unit? true
                     :on-update-map
                       {:hide (lib/every-n-seconds
                                3
                                (fn [e s _]
                                  (assoc e :hidden? true)))}
                     :transform
                       (lib/->transform pos 25 25 1)})))))
        edges (->edges e neurons)
        edges
          (into
            []
            (map
              (fn [[i j _w]]
                (let [start-pos (lib/position (neurons i))
                      end-pos (lib/position (neurons j))]
                  (lib/->entity
                    :multi-line
                    {:hidden? true
                     :color (:orange controls/color-map)
                     :transform (lib/->transform
                                  (lib/position (neurons i))
                                  1
                                  1
                                  1)
                     :stroke-weight 3
                     :on-update-map
                       {:fade (fn [e s _]
                                (update
                                  e
                                  :color
                                  (fn [c]
                                    (let [c (lib/->hsb c)]
                                      (lib/with-alpha
                                        c
                                        (* 0.9
                                           (q/alpha c)))))))
                        :hide
                          (lib/every-n-seconds
                            3
                            (fn [e s _]
                              (assoc e
                                :hidden? true
                                :color
                                  (:orange
                                    controls/color-map))))}
                     :edge? true
                     ;; {:show-sometimes
                     ;;  (lib/every-n-seconds
                     ;;   (* (rand) 4)
                     ;;   (fn [e s _]
                     ;;     (assoc e :hidden? (not
                     ;;     (:hidden? e)))))}
                     :vertices
                       (elib/rect-multi-line-vertices
                         (neurons j)
                         (neurons i))})))
              edges))
        e (assoc e :components (map :id neurons))]
    (concat [e] neurons edges)))

(defmethod setup-version :grid
  [state]
  (let [state (-> state
                  (lib/append-ents (->neuronal-area
                                     ;; 6 10
                                     200
                                     ;; 6
                                     0.01
                                     {:n-width 20
                                      ;; 20 3
                                      :pos [100 100]
                                      :spacing 30})))
        edges-set (into #{}
                        (comp (filter :edge?) (map :id))
                        (lib/entities state))
        neuron-lookup (into {}
                            (comp (filter :neuron-i)
                                  (map (juxt :neuron-i
                                             :id)))
                            (lib/entities state))]
    (->
      state
      (assoc-in
        [:on-update-map :time-tick]
        (lib/every-n-seconds
          1
          (fn [s _]
            (let [color ((rand-nth
                           [:magenta :orange :heliotrope
                            :green-yellow :mint :fruit-salad
                            :yello :sweet-pink])
                          controls/color-map)
                  edges (take 20 (shuffle edges-set))
                  s (reduce (fn [s id]
                              (update-in s
                                         [:eid->entity id]
                                         (fn [e]
                                           (assoc e
                                             :hidden?
                                               false))))
                      s
                      edges)
                  neurons-active
                    (take 25 (shuffle neuron-lookup))
                  s (reduce (fn [s [_ id]]
                              (update-in s
                                         [:eid->entity id]
                                         (fn [e]
                                           (assoc e
                                             :active? true
                                             :hidden? false
                                             :color
                                               color))))
                      s
                      neurons-active)]
              (-> s
                  (update :tick (fnil inc 0))))))))))

(defn setup
  [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (q/background (lib/->hsb (-> controls
                               :background-color)))
  (let [state {:controls controls :on-update []}
        state (-> state
                  setup-version)]
    (reset! the-state state)))

(defn on-double-click
  [state id]
  (lib/call-double-clicks state ((lib/entities-by-id state) id)))

(defn double-clicked? [{id-1 :id time-old :time} {id-2 :id time-new :time}]
  (and
   (= id-2 id-1)
   (< (- time-new time-old) 300)))

(defn mouse-pressed
  [state]
  (if-let
      [draggable (lib/find-closest-draggable state)]
    (let [new-selection {:id (:id draggable) :time (q/millis)}
          old-selection (:selection state)
          state (-> state
                    (assoc :pressed true)
                    (assoc-in [:eid->entity (:id draggable) :dragged?] true)
                    (assoc :selection new-selection))
          state ((lib/->call-callbacks :on-click-map) state draggable)
          state ((lib/->call-callbacks :on-drag-start-map) state draggable)]
      (cond-> state
        (double-clicked? old-selection new-selection)
        (on-double-click (:id draggable))))
      state))

(defn mouse-released
  [{:as state :keys [selection]}]
  (if-not selection
    state
    (let [{:as selection :keys [dragged?]}
            ((lib/entities-by-id state) (:id selection))]
      (if-not selection
        state
        (cond-> state
          dragged? ((lib/->call-callbacks :on-drag-end-map)
                     state
                     selection)
          :regardless (update-in
                        [:eid->entity (:id selection)]
                        (fn [e]
                          (assoc e :dragged? false))))))))

(defn rotate-entity
  [state id rotation]
  (update-in state [:eid->entity id :transform :rotation] + rotation))

(defn mouse-wheel [state rotation]
  (if-let [ent ((lib/entities-by-id state) (-> state :selection :id))]
    (update-in state [:eid->entity (:id ent) :angular-acceleration] + (/ rotation 60 2.5))
    state))

(defn sketch
  [host {:keys [width height]} controls]
  (let [[screen-width screen-height] (lib/window-dimensions)
        width (cond (= width "max") screen-width width width :else screen-width)
        height (cond (= height "max") screen-height height height :else screen-height)
        width 1000
        height 800]
    (q/sketch :host host
              :size [width height]
              :setup (partial setup controls)
              :update update-state
              :draw draw-state
              :features [:keep-on-top]
              :middleware [m/fun-mode]
              :mouse-pressed mouse-pressed
              :mouse-released mouse-released
              :mouse-wheel mouse-wheel
              :frame-rate 30)))


(defonce restart-fn (atom nil))
(defmethod art/view "assembly-fun"
  [{:as opts :keys [place version]}]
  (let [f (fn []
            (let [controls
                  (merge
                   (controls/default-versions "assembly-fun")
                   (get-in versions ["assembly-fun" version])
                   @user-controls/!app)]
              (sketch place opts controls)))]
    (reset! restart-fn f)
    (f)))

(defmethod user-controls/action-button ::restart
  [_]
  (some-> @restart-fn (apply nil)))
