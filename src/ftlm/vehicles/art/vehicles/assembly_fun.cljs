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
   [ftlm.vehicles.art.grid]
   [goog.style]
   [ftlm.vehicles.hdv]
   [tech.v3.datatype.argops :as argops]
   [tech.v3.datatype.functional :as dtype-fn]
   [tech.v3.dataset :as ds]
   [tech.v3.datatype :as dtype]
   ["mathjs" :as mathjs]))

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

;; ==== assembly calculus ====

;; 1. neuronal area
;;   -> n neurons / 'neuronal units'
;;   -> random directed graph ((with or without geometry))
;;   ->
;; 2. Inhibition model
;;   -> cap-k algorithm
;; 3. Discrite time
;;   - each time step:
;;   - 1. update the state of the neurons (fire or not),  (cap-k algorithm)
;;   - 2. update the state of the synapses (update weights) (hebbian learning)
;;

;; 4. operations
;; - fire neurons in the assembly (inputs)
;;

(defn gaussian [amplitude mean std-deviation x]
  (* amplitude (Math/exp
                (-
                 (/ (Math/pow (- x mean) 2)
                    (* 2 (Math/pow std-deviation 2)))))))

(defn fires? [activations i]
  (nth activations i))

;; Hebbian plasticity dictates that w-ij be increased by a factor of 1 + β at time t + 1
;; if j fires at time t and i fires at time t + 1,
;; --------------------------------------------------------------------------------------

;; (defn ->hebbian-plasticity
;;   [plasticity]
;;   (fn [current-activations next-activations weights]
;;     (time
;;      (into
;;       []
;;       (let [weight (fn [i j] (nth (nth weights i) j))]
;;         (for [i (range (count weights))]
;;           (dtype/make-container
;;            :float
;;            (for [j (range (count weights))]
;;              (if (and (fires? current-activations j)
;;                       (fires? next-activations i))
;;                (* (weight i j) (+ 1 plasticity))
;;                (weight i j))))))))))
;; (made it a bit more performant)

(defn ->hebbian-plasticity
  [plasticity]
  (fn [current-activations next-activations weights]
    (into []
          (map (fn [w i-fires?]
                 (if-not i-fires?
                   w
                   (dtype/clone
                    (dtype/emap
                     (fn [w j-fires?]
                       (if j-fires?
                         (* w (+ 1 plasticity))
                         w))
                     :float
                     w
                     current-activations))))
               weights
               next-activations))))

(comment
  ((->hebbian-plasticity 10)
    [true false true]
    [true true false]
    [[1 1 1] [1 1 1] [1 1 1]])
  ;; [#typed-buffer [[:float 3] [11 1 11]]
  ;;  #typed-buffer [[:float 3] [11 1 11]] [1 1 1]]
  )

(defn normalize-weigths
  [weights]
  ;; impl with mathjs
  weights
  ;; (time
  ;;  (let [weights (mathjs/matrix
  ;;                 (clj->js (map (fn [ws] (into-array :int ws)) weights)))
  ;;        sum (mathjs/sum weights)
  ;;        weights (mathjs/multiply weights (/ 1 sum))]
  ;;    ;; (def weights weights)
  ;;    (.valueOf weights)))

  ;; (time
  ;;  (into []
  ;;        (for [i (range (count weights))
  ;;              :let [weight (nth weights i)
  ;;                    sum (dtype-fn/sum weight)]]
  ;;          (if (zero? sum)
  ;;            weight
  ;;            (dtype/clone
  ;;             (dtype/emap
  ;;              (fn [w] (/ w sum))
  ;;              :float
  ;;              (nth weights i)))))))
  )

;; Assembly calculus command
(defn set-inputs
  [{:as e :keys [weights]} next-active]
  (let [next-active? (into #{} next-active)]
    (assoc e
      :elements (dtype/clone
                  (dtype/make-container
                    :boolean
                    (for [i (range (count weights))]
                      (boolean (next-active? i))))))))



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

(defn ->weight
  [{:keys [weights]} neuron-i neuron-j]
  (get-in weights [neuron-i neuron-j]))

(defn ->edges [state neurons]
  (for [i (range (count neurons))
        j (range (count neurons))
        :let [w (->weight state i j)]
        :when (not (zero? w))]
    [i j w]))

(defn ->neuronal-area-1
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
                (let [_start-pos (lib/position (neurons i))
                      _end-pos (lib/position (neurons j))]
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
                     :vertices
                       (elib/rect-multi-line-vertices
                         (neurons j)
                         (neurons i))})))
              edges))
        e (assoc e :components (map :id neurons))]
    (concat [e] neurons edges)))

(defmethod setup-version :grid-1
  [state]
  (let [state (-> state
                  (lib/append-ents (->neuronal-area-1
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



;; neuron elements is an array of neurons
;; each neuron is active or not active

(defn dot-product [v1 v2]
  (dtype-fn/sum (dtype/emap (fn [a b] (* a b)) :float v1 v2)))

(defn ->synaptic-input-1
  [weights inputs]
  (map (fn [v] (. v -value))
       (let [w (mathjs/matrix
                (clj->js (map (fn [ws] (into-array :int ws)) weights)))
             inputs (mathjs/matrix (into-array (map (fn [i] (if i 1 0)) inputs)))]
         (mathjs/multiply w inputs)))


  ;; (let [w (mathjs/matrix
  ;;          (clj->js (map (fn [ws] (into-array :int ws)) weights)))
  ;;       inputs (mathjs/matrix (into-array (map (fn [i] (if i 1 0)) inputs)))]
  ;;   (mathjs/multiply w inputs))

  ;; this would be so easy to parallelize...
  ;; (for [i (range (count weights))]
  ;;   (dot-product (weights i) inputs))
  )

(comment
  (cap-k
   (map (fn [v] (. v -value))
        (let [w (mathjs/matrix
                 (clj->js (map (fn [ws] (into-array :int ws)) weights)))
              inputs (mathjs/matrix (into-array (map (fn [i] (if i 1 0)) inputs)))]
          (mathjs/multiply w inputs)))
   10)

  (def w (ds/->dataset
          [{0 [1 1 1]
            1 [1 0 1]
            2 [1 1 1]}]
          ))
  (def inputs [1 1 1])

  (ds/column-map w :synaptic-input (fn [coll] (dot-product coll inputs)))

  (map (fn [coll] (dot-product coll inputs)) (ds/columns w))
  (first (ds/columns w))
  (ds/rowvecs w)

  ;; (ds/ :synaptic-input (fn [coll] (dot-product coll inputs)))

  (mathjs/matrix (clj->js [[0 1] [2 3] [4 5]]))


  (def w (mathjs/matrix (clj->js [[1 1 1] [1 0 1] [1 1 1]])))
  (def inputs (mathjs/matrix (clj->js [1 1 1])))
  (mathjs/multiply w inputs)


  (let [testds (ds/->dataset [{:a 1.0 :b 2.0} {:a 3.0 :b 5.0} {:a 4.0 :b nil}])]
    ;;result scanned for both datatype and missing set
    (ds/column-map testds :b2 #(when % (inc %)) [:b])))


(defn ->synaptic-input
  [state]
  (->synaptic-input-1 (:weights state) (:elements state)))

(defn cap-k [activations k]
  (take k (argops/argsort > activations)))

;; --- I am allowing the neurons to
;; connect to themselves ---
(defn ->random-directed-graph
  [n-neurons density]
  (into []
        (for [_ (range n-neurons)]
          (dtype/clone
            (dtype/emap
              (fn [] (if (< (rand) density) 1.0 0.0))
              :float
              (dtype/make-container :float n-neurons))))))

;; (defn ->random-directed-graph-with-geometry
;;   [n-neurons density spread]
;;   (into []
;;         (for [i (range n-neurons)]
;;           (dtype/clone
;;            (dtype/make-container
;;             :float
;;             (for [j (range n-neurons)]
;;               (lib/normal-distr )
;;               )
;;             n-neurons))
;;           (dtype/clone
;;            (dtype/emap
;;             (fn [] (if (< (rand) density) 1.0 0.0))
;;             :float
;;             (dtype/make-container :float n-neurons))))))

(defn ->random-directed-graph-with-geometry-per-row
  [n-neurons density row-length]
  (let [row (fn [i] (quot i row-length))
        high-probablity density
        ;; (* density 10)
        low-probablity (/ density 10)]
    (into []
          (for [i (range n-neurons)]
            (dtype/clone (dtype/make-container
                           :float
                           (for [j (range n-neurons)]
                             (if (< (rand)
                                    (if (= (row i) (row j))
                                      high-probablity
                                      low-probablity))
                               1.0
                               0.0))))))))


(defn ->input-space-elements
  [n]
  (dtype/make-container :boolean n))

;; ===
;; input
;; -------------------
;; 1. Each input neuron is connected to a random subset of the neurons in the neuronal area.
;; 2. Likewise for each off-state,
;;    simulating an inhibitory neuron in between
;; This allows the area to represent the absence of something.

(defn ->input-space
  [n]
  (lib/->entity
   :grid
   {:color (:red controls/color-map)
    :input-space? true
    :draw-element
    (fn [active?]
      (q/with-stroke
        (lib/->hsb (:navajo-white controls/color-map))
        (lib/draw-color
         (if active?
           (:navajo-white controls/color-map)
           (:woodsmoke controls/color-map)))
        (q/rect 0 0 20 20)))
    :elements (->input-space-elements n)
    :grid-width 5
    :on-update-map
    {:sensitive-to-mouse
     (lib/every-n-seconds
      0.1
      (fn [e s _]
        (let [y (q/mouse-y)
              x (q/mouse-x)
              ;; project mouse position to input
              ;; space
              inputs-identity
              (int (* (/ (- (* (q/width) 0.9) x)
                         (* (q/width) 0.9))
                      n))]
          (assoc e
                 :elements
                 (dtype/clone
                  (dtype/make-container
                   :boolean
                   (for [i (range n)]
                     (boolean (= inputs-identity i)))))))))}
    :spacing 20
    :transform (lib/->transform [(elib/from-right 300) (elib/from-bottom 400)] 1 1 1)}))

(defn ->neuronal-area
  [{:as opts
    :keys [n-neurons density spacing inhibition-model
           plasticity-model grid-width]}]
  (lib/->entity
    :grid
    (merge
      {:color (:cyan controls/color-map)
       :draw-element (fn [active?]
                       (when active? (q/rect 0 0 25 25 2)))
       ;; :->elements (fn [e] (:neuron-activations e))
       :elements (dtype/make-container :boolean n-neurons)
       :i->pos (fn [{:keys [transform]} i]
                 (let [[x y] (:pos transform)
                       coll (mod i grid-width)
                       row (quot i grid-width)
                       x (+ x (* coll spacing))
                       y (+ y (* row spacing))]
                   [x y]))
       :next-color (constantly (:cyan controls/color-map))
       :weights
         (->random-directed-graph-with-geometry-per-row
           n-neurons
           density
           grid-width)
       ;; (->random-directed-graph n-neurons density)
       :on-update-map
         {:normalize-weights
            (lib/every-n-seconds
              5
              (fn [e _s _]
                (update e :weights normalize-weigths)))
          :sensory-input
            (lib/every-n-seconds
              3
              (fn [e s _k]
                (let [{:keys [elements input-projection]}
                        ((lib/entities-by-id s)
                          (s :input-space))
                      sensory-inputs elements
                      synaptic-input (->synaptic-input-1
                                       input-projection
                                       sensory-inputs)
                      ;; ========== do you here do or
                      ;; not do the inhibition model?
                      ;; -> input inhibition model
                      ;; ==========
                      next-active
                        (inhibition-model e synaptic-input)]
                  (->
                    e
                    (set-inputs next-active)
                    (assoc
                      :next-color
                        (let [remaining
                                ;; (atom 10)
                                (atom 2)]
                          (fn []
                            (swap! remaining dec)
                            (if (<= @remaining 0)
                              (:cyan controls/color-map)
                              (:red
                                controls/color-map)))))))))
          :update-neurons
            (lib/every-n-seconds
              0.5
              (fn [e _s _]
                (let [current-activations (:elements e)
                      synaptic-input (->synaptic-input e)
                      next-active
                        (inhibition-model e synaptic-input)
                      e (set-inputs e next-active)
                      next-weights (plasticity-model
                                     current-activations
                                     (:elements e)
                                     (:weights e))]
                  (-> e
                      (assoc :weights next-weights)
                      (assoc :color ((:next-color e)))))))}
       :spacing spacing}
      opts)))

(comment
  (def example-weights (into []
                             (for [_ (range 10)]
                               (dtype/make-container
                                :float
                                (for [j (range 10)]
                                  (rand-int 10))))))

  (let [weights example-weights]
    (into
     []
     (for
         [i (range (count weights))
          :let
          [weight (nth weights i) sum (dtype-fn/sum weight)]]
         (dtype/clone
          (dtype/emap (fn [w] (/ w sum)) :float (nth weights i)))))))


;; sensory input
;; -------------------------

(defn input-projection
  [input-space-size neuronal-area connection-probability]
  (let [neuron-n (count (:elements neuronal-area))]
    (into
     []
     (for [_ (range neuron-n)]
       (dtype/clone
        (dtype/emap
         (fn []
           (if (< (rand) connection-probability) 1 0))
         :int32
         (dtype/make-container :int32
                               input-space-size)))))))

(defmethod setup-version :grid
  [state]
  (let [input-space-size 10
        n-neurons 500
        n-area (->neuronal-area
                {:density 0.1
                 :grid-width 20
                 :n-neurons n-neurons
                 :spacing 25
                 :plasticity 0.1
                 :plasticity-model (->hebbian-plasticity 0.8)
                 :inhibition-model
                 ;; (fn [state activations]
                 ;;   (let [k
                 ;;         (int
                 ;;          (+ 10
                 ;;             (gaussian
                 ;;              100
                 ;;              20
                 ;;              10
                 ;;              (count
                 ;;              (argops/argfilter
                 ;;              true? (:elements
                 ;;              state))))))]
                 ;;     (println k)
                 ;;     (cap-k activations k)))
                 (fn [_state activations]
                   (cap-k activations 10))
                 :transform
                 (lib/->transform [100 100] 20 20 1)})
        id-area (:id n-area)
        input-space (->input-space input-space-size)
        input-space
        (assoc input-space  :input-projection (input-projection input-space-size n-area 0.1))
        state (-> state
                  (assoc :input-space (:id input-space))
                  (assoc :neuronal-area (:id n-area))
                  (lib/append-ents [n-area input-space]))]
    (->
     state
     (assoc-in
      [:on-update-map :time-tick]
      (lib/every-n-seconds
       0.1
       (fn [s _]
         (let
             [show-one-line
              (fn [s]
                (let [e ((lib/entities-by-id s) id-area)
                      rand-coll (rand-int (count (:weights e)))
                      i->pos (fn [i] ((e :i->pos) e i))
                      rand-edge-j
                      (first
                       (take 1 (argops/argfilter #(not (zero? %)) (get (:weights e) rand-coll))))]
                  (when rand-edge-j
                    (let [i rand-coll j rand-edge-j]
                      [(lib/->entity
                        :multi-line
                        {:vertices (elib/rect-line-vertices-1 (i->pos i) (i->pos j))
                         :color (:cyan controls/color-map)
                         :stroke-weight 1
                         :on-update-map
                         {:fade (elib/->fade 1)}
                         :lifetime 3
                         :transform (lib/->transform (i->pos i) 1 1 1)})]))))]
             (-> s
                 (lib/append-ents
                  (apply concat (repeatedly 3 #(show-one-line s))))))))))))

(defn setup
  [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (q/background (lib/->hsb (-> controls
                               :background-color)))
  (let [state {:controls controls :on-update []}
        state (-> state setup-version)]
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



(comment

  (into-array :int (take 3 (argops/argsort > (.valueOf (mathjs/matrix #js [1 2 3 4])))))


  (dtype/set-and
   (dtype/indexed-buffer [0 1 2] (dtype/make-container :float (range 10))))

  (first (dtype/set-and (dtype/->set a1) (dtype/->set a2)))
  (dtype/set-and (dtype/->set a1) (dtype/->set a2))

  (dtype/indexed-iterate!)

  (do
    (def a1 (dtype/make-container :int32 [0 1 2]))
    (def a2 (dtype/make-container :int32 [2]))
    (dtype/emap
     (fn [w] (* 1.1 w))
     :float
     (dtype/indexed-buffer
      (dtype/set-and (dtype/->set a1) (dtype/->set a2))
      (dtype/make-container :float (range 10)))))
  )
