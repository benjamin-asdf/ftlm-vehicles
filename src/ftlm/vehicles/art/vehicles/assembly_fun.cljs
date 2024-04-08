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
   ;; [tech.v3.dataset :as ds]
   [tech.v3.datatype :as dtype]
   ["mathjs" :as mathjs]
   [ftlm.vehicles.assembly-calculus :as ac]))

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

(defn gaussian
  [amplitude mean std-deviation x]
  (* amplitude
     (Math/exp (- (/ (Math/pow (- x mean) 2)
                     (* 2 (Math/pow std-deviation 2)))))))

;; neuron elements is an array of neurons
;; each neuron is active or not active
(defn dot-product [v1 v2]
  (dtype-fn/sum (dtype/emap (fn [a b] (* a b)) :float v1 v2)))

(defn ->synaptic-input-1
  [weights inputs]
  (for [i (range (count weights))]
    (dot-product (weights i) inputs)))

(defn normalize-weigths
  [weights]
  (into []
        (for [i (range (count weights))
              :let [weight (nth weights i)
                    sum (dtype-fn/sum weight)]]
          (if (zero? sum)
            weight
            (dtype/clone
             (dtype/emap
              (fn [w] (/ w sum))
              :float
              (nth weights i)))))))

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
                         (fn [e _s _]
                           (assoc e :hidden? true)))}
                 :transform
                 (lib/->transform pos 25 25 1)}))))
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
                       {:fade (fn [e _s _]
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
                            (fn [e _s _]
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


;; My neuronal dream
;; ---------------------------
;; this version was just to develop the look a bit
;; --------------------------
(defmethod setup-version :grid-1
  [state]
  (let [state (-> state
                  (lib/append-ents
                   (->neuronal-area-1
                    200
                    0.01
                    {:n-width 20
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

(defn ->input-space-ent
  []
  (lib/->entity :input-space
                {:color (:red controls/color-map)
                 :grid-width 5
                 :input-space? true
                 :spacing 20
                 :transform (lib/->transform
                              [(elib/from-right 500)
                               (elib/from-bottom 500)]
                              1
                              1
                              1)}))

(defn ->input-space-ac-2
  [n-neurons opts]
  (merge
    (->input-space-ent)
    {:ac-area {:activations #js [0] :n-neurons n-neurons}
     :color (:horizon controls/color-map)
     :draw-functions
     {:1 (fn [e]
           (let [neurons (into #{}
                               (ac/read-activations
                                (:ac-area e)))]
             (doall
              (for [i (range n-neurons)
                    :let [pos (elib/grid-pos e i)]]
                (do
                  (q/stroke-weight 1)
                  (q/with-stroke
                    controls/white
                    (q/with-fill
                      (lib/->hsb
                       (if (neurons i)
                         (:horizon controls/color-map)
                         (:woodsmoke
                          controls/color-map)))
                      (q/with-translation
                        pos
                        (q/rect 0 0 20 20 1.5)))))))))}
     :grid-width 5
     :input-space? true
     :spacing 20
     :transform (lib/->transform [(elib/from-right 500)
                                  (elib/from-bottom 500)]
                                 1
                                 1
                                 1)}
    opts))

(defn ->input-space-2
  [{:keys [n n-neurons projection-density]}]
  (lib/->entity
   :grid
   {:apparatus (ac/sensory-apparatus
                {:k-sensory-units n
                 :n-neurons n-neurons
                 :projection-density projection-density})
    :color (:red controls/color-map)
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
    :input-space? true
    :input-state (fn [e] (:elements e))
    :spacing 20
    :transform (lib/->transform [(elib/from-right 500)
                                 (elib/from-bottom 500)]
                                1
                                1
                                1)}))

(defn ->input-space-ac
  ([n n-neurons]
   (->input-space-ac
     {:n n :n-neurons n-neurons :projection-density 0.1}))
  ([{:as opts :keys [n n-neurons projection-density]}]
   (-> (->input-space-2 opts)
       (assoc
         :on-update-map
           {:sensitive-to-mouse
              (lib/every-n-seconds
                0.1
                (fn [e _s _]
                  (let [_y (q/mouse-y)
                        x (q/mouse-x)
                        inputs-identity
                          (int (* (/ (- (* (q/width) 0.9) x)
                                     (* (q/width) 0.9))
                                  n))]
                    (assoc e
                      :elements (dtype/clone
                                  (dtype/make-container
                                    :boolean
                                    (for [i (range n)]
                                      (boolean
                                        (= inputs-identity
                                           i)))))))))}))))

;; In the triangle world, there are 3 states.
;; and these transitions
;; 1->2, 2->3, 3->1, 2->1, 3->2, 1->3
;; The triangly world goes either left or right
;; The next state is determined by the current state and the direction

(defn triangle-world-1 [x direction]
  (get-in
   {0 {:left 1 :right 2}
    1 {:left 2 :right 0}
    2 {:left 0 :right 1}}
   [x direction]))

(def triangle-world-space
  {0 [true false false]
   1 [false true false]
   2 [false false true]})

(defn ->triangle-world
  [{:keys [n-neurons frequency]}]
  (-> (->input-space-ac 3 n-neurons)
      (assoc :world-state 0
             :on-update-map
             {:time-tick
              (lib/every-n-seconds
               (/ 1 frequency)
               (fn [e _ _]
                 (let [dir :left
                       world-state (:world-state e)
                       world-state (triangle-world-1 world-state dir)]
                   (-> e
                       (assoc :world-state world-state)
                       (assoc :elements
                              (triangle-world-space world-state))))))})))


;; ==============
;; Wavemaker (just an experiment)
;; ==============
;;
;; Idea:
;;
;; If you have a geometry in your neurons:
;; (geometry means that the graph of connections is not all over the place, but that there are
;; rules that make you more likely to be connected to somewhere,
;; presumably usually to your neighbours, other geometries are thinkable and potentially interesting).
;;

;; +---------------+          -+
;; |               |           | Here I roughly from 'columnar' cell assembles
;; |     A       A |          -+  (they are horizontal, saying column because it invokes cortical colums)
;; |        A      |   ^
;; |               |   |
;; |   B       B   |   v   ^
;; |        B      |       |
;; |   C  C    C   |       v
;; |     C         |
;; |     .         |
;; |     .         |
;; |               |
;; +---------------+

;; What could you do with a wave of activation going through the geometry?
;;
;;
;;
;;                     [ 'wave maker nucleus' ]     (see also sync-fire-chain)
;;                              |
;;                              |  imagine a slow fiber here
;;                              |
;; +-----------+      <---------+   t1
;; | |         | A              |
;; +-+---------+      <---------+   t2
;; | v |       | B              |
;; +---+-------+      <---------+   t3
;; |   v       | C              |
;; +-----------+                |

;; Here, A, B, C roughly correspond to cell assemblies forming from input
;; (remember cell assemblies are something like a datastructure that represent some information)
;;
;; wave maker:
;; A hypothetical element in the circuit that has the relatively stupid
;; wiring of making (timed) waves of activation through the geometry.
;; Limiting myself right now to imagining it goes top to bottom through.

;; The frequency of wave maker is sort of the time resolution for this event flow anchor mechanism.
;; This could also be implemented with a sync-fire-chain, listening to events in the system, instead of
;; static time.
;; --
;; I have called a very similar idea an n-clock somewhere, for neuron-clock or neuronal-clock.
;; Basically, you can count how often something happens (going around a clock of states) and something
;; else in your assemblies can base itself on that.
;; For instance, you can say 'event b happened 3 neuronal-events after event a'. Where neuronal-event is
;; already allowed to be arbitrarily abstract - i.e. events comming from somewhere in else in the cognition machine.
;; --
;; Basically we might be allowed to imagine wave-maker made from many neurons himself, so he could be
;; swapping from t1 to t2 only under certain circumstances.
;;
;; -> Still, it is joyful to try to come up with extremely simple elements, at least initially.
;;
;;
;;
;;
;; Consider this case,
;; 1. Inputs already ignited cell assembly A,
;;    In other words, A is in short term memory (the information representing the signal a)
;;    (signal-a, lower case; cell-assembly-A, upper case)
;; 2. The wave maker goes through the neuronal area. In front of A, the additional activation from wavemaker
;;    doesn't do anything (for sake of consideration).
;;    On top of A, it doesn't realy do anything because it is active already
;; 3. Now, just behind A, there is an interesting overlap in the circuit.
;;    It is that A is activating the area behind A a little bit and now wavemaker (time sayer) is activating it too!
;;    This area now represents a question: Is there any part of the system that would mean B
;;    Is there anything in our existing signal processing machinery that this potential B is already listening to,
;;    that would now allow it to fire?
;;    If it fires, hebbian plasticity already automatically strenghtes its inputs, calling them lower case b.
;;    Lower case b is the answer to the question what follows A in time.
;;    Keep in mind that A was active from short term memory to begin with.
;;    The relationship A->B is a 'follows in time' relationship
;;    Braitenberg called this an Ergotrix wire. (Vehicle 11 - Rules And Regularites).
;;    Other names: Causality wire, e-line (borrowed from ergotrix), trans-line, directed-association,
;;    Association through time
;;    very similar to Minsky Transframes
;;
;;    It is expecially satisfying that Hebb plasticity is resulting in both association lines (m-lines for Mnemotrix)
;;    and e-lines - simply because of the fact that cell assemblies represent information across time.
;;    Allowing the association wires to form between something in short term memory and something new comming in.
;;
;;
;;
;;                     [ 'wave maker nucleus' ]
;;                              |
;;                              |
;;                              |
;; +-----------+      <---------+   t1
;; | |         | A              |
;; +-+---------+      <---------+   t2
;; | v         | ???                                   !         -> B
;; +-----------+  <--------------------------------------- b
;; |           |
;; +-----------+

;; If this works the way I think then such a mechanism would find and represent those b-signals.

(comment (let [thickness 20
               n 3]
           (into []
                 (for [i (range n)]
                   (into
                    #{}
                    (repeatedly 10
                                #(+ (* i thickness) (rand-int thickness))))))))

(defn ->wavemaker
  [{:keys [n n-neurons wave-speed _height density color]}]
  (let [color (or color (:orange controls/color-map))
        height 300
        pos [400 80]
        height-of-1-element (int (/ height n))
        thickness (int (/ n-neurons n))
        projection (into []
                         (for [i (range n)]
                           (into #{}
                                 (repeatedly
                                   (* n-neurons density)
                                   #(+ (* i thickness)
                                       (rand-int
                                         thickness))))))
        element-position
          (fn [elm] [(first pos)
                     (+ (second pos)
                        (* elm height-of-1-element))])]
    (lib/->entity
      :wavemakers
      {:active-element-position
       (fn [e] (element-position (:active-elm e)))
       :active-elm 0
       :arousal-element? true
       :color color
       :components
       [(lib/->entity
         :line
         {:color (or color (:orange controls/color-map))
          :end-pos
          (update (element-position (dec n)) 1 + 30)
          :transform
          (lib/->transform [(first pos) 30] 1 1 1)})]
       :draw-functions
       {:draw
        (fn [{:keys [elements color]}]
          (doall
           (map-indexed
            (fn [idx element]
              (let [pos (element-position idx)]
                (q/stroke-weight 2)
                (q/with-stroke
                  (lib/->hsb color)
                  (q/with-fill
                    (lib/->hsb
                     (if element
                       color
                       (:woodsmoke
                        controls/color-map)))
                    (q/with-translation
                      pos
                      (q/ellipse 0 0 15 15))))))
            elements)))}
       :elements (->input-space-elements n)
       :grid-width 5
       :input-state (fn [e] (:elements e))
       :on-update-map
       {:u (lib/every-n-seconds
            (/ 1 wave-speed)
            (fn [e _s _]
              (let [next-active
                    (mod (inc (:active-elm e)) n)]
                (-> e
                    (assoc :active-elm next-active)
                    (update :elements
                            (fn [_]
                              (dtype/make-container
                               :boolean
                               (doall
                                (for [i (range n)]
                                  (if (= i next-active)
                                    true
                                    false))))))))))}
       :projection projection
       :spacing 20
       :transform (lib/->transform pos 1 1 1)})))

(defn wavemaker-lines [wavemaker-id nn-area-id]
  (fn [s _]
    (let [e ((lib/entities-by-id s) wavemaker-id)
          e-nn ((lib/entities-by-id s) nn-area-id)
          active-elm (:active-elm e)
          wavemaker-active-pos ((:active-element-position e) e)]
      (lib/append-ents
       s
       (for [i (take 1 (shuffle (nth (:projection e) active-elm)))]
         (do
           (assoc
            (elib/->flash-of-line
             wavemaker-active-pos
             ((:i->pos e-nn) e-nn i))
            :color (:color e))))))))

(defn wire-wave-maker
  [n-area wavemaker frequency]
  (assoc-in
   n-area
   [:on-update-map :wavemaker-input]
   (lib/every-n-seconds
    (/ 1 frequency)
    (fn [e s _k]
      (let [wavemaker ((lib/entities-by-id s)
                       (:id wavemaker))
            active-elm (:active-elm wavemaker)
            inputs (nth (:projection wavemaker)
                        active-elm)]
        (-> e
            (update :ac-area
                    ac/append-input
                    (into-array :int inputs))))))))


;; === prediction states ===
;;
;; I realized when you make prediction mechanisms, you might as well have the whole system
;; go 1 timestep ahead.
;;
;; The complexity of the mechanisms is the same and you edge out 1 timestep speedup
;; You can then be surprised about your inputs after the fact, and re-arrange your mental story
;; of what happens on fly and so forth.
;;
;; Prediction area would make sense if it is simply part of the cell assemblies of the high-dim areas
;; But perhaps you want it in an anatomically distinct location
;; My hunch is that derived thalamic nuclei could fulfill this role.
;; Then perhaps the thalamus is also implementing a comparator, which can make a surprise signal,
;; if the incomming inputs don't fit with the current prediction.
;;
;; Another benefit from this is that the rest of the system can treat your p-states as inputs,
;; with all the normal machinery that already evoloved for inputs.
;;
;;
(defn prediction-area [low-dim]
  (lib/->entity
   :grid
   {:color (:cyan controls/color-map)
    :input-space? true
    :prediction-area? true
    :draw-element
    (fn [active?]
      (q/with-stroke
        (lib/->hsb (:cyan controls/color-map))
        (lib/draw-color
         (if active?
           (:very-blue controls/color-map)
           (:woodsmoke controls/color-map)))
        (q/rect 0 0 20 20)))
    :elements (->input-space-elements low-dim)
    :input-state (fn [e] (:elements e))
    :grid-width 5
    :spacing 20
    :transform (lib/->transform
                [(elib/from-right 500)
                 (elib/from-bottom 525)]
                1 1 1)}))

;;
;; I realized I should make a primitive for making areas,
;; then fiber between 2 areas
;; then prediction area is allowed to be a n-area with low-dim and fibers to the high-dim areas
;;
;;
(defn wire-prediction-area
  [prediction-area n-area]
  (let [prediction-lines
        (into []
              (map (fn []
                     (into #{}
                           (repeatedly 10
                                       #(rand-int
                                         (:n-neurons
                                          n-area)))))
                   (range (count (:elements
                                  prediction-area)))))]
    (assoc-in prediction-area
              [:on-update-map :wire]
              (lib/every-n-seconds
               0.1
               (fn [e s _k]
                 (let [prediction-area ((lib/entities-by-id s)
                                        (:id prediction-area))
                       e-nn ((lib/entities-by-id s) (:id n-area))
                       activations (ac/read-activations (:ac-area
                                                         e-nn))
                       inputs (:elements prediction-area)
                       active? (into #{} (.valueOf activations))]
                   (-> e
                       (update :elements
                               (fn [_]
                                 (dtype/make-container
                                  :boolean
                                  (for [p-line prediction-lines]
                                    (if (some (fn [i]
                                                (contains? active?
                                                           i))
                                              p-line)
                                      true
                                      false))))))))))))




;;
;; Θ, big theta in the system
;; See Braitenberg 1978 - you can make associations by increasing and decreasing
;; threshold.
;;
;; Untuitively, you lower the neuron count, they have a chance to have overlap with mult.
;; cell assemblies.
;;
;; Then higher again -> you fall into multiple possible states
;; Looks like trains of thought.
;;
;; Maybe beta EEG is that you go low,high,low,high etc.
;; Means you have beta-freq * 2 neuron steps, and beta-freq trains of thought steps.
;;
;; Threshold instead of cap-k allows you to find especially well connected cell assemblies.
;; This is a satisfying perspective, because it maps onto what we percieve as 'everything fits' (G. Palm, e.g. 2015).
;;
;; Situation: threshold is low AND there is a large assembly.
;;
;;
;; -> looks a little bit like what we percieve as good ideas or good jokes
;;
;; Maybe you can get some development of memetic societies this way, too.
;; You find the memes that fit well, given an environment.
;;
;; Divergent thought might be making this threshold high->low->high->low move a lot.
;; Maybe doing the same thing in gamme freq.
;;
;; Counter intuitively, I think what is the case is that if I increase the threshold a lot,
;; I get more divergent thought.
;; Because a small number of neurons have a higher chance to be part of multiple assemblies (hunch).
;;
;;
;; Such a mechanism must be regulated feedback wise I suppose.
;; The concept of 'high', 'low' must be meaningful
;;
;; If you go in feedback with your area, it is actually the 'threshold' you set that is interesting
;; if you say threshold is high and then you have to lower and lower the threshold to get to the same
;; activation - that is an idea 'that fits well'.
;;

(defn ->dynamic-threshold-thought-pump
  [n-area]
  (lib/->entity
   :circle
   {:color (lib/with-alpha controls/white 0)
    :current-threshold 20
    ;; either high or low atm
    :threshold-mode :high
    ;; pumping means you are in flip mode,
    ;; otherwise you stablelize neuron count
    ;; presumably to hold an assembly
    :pumping? true
    :on-update-map
    ;; flip mode
    {:dynamic-threshold
     (lib/every-n-seconds
      0.1
      (fn [{:keys [current-threshold threshold-mode id
                   synaptic-threshold]} s _]
        {:updated-state
         (-> s
             (assoc-in
              [:eid->entity (:id n-area) :ac-area
               :inhibition-model]
              (fn [{:keys [activations]}
                   synaptic-input]
                (let [r (ac/threshold-inhibiton
                         synaptic-input
                         (* current-threshold
                            (mathjs/divide
                             (mathjs/sum
                              synaptic-input)
                             (:n-neurons
                              n-area))))]
                  (if (< (mathjs/count r) 1)
                    (ac/cap-k 2 synaptic-input)
                    r))))
             (assoc-in [:eid->entity id
                        :stroke-weight]
                       (+ 1
                          (* (lib/normalize-value-1
                              1
                              50
                              current-threshold)
                             10))))}))
     :mouse-senstive-threshold
     (lib/every-n-seconds
      0.1
      (fn [e s _]
        (let [new-threshold
              (min 50
                   (max 1
                        (int (* 40
                                (/ (q/mouse-y)
                                   (q/height))))))]
          (assoc e
                 :current-threshold new-threshold))))}
    :stroke controls/white
    :stroke-weight 10
    :transform (lib/->transform [200 200] 25 25 1)}))

(defn ->dynamic-threshold-thought-pump-1
  [n-area]
  (lib/->entity
    :circle
    {:color (lib/with-alpha controls/white 0)
     :current-threshold 20
     ;; either high or low atm
     :threshold-mode :high
     ;; pumping means you are in flip mode,
     ;; otherwise you stablelize neuron count
     ;; presumably to hold an assembly
     :pumping? true
     :on-update-map
       ;; flip mode
       {:dynamic-threshold
          (lib/every-n-seconds
            0.1
            (fn [{:keys [current-threshold threshold-mode id
                         synaptic-threshold]} s _]
              {:updated-state
                 (-> s
                     (assoc-in
                       [:eid->entity (:id n-area) :ac-area
                        :inhibition-model]
                       (fn [{:keys [activations]}
                            synaptic-input]
                         (ac/cap-k 50 synaptic-input)
                         ;; (let [r (ac/threshold-inhibiton
                         ;;          synaptic-input
                         ;;          (* current-threshold
                         ;;             10
                         ;;             (mathjs/divide
                         ;;              (mathjs/sum
                         ;;               synaptic-input)
                         ;;              (:n-neurons
                         ;;               n-area))))]
                         ;;   (if (< (mathjs/count r) 1)
                         ;;     (ac/cap-k 2 synaptic-input)
                         ;;     r))
                         ))
                     (assoc-in [:eid->entity id
                                :stroke-weight]
                               (+ 1
                                  (* (lib/normalize-value-1
                                       1
                                       50
                                       current-threshold)
                                     10))))}))}
     :stroke controls/white
     :stroke-weight 10
     :transform (lib/->transform [200 200] 25 25 1)}))



;; Idea
;;
;; Similar to wavemaker,
;; But this time the rest of the system is guiding it
;;
;; If you go high->low->high what says which direction you go? The rest of the system.
;; So now you can have thought-flow that is guided by the rest of the system.
;; Same idea like 'internal muscles'.
;; Actually 'music of thought' is quite cool, too
;; It the rhythm and melody of thought.
;;




;; Idea:
;;
;; Triangle world with a bump,
;; the bump is a 4th state with a imaginary darwinian value
;; The challenge is to build a neuronal area that will learn to go to the bump
;; Available
;; I. An oracle darwinian value giver, which gives a value
;; depending on the world state
;;
;; II. 2 magic output neurons that move the triangle world left or right
;;     If both are active, you move up, but only on the element that leads to the bump
;;
;;

(defn wire-input-space-1
  [{:keys [neuronal-area input-space frequency
           set-input-op]}]
  (assoc-in
   neuronal-area
   [:on-update-map :sensory-input]
   (lib/every-n-seconds
    (/ 1 frequency)
    (fn [e s _k]
      (let [{:keys [sensory-projection]} (:apparatus input-space)
            e-space ((lib/entities-by-id s)
                     (:id input-space))
            sensory-inputs ((:input-state e-space)
                            e-space)
            ;; new-activations
            inputs (ac/->sensory-inputs
                    sensory-inputs
                    sensory-projection)]
        (-> e
            (update :ac-area
                    (or set-input-op ac/set-input)
                    inputs)
            (assoc :color (:red controls/color-map)
                   :next-color
                   (let [next-colors
                         (atom (concat [:red :red]
                                       (cycle
                                        [:cyan])))]
                     (fn []
                       (let [[v] @next-colors]
                         (swap! next-colors next)
                         (v
                          controls/color-map)))))))))))

(defn ->flash-lines
  [s indices {:as e :keys [color]} n-area]
  (lib/append-ents s
                   (for [i indices]
                     (assoc (elib/->flash-of-line
                              (lib/position e)
                              ((:i->pos n-area) n-area i))
                            :color color))))

(defn wire-input-space-bursts
  [{:keys [neuronal-area input-space frequency set-input-op
           burst-count burst-frequency]}]
  (let [bursting-count (atom 0)]
    (->
     neuronal-area
     (assoc-in [:on-update-map :burst]
               (lib/every-n-seconds (/ 1 frequency)
                                    (fn [_]
                                      (swap! bursting-count
                                             (constantly
                                              burst-count))
                                      nil)))
     (assoc-in
      [:on-update-map :sensory-input]
      (lib/every-n-seconds
       (/ 1 burst-frequency)
       (fn [e s _k]
         (when (< 0 @bursting-count)
           (do
             (swap! bursting-count dec)
             (let [{:keys [sensory-projection]}
                   (:apparatus input-space)
                   e-space ((lib/entities-by-id s)
                            (:id input-space))
                   sensory-inputs ((:input-state e-space)
                                   e-space)
                   ;; new-activations
                   inputs (ac/->sensory-inputs
                           sensory-inputs
                           sensory-projection)
                   e (-> e
                         (update :ac-area
                                 (or set-input-op
                                     ac/append-input)
                                 inputs)
                         (assoc
                          :color
                          (:navajo-white
                           controls/color-map)))]
               {:updated-state
                (-> s
                    (assoc-in [:eid->entity (:id e)] e)
                    (lib/append-ents
                     (for [i (take 10
                                   (shuffle (.valueOf inputs)))]
                       (assoc
                        (elib/->flash-of-line
                         (lib/position e-space)
                         ((:i->pos neuronal-area)
                          ((lib/entities-by-id s) (:id neuronal-area))
                          i))
                        :color (:navajo-white
                                controls/color-map)))))})))))))))

(defn wire-input-space [neuronal-area input-space frequency]
  (wire-input-space-1
   {:neuronal-area neuronal-area
    :input-space input-space
    :frequency frequency}))

(defn ->neuronal-area-ac-1
  [{:as opts :keys [spacing grid-width draw-i]}]
  (lib/->entity
    :nn-area
    (merge
      {:color (:cyan controls/color-map)
       :draw-functions
         {:1 (fn [e]
               (let [neurons (ac/read-activations (:ac-area
                                                    e))
                     i->pos (fn [i] ((e :i->pos) e i))]
                 (q/with-stroke
                   nil
                   (doall (for [i neurons
                                :let [pos (i->pos i)]]
                            (q/with-translation
                              pos
                              (if draw-i
                                (draw-i i)
                                (q/rect 0 0 15 15 3))))))))}
       :i->pos (fn [{:keys [transform]} i]
                 (let [[x y] (:pos transform)
                       coll (mod i grid-width)
                       row (quot i grid-width)
                       x (+ x (* coll spacing))
                       y (+ y (* row spacing))]
                   [x y]))
       :next-color (constantly (:cyan controls/color-map))
       :spacing spacing}
      opts)))

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

(defmethod setup-version :grid
  [state]
  (let [input-space-size 10
        n-neurons 300
        n-area
        (->neuronal-area-ac
         {:color (:cyan controls/color-map)
          :draw-i (fn [_]
                    (q/with-stroke nil
                      (q/rect 0 0 25 25 2)))
          :frequency 10
          :grid-width 20
          :n-neurons n-neurons
          :spacing 25
          :transform
          (lib/->transform [100 100] 20 20 1)})
        n-area
        (-> n-area
            (assoc :ac-area
                   {:activations (into-array :int (take 20 (shuffle (range n-neurons))))
                    :inhibition-model
                    (fn [_ synaptic-input]
                      (ac/cap-k 25 synaptic-input))
                    :normalize-weights? true
                    :plasticity 0.01
                    :plasticity-model ac/hebbian-plasticity
                    :weights (ac/->random-directed-graph n-neurons 0.1)})
            (assoc-in
             [:on-update-map :normalize-weights]
             (lib/every-n-seconds
              5
              (fn [e _s _]
                (update-in e [:ac-area :weights] ac/normalize)))))
        id-area (:id n-area)
        input-space (->input-space-ac input-space-size
                                      n-neurons)
        n-area (wire-input-space n-area input-space (/ 1 3))
        state (-> state (assoc :neuronal-area (:id n-area))
                  (lib/append-ents [n-area input-space]))]

    (->
     state
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
                   (for [[i j] (partition-all 2 (take (* 3 2) (shuffle activations)))]
                     (lib/->entity
                      :multi-line
                      {:color (:cyan
                               controls/color-map)
                       :lifetime 3
                       :on-update-map
                       {:fade (elib/->fade 1)}
                       :stroke-weight 1
                       :transform (lib/->transform (i->pos i) 1 1 1)
                       :vertices
                       (elib/rect-line-vertices-1
                        (i->pos i)
                        (i->pos j))}))))]
           (-> s
               (lib/append-ents (show-lines s))))))))))

(defmethod setup-version :dots
  [state]
  (let [input-space-size 3
        n-neurons 500
        n-area
        (->neuronal-area-ac
         {:density 0.1
          :frequency 5
          :grid-width 20
          :n-neurons n-neurons
          :spacing 20
          :transform
          (lib/->transform [100 100] 20 20 1)})
        n-area
        (-> n-area
            (assoc :ac-area
                   {:activations (ac/->neurons
                                  n-neurons)
                    :inhibition-model
                    (fn [_ synaptic-input]
                      (ac/cap-k 30 synaptic-input))
                    :normalize-weights? true
                    :plasticity 0.01
                    :plasticity-model
                    ac/hebbian-plasticity
                    :weights (ac/->random-directed-graph n-neurons 0.2)})
            (assoc-in [:on-update-map :normalize-weights]
                      (lib/every-n-seconds
                       20
                       (fn [e _s _]
                         (update-in e [:ac-area :weights] ac/normalize)))))
        id-area (:id n-area)
        input-space (->input-space-ac input-space-size n-neurons)
        n-area (wire-input-space n-area input-space (/ 1 3))
        state (-> state
                  (assoc :neuronal-area (:id n-area))
                  (lib/append-ents [n-area input-space]))]

    (->
     state
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
                   (for [[i j] (partition-all 2 (take (* 3 2) (shuffle activations)))]
                     (lib/->entity
                      :multi-line
                      {:color (:cyan
                               controls/color-map)
                       :lifetime 3
                       :on-update-map
                       {:fade (elib/->fade 1)}
                       :stroke-weight 1
                       :transform (lib/->transform
                                   (i->pos i)
                                   1
                                   1
                                   1)
                       :vertices
                       (elib/rect-line-vertices-1
                        (i->pos i)
                        (i->pos j))}))))]
           (-> s
               (lib/append-ents (show-lines s))))))))))


(defmethod setup-version :geometry
  [state]
  (let [input-space-size 5
        n-neurons 600
        n-area
        (->neuronal-area-ac
         {:frequency 10
          :grid-width 20
          :n-neurons n-neurons
          :spacing 20
          :transform
          (lib/->transform [50 50] 20 20 1)})
        n-area
        (-> n-area
            (assoc :ac-area
                   {:activations (ac/->neurons n-neurons)
                    :weights
                    (ac/->directed-graph-with-geometry
                     n-neurons
                     (ac/lin-gaussian-geometry {:amplitude 0.6 :std-deviation 20}))
                    :inhibition-model (fn [_ synaptic-input] (ac/cap-k 30 synaptic-input))
                    :plasticity nil
                    :plasticity-model nil})
            (assoc-in [:on-update-map :normalize-weights]
                      (lib/every-n-seconds
                       20
                       (fn [e _s _]
                         (update-in e [:ac-area :weights] ac/normalize)))))
        id-area (:id n-area)
        input-space (->input-space-ac input-space-size
                                      n-neurons)
        n-area (wire-input-space n-area input-space 1)
        state (-> state
                  (assoc :neuronal-area (:id n-area))
                  (lib/append-ents [n-area input-space]))]
    (->
     state
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
                   (for [[i j] (partition-all 2 (take (* 3 2) (shuffle activations)))]
                     (lib/->entity
                      :multi-line
                      {:color (:cyan
                               controls/color-map)
                       :lifetime 3
                       :on-update-map
                       {:fade (elib/->fade 1)}
                       :stroke-weight 1
                       :transform (lib/->transform
                                   (i->pos i)
                                   1
                                   1
                                   1)
                       :vertices
                       (elib/rect-line-vertices-1
                        (i->pos i)
                        (i->pos j))}))))]
           (-> s
               (lib/append-ents (show-lines s))))))))))

(defmethod setup-version :geometry-p
  [state]
  (let [input-space-size 3
        n-neurons 400
        n-area
        (->neuronal-area-ac
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
                   {:activations (ac/->neurons n-neurons)
                    :weights
                    (ac/->directed-graph-with-geometry
                     n-neurons
                     (ac/lin-gaussian-geometry
                      {:amplitude 0.6 :std-deviation 30}))
                    :inhibition-model
                    (fn [_ synaptic-input]
                      (ac/cap-k 25 synaptic-input))
                    :plasticity 0.01
                    :plasticity-model ac/hebbian-plasticity})
            (assoc-in [:on-update-map :normalize-weights]
                      (lib/every-n-seconds
                       5
                       (fn [e _s _]
                         (update-in e [:ac-area :weights] ac/normalize)))))
        id-area (:id n-area)
        input-space (->input-space-ac input-space-size
                                      n-neurons)
        n-area (wire-input-space n-area input-space 1)
        state (-> state
                  (assoc :neuronal-area (:id n-area))
                  (lib/append-ents [n-area input-space]))]


    (->
     state
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
                       i->pos (fn [i] ((e :i->pos) e i))]
                   (for [[i j] (partition-all 2 (take (* 3 2) (shuffle activations)))
                         :when (and i j)]
                     (lib/->entity
                      :multi-line
                      {:color (:cyan
                               controls/color-map)
                       :lifetime 3
                       :on-update-map
                       {:fade (elib/->fade 1)}
                       :stroke-weight 1
                       :transform (lib/->transform
                                   (i->pos i)
                                   1
                                   1
                                   1)
                       :vertices
                       (elib/rect-line-vertices-1
                        (i->pos i)
                        (i->pos j))}))))]
           (-> s
               (lib/append-ents (show-lines s))))))))))


(defmethod setup-version :burst-inputs
  [state]
  (let [input-space-size 3
        n-neurons 1000

        n-area (->neuronal-area-ac
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
                   {:activations (ac/->neurons
                                  n-neurons)
                    :inhibition-model
                    (fn [{:keys [activations]}
                         synaptic-input]
                      ;; (ac/cap-k
                      ;;  (rand-nth [50 100])
                      ;;  synaptic-input)
                      (ac/cap-k (rand-nth [50 100]) synaptic-input))
                    :plasticity 0.1
                    :plasticity-model
                    ac/hebbian-plasticity
                    :weights
                    (ac/->directed-graph-with-geometry
                     n-neurons
                     (ac/lin-gaussian-geometry {:amplitude 0.3 :std-deviation 10}))})
            (assoc-in [:on-update-map :normalize-weights]
                      (lib/every-n-seconds
                       5
                       (fn [e _s _]
                         (update-in e
                                    [:ac-area :weights]
                                    ac/normalize)))))
        id-area (:id n-area)
        input-space
        (->input-space-ac
         {:n input-space-size
          :n-neurons n-neurons
          :projection-density (* 0.1 (/ 100 n-neurons input-space-size))})
        n-area (wire-input-space-bursts
                {:burst-count 10
                 :burst-frequency 10
                 :frequency (/ 1 3)
                 :input-space input-space
                 :neuronal-area n-area
                 :set-input-op ac/append-input})

        wavemaker (->wavemaker
                   {:n 15
                    :n-neurons n-neurons
                    :density 0.1
                    :wave-speed 2})
        n-area
        (let [frequency 5]
          (assoc-in n-area
                    [:on-update-map :wavemaker-input]
                    (lib/every-n-seconds
                     (/ 1 frequency)
                     (fn [e s _k]
                       (let [wavemaker ((lib/entities-by-id s)
                                        (:id wavemaker))
                             active-elm (:active-elm wavemaker)
                             inputs (nth (:projection wavemaker)
                                         active-elm)]
                         (-> e
                             (update :ac-area
                                     ac/append-input
                                     (into-array :int
                                                 inputs))))))))

        state (-> state
                  (assoc :neuronal-area (:id n-area))
                  (lib/append-ents [n-area input-space wavemaker]))]
    (-> state
        (assoc-in
         [:on-update-map :wavemaker-lines]
         (lib/every-n-seconds
          (/ 1 5)
          (wavemaker-lines
           (:id wavemaker)
           (:id n-area))))

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


(defmethod setup-version :burst-inputs-triangle-world
  [state]
  (let [input-space-size 3
        n-neurons 1000
        n-area (->neuronal-area-ac
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
                   {:activations (ac/->neurons
                                  n-neurons)
                    :inhibition-model
                    (fn [{:keys [activations]}
                         synaptic-input]
                      ;; (ac/cap-k
                      ;;  (rand-nth [50 100])
                      ;;  synaptic-input)
                      (ac/cap-k (rand-nth [50 100]) synaptic-input))
                    :plasticity 0.1
                    :plasticity-model
                    ac/hebbian-plasticity
                    :weights
                    (ac/->directed-graph-with-geometry
                     n-neurons
                     (ac/lin-gaussian-geometry {:amplitude 0.3 :std-deviation 10}))})
            (assoc-in [:on-update-map :normalize-weights]
                      (lib/every-n-seconds
                       5
                       (fn [e _s _]
                         (update-in e
                                    [:ac-area :weights]
                                    ac/normalize)))))
        id-area (:id n-area)

        input-space (->triangle-world
                     {:frequency 1
                      :n-neurons
                      n-neurons})

        n-area (wire-input-space-bursts
                {:burst-count 10
                 :burst-frequency 10
                 :frequency (/ 1 3)
                 :input-space input-space
                 :neuronal-area n-area
                 :set-input-op ac/append-input})

        wavemaker (->wavemaker
                   {:n 10
                    :n-neurons n-neurons
                    :density 0.1
                    :wave-speed 10})
        n-area
        (let [frequency 5]
          (assoc-in n-area
                    [:on-update-map :wavemaker-input]
                    (lib/every-n-seconds
                     (/ 1 frequency)
                     (fn [e s _k]
                       (let [wavemaker ((lib/entities-by-id s)
                                        (:id wavemaker))
                             active-elm (:active-elm wavemaker)
                             inputs (nth (:projection wavemaker)
                                         active-elm)]
                         (-> e
                             (update :ac-area
                                     ac/append-input
                                     (into-array :int
                                                 inputs))))))))

        state (-> state
                  (assoc :neuronal-area (:id n-area))
                  (lib/append-ents [n-area input-space wavemaker]))]
    (-> state
        (assoc-in
         [:on-update-map :wavemaker-lines]
         (lib/every-n-seconds
          (/ 1 5)
          (wavemaker-lines
           (:id wavemaker)
           (:id n-area))))

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

(defmethod setup-version :inhibition-pump-mouse
  [state]
  (let [input-space-size 10
        n-neurons 500
        n-area (->neuronal-area-ac
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
                   {:activations (mathjs/range 0 10)
                    :plasticity 0.1
                    :plasticity-model
                    ac/hebbian-plasticity
                    :weights
                    (ac/->directed-graph-with-geometry
                     n-neurons
                     (ac/lin-gaussian-geometry
                      {:amplitude 0.3
                       :std-deviation 10}))})
            (assoc-in [:on-update-map :normalize-weights]
                      (lib/every-n-seconds
                       5
                       (fn [e _s _]
                         (update-in e
                                    [:ac-area :weights]
                                    ac/normalize)))))
        id-area (:id n-area)
        big-theta (->dynamic-threshold-thought-pump n-area)
        state (-> state
                  (assoc :neuronal-area (:id n-area))
                  (lib/append-ents [n-area
                                    ;; input-space
                                    big-theta]))]
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

(defmethod setup-version :thought-pump-rhythm
  [state]
  (let [input-space-size 10
        n-neurons 500
        n-area (->neuronal-area-ac
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
                   {:activations (mathjs/range 0 10)
                    :plasticity 0.1
                    :plasticity-model
                    ac/hebbian-plasticity
                    :weights
                    (ac/->directed-graph-with-geometry
                     n-neurons
                     (ac/lin-gaussian-geometry
                      {:amplitude 0.3
                       :std-deviation 10}))})
            (assoc-in [:on-update-map :normalize-weights]
                      (lib/every-n-seconds
                       5
                       (fn [e _s _]
                         (update-in e
                                    [:ac-area :weights]
                                    ac/normalize)))))
        id-area (:id n-area)
        big-theta (->dynamic-threshold-thought-pump-1 n-area)
        big-theta
        (assoc-in big-theta
                  [:on-update-map :rhythm]
                  (lib/every-n-seconds
                   1
                   (fn [{:as e :keys [threshold-mode]} s _]
                     (-> e
                         (update-in [:threshold-mode]
                                    {:high :low :low :high})
                         (update-in [:current-threshold]
                                    (fn [theta]
                                      (case threshold-mode
                                        :high 30
                                        :low 1)))))))
        state (-> state
                  (assoc :neuronal-area (:id n-area))
                  (lib/append-ents [n-area
                                    ;; input-space
                                    big-theta]))]
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


(defmethod setup-version :thought-pump-rhythm-1
  [state]
  (let [input-space-size 10
        n-neurons 500
        n-area (->neuronal-area-ac
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
                       {:activations (mathjs/range 0 10)
                        :plasticity 0.1
                        :plasticity-model
                          ac/hebbian-plasticity
                        :weights
                          (ac/->directed-graph-with-geometry
                            n-neurons
                            (ac/lin-gaussian-geometry
                              {:amplitude 0.3
                               :std-deviation 10}))})
              (assoc-in [:on-update-map :normalize-weights]
                        (lib/every-n-seconds
                          5
                          (fn [e _s _]
                            (update-in e
                                       [:ac-area :weights]
                                       ac/normalize)))))
        id-area (:id n-area)
        big-theta (->dynamic-threshold-thought-pump-1
                    n-area)
        big-theta
          (assoc-in big-theta
            [:on-update-map :rhythm]
            (let [passed (atom 0)]
              (fn [e s _]
                (swap! passed + lib/*dt*)
                (update-in
                  e
                  [:current-threshold]
                  (fn [theta]
                    (max 1
                         (* 50 (lib/normal-distr 0.5 0.5)))
                    ;; period: 0.5 s
                    ;; amplitude: 50
                    ;; phase: 0
                    ;; (let [min 1]
                    ;;   (+ min (* 50 (q/sin (mod (*
                    ;;   @passed 8) q/PI)))))
                  )))))
        state (-> state
                  (assoc :neuronal-area (:id n-area))
                  (lib/append-ents [n-area
                                    ;; input-space
                                    big-theta]))]
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


(defmethod setup-version :prediction-area
  [state]
  (let [input-space-size 3
        n-neurons 500
        n-area (->neuronal-area-ac
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
                   {:activations (mathjs/range 0 10)
                    :plasticity 0.1
                    :plasticity-model
                    ac/hebbian-plasticity
                    :weights
                    (ac/->directed-graph-with-geometry
                     n-neurons
                     (ac/lin-gaussian-geometry
                      {:amplitude 0.3
                       :std-deviation 10}))})
            (assoc-in [:on-update-map :normalize-weights]
                      (lib/every-n-seconds
                       5
                       (fn [e _s _]
                         (update-in e
                                    [:ac-area :weights]
                                    ac/normalize)))))
        id-area (:id n-area)

        input-space (->input-space-ac-2 3 {})

        input-space (merge
                     input-space
                     {:world-state 0
                      :on-update-map
                      {:triangle-world
                       (lib/every-n-seconds
                        0.1
                        (fn [e s _]
                          (let [e (update-in e [:world-state] (fn [ws] (mod (inc ws) 3)))]
                            (-> e
                                (update-in
                                 [:ac-area :activations]
                                 #js [(:world-state e)])))))}})

        ;; n-area (wire-input-space-1 {:frequency (/ 1 3)
        ;;                             :input-space input-space
        ;;                             :neuronal-area n-area
        ;;                             :set-input-op
        ;;                             ac/append-input})

        prediction-area (prediction-area 3)
        prediction-area (wire-prediction-area prediction-area n-area)


        ;; n-area
        ;; (wire-input-space-bursts
        ;;  {:burst-count 10
        ;;   :burst-frequency 10
        ;;   :frequency (/ 1 3)
        ;;   :input-space input-space
        ;;   :neuronal-area n-area
        ;;   :set-input-op ac/append-input})
        wavemaker (->wavemaker
                   {:color (q/lerp-color
                            (lib/->hsb controls/white)
                            (lib/->hsb
                             (:orange
                              controls/color-map))
                            0.3)
                    :density 0.001
                    :n 4
                    :n-neurons n-neurons
                    :wave-speed 1})
        n-area (wire-wave-maker n-area wavemaker 5)
        big-theta (->dynamic-threshold-thought-pump-1
                   n-area)
        big-theta
        (assoc-in big-theta
                  [:on-update-map :rhythm]
                  (lib/every-n-seconds
                   1
                   (fn [{:as e :keys [threshold-mode]} s _]
                     (-> e
                         (update-in [:threshold-mode]
                                    {:high :low :low :high})
                         (update-in [:current-threshold]
                                    (fn [theta]
                                      (case threshold-mode
                                        :high 30
                                        :low 1)))))))
        state (-> state
                  (assoc :neuronal-area (:id n-area))
                  (lib/append-ents [n-area wavemaker
                                    input-space big-theta
                                    prediction-area]))]
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
                    (lib/append-ents (show-lines s)))))))
        (assoc-in [:on-update-map :wavemaker-lines]
                  (lib/every-n-seconds (/ 1 2)
                                       (wavemaker-lines
                                         (:id wavemaker)
                                         (:id n-area)))))))

(defmethod setup-version :binary-synapses
  [state]
  (let [n-neurons 500
        n-area (->neuronal-area-ac-1
                 {:density 0.1
                  :frequency 10
                  :grid-width 20
                  :n-neurons n-neurons
                  :spacing 15
                  :transform (lib/->transform [50 50] 20 20 1)})
        n-area
          (-> n-area
              (assoc :ac-area
                       {:activations #js []
                        :inhibition-model
                        (fn [{:keys [activations]}
                             synaptic-input]
                          (ac/cap-k 20 synaptic-input))
                        :n-neurons n-neurons
                        :plasticity 0.1
                        :plasticity-model
                        ac/binary-hebbian-plasticity
                        :weights
                        (ac/->random-directed-graph
                         n-neurons
                         0.01)})
              (assoc-in
                [:on-update-map :normalize-weights]
                (lib/every-n-seconds
                  5
                  (fn [e _s _]
                    (update-in
                      e
                      [:ac-area :weights]
                      (fn [w]
                        (println "pruning " (mathjs/sum w))
                        (let [r (ac/binary-prune-synapses
                                  w
                                  0.1)]
                          (println "synapses left: "
                                   (mathjs/sum w))
                          r)))))))
        id-area (:id n-area)
        state (-> state
                  (assoc :neuronal-area (:id n-area))
                  (lib/append-ents [n-area])
                  (assoc :tick 0))
        time-tick
          (lib/every-n-seconds
            0.1
            (fn [{:as s :keys [_input-bursting? tick]} _]
              (let [lst (partition-all 20 (concat
                                           (range n-neurons)
                                           (range n-neurons)))
                    pretend-input
                    (when (< tick (count lst)) (nth lst tick))
                    append-inputs
                    (fn [s]
                      (-> s
                          (update-in
                           [:eid->entity (:id n-area)
                            :ac-area]
                           ac/append-input
                           (into-array
                            :int
                            pretend-input))))
                    s (->
                       s
                       (update :tick (fnil inc 0))
                       (update-in [:eid->entity
                                   (:id n-area) :ac-area]
                                  ac/update-neuronal-area)
                       (assoc-in
                        [:eid->entity (:id n-area) :color]
                        (if pretend-input
                          (:red controls/color-map)
                          (:cyan controls/color-map)))
                       (assoc-in
                        [:eid->entity (:id n-area)
                         :ac-area :inhibition-model]
                        (fn [{:keys [_activations]}
                             synaptic-input]
                          (ac/cap-k (if (odd? tick) 20 10)
                                    synaptic-input))))]
                ;;
                ;; later: comparator
                ;; input space -> n-area
                ;; step n area
                ;;
                ;;
                ;;
                ;;
                (if pretend-input (append-inputs s) s))))]
    (-> state
        (assoc-in [:on-update-map :time-tick] time-tick)
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
                    (lib/append-ents (show-lines s))))))))))



(defmethod setup-version :binary-synapses-2
  [state]
  (let [n-neurons 500
        n-area (->neuronal-area-ac-1
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
                   {:activations #js[]
                    :plasticity 0.1
                    :n-neurons n-neurons
                    :plasticity-model
                    ac/binary-hebbian-plasticity
                    :weights
                    ;; (ac/->random-directed-graph
                    ;; n-neurons 0.0)
                    ;; (ac/->random-directed-graph
                    ;; 3 0.0)
                    (ac/->directed-graph-with-geometry
                     n-neurons
                     (ac/lin-gaussian-geometry
                      {:amplitude 0.3
                       :std-deviation 10}))})
            (assoc-in
             [:on-update-map :normalize-weights]
             (lib/every-n-seconds
              5
              (fn [e _s _]
                (update-in
                 e
                 [:ac-area :weights]
                 (fn [w]
                   (println "pruning " (mathjs/sum w))
                   (let [r (ac/binary-prune-synapses
                            w
                            0.1)]
                     (println "synapses left: "
                              (mathjs/sum w))
                     r)))))))
        id-area (:id n-area)
        input-space (->input-space-ac-2 3 {})
        input-space
        (merge
         input-space
         {:on-update-map
          {:triangle-world
           (lib/every-n-seconds
            3
            (fn [e s _]
              (let [e (update-in
                       e
                       [:world-state]
                       (fn [ws] (mod (inc ws) 3)))]
                (-> e
                    (assoc-in
                     [:ac-area :activations]
                     #js[(:world-state e)])))))}
          :world-state 0})
        input-fiber
        (ac/->input-fiber
         (:ac-area input-space)
         (:ac-area n-area)
         0.01)

        ;; n-area
        ;; (wire-input-space-1
        ;;  {:frequency (/ 1 3)
        ;;   :input-space input-space
        ;;   :neuronal-area n-area
        ;;   :set-input-op
        ;;   ac/append-input})
        ;; n-area
        ;; (wire-input-space-bursts
        ;;  {:burst-count 10
        ;;   :burst-frequency 10
        ;;   :frequency (/ 1 3)
        ;;   :input-space input-space
        ;;   :neuronal-area n-area
        ;;   :set-input-op ac/append-input})

        ;; wavemaker (->wavemaker
        ;;             {:color (q/lerp-color
        ;;                       (lib/->hsb controls/white)
        ;;                       (lib/->hsb
        ;;                         (:orange
        ;;                           controls/color-map))
        ;;                       0.3)
        ;;              :density 0.001
        ;;              :n 4
        ;;              :n-neurons n-neurons
        ;;              :wave-speed 1})

        ;; n-area (wire-wave-maker n-area wavemaker 5)
        big-theta (->dynamic-threshold-thought-pump-1
                   n-area)
        big-theta
        (assoc-in big-theta
                  [:on-update-map :rhythm]
                  (lib/every-n-seconds
                   1
                   (fn [{:as e :keys [threshold-mode]} s _]
                     (-> e
                         (update-in [:threshold-mode]
                                    {:high :low :low :high})
                         (update-in [:current-threshold]
                                    (fn [theta]
                                      (case threshold-mode
                                        :high 30
                                        :low 1)))))))
        state (-> state
                  (assoc :neuronal-area (:id n-area))
                  (lib/append-ents [n-area
                                    ;; wavemaker
                                    input-space big-theta]))
        ;; input-fiber (ac/->input-fiber)



        time-tick
        (lib/every-n-seconds
         0.01
         (fn [{:as s :keys [input-bursting? tick]} _]
           (let [input-space
                 ((lib/entities-by-id s)
                  (:id input-space))
                 input-space-area
                 (:ac-area input-space)
                 input-bursting? (< 10 (mod tick 20))



                 flash-input-lines
                 (fn [s indices]
                   (->flash-lines
                    s
                    (take 10 (shuffle (.valueOf indices)))
                    input-space
                    ((lib/entities-by-id s) (:id n-area))))

                 append-inputs
                 (fn [s]
                   (let [inputs
                         (ac/input-fiber-activations input-space-area input-fiber)]
                     (-> s
                         ;; (update-in [:eid->entity (:id n-area) :ac-area] ac/append-input inputs)
                         (update-in [:eid->entity (:id n-area) :ac-area] ac/set-input inputs)
                         (flash-input-lines inputs))))

                 s (-> s
                       (update :tick (fnil inc 0))
                       (update-in
                        [:eid->entity (:id n-area)
                         :ac-area]
                        ac/update-neuronal-area))
                 s (cond-> s
                     input-bursting? append-inputs)]
             ;;
             ;; later: comparator
             ;; input space -> n-area
             ;; step n area
             ;;
             ;;
             ;;
             ;;
             s)))]
    (-> state
        (assoc-in [:on-update-map :time-tick] time-tick)
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
        ;; (assoc-in
        ;;  [:on-update-map :wavemaker-lines]
        ;;  (lib/every-n-seconds
        ;;   (/ 1 2)
        ;;   (wavemaker-lines
        ;;    (:id wavemaker)
        ;;    (:id n-area))))
        )))

(defmethod setup-version :triangle-world
  [state]
  (let [n-neurons 600
        n-area (->neuronal-area-ac
                {:density 0.1
                 :frequency 10
                 :grid-width 20
                 :n-neurons n-neurons
                 :spacing 15
                 :transform
                 (lib/->transform [50 50] 20 20 1)})
        n-area
        (-> n-area
            (assoc
             :ac-area
             {:activations (ac/->neurons n-neurons)
              :inhibition-model
              (fn [{:keys [activations]} synaptic-input]
                (ac/cap-k 50 synaptic-input))
              :plasticity 0.1
              :plasticity-model ac/hebbian-plasticity
              :weights
              (ac/->directed-graph-with-geometry
               n-neurons
               (ac/lin-gaussian-geometry
                {:amplitude 0.1
                 :std-deviation 100}))})
            (assoc-in [:on-update-map :normalize-weights]
                      (lib/every-n-seconds
                       5
                       (fn [e _s _]
                         (update-in e
                                    [:ac-area :weights]
                                    ac/normalize)))))
        id-area (:id n-area)
        input-space (->triangle-world
                     {:frequency (/ 1 5)
                      :n-neurons
                      n-neurons})
        n-area (wire-input-space-1
                {:frequency (/ 1 3)
                 :neuronal-area n-area
                 :input-space input-space
                 :set-input-op ac/append-input})
        state (-> state
                  (assoc :neuronal-area (:id n-area))
                  (lib/append-ents [n-area input-space]))]
    (->
     state
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
                                (take (* 3 2)
                                      (shuffle
                                       activations)))
                         :when (and i j)]
                     (lib/->entity
                      :multi-line
                      {:color (:cyan
                               controls/color-map)
                       :lifetime 3
                       :on-update-map
                       {:fade (elib/->fade 1)}
                       :stroke-weight 1
                       :transform (lib/->transform
                                   (i->pos i)
                                   1
                                   1
                                   1)
                       :vertices
                       (elib/rect-line-vertices-1
                        (i->pos i)
                        (i->pos j))}))))]
           (-> s
               (lib/append-ents (show-lines s))))))))))

(defmethod setup-version
  :triangle-world-and-geometry-timer-wave
  [state]
  (let [n-neurons 400
        n-area (->neuronal-area-ac
                 {:density 0.2
                  :frequency 20
                  :grid-width 20
                  :n-neurons n-neurons
                  :spacing 15
                  :transform
                    (lib/->transform [50 50] 20 20 1)})
        n-area
          (-> n-area
              (assoc
                :ac-area
                  {:activations (ac/->neurons n-neurons)
                   :inhibition-model
                   (fn [{:keys [activations]}
                        synaptic-input]
                     (ac/cap-k 50 synaptic-input)
                     ;; (ac/cap-k
                     ;;  (max (gaussian
                     ;;        60
                     ;;        15
                     ;;        20
                     ;;        (count (.valueOf
                     ;;                activations)))
                     ;;       1)
                     ;;  synaptic-input)
                     )
                   :plasticity 0.1
                   :plasticity-model nil
                   ;; ac/hebbian-plasticity
                   :weights
                   (ac/->directed-graph-with-geometry
                    n-neurons
                    (ac/lin-gaussian-geometry
                     {:amplitude 0.2
                      :std-deviation 30}))})
              (assoc-in [:on-update-map :normalize-weights]
                        (lib/every-n-seconds
                          5
                          (fn [e _s _]
                            (update-in e
                                       [:ac-area :weights]
                                       ac/normalize)))))
        wavemaker (->wavemaker
                   {:n 5
                    :n-neurons n-neurons
                    :density 0.1
                    :wave-speed 5})
        id-area (:id n-area)
        input-space (->triangle-world
                     {:frequency (/ 1 5)
                      :n-neurons
                      n-neurons})
        n-area (wire-input-space-1
                {:frequency (/ 1 3)
                 :input-space input-space
                 :neuronal-area n-area
                 :set-input-op
                 ac/append-input})
        n-area
        (let [frequency 10]
            (assoc-in n-area
              [:on-update-map :wavemaker-input]
              (lib/every-n-seconds
                (/ 1 frequency)
                (fn [e s _k]
                  (let [wavemaker ((lib/entities-by-id s)
                                    (:id wavemaker))
                        active-elm (:active-elm wavemaker)
                        inputs (nth (:projection wavemaker)
                                    active-elm)]
                    (-> e
                        ;; (update :ac-area
                        ;; ac/set-input (into-array
                        ;; :int inputs))
                        (update :ac-area
                                ac/append-input
                                (into-array :int
                                            inputs))))))))
        state (-> state
                  (assoc :neuronal-area (:id n-area))
                  (lib/append-ents [n-area input-space
                                    wavemaker]))]
    (-> state
        (assoc-in
          [:on-update-map :time-tick]
          (lib/every-n-seconds
            0.2
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
        (assoc-in
          [:on-update-map :wavemaker-lines]
          (lib/every-n-seconds
           (/ 1 5)
           (wavemaker-lines
            (:id wavemaker)
            (:id n-area)))))))


(defmethod setup-version
  :wavemaker-without-world
  [state]
  (let [n-neurons 400
        n-area (->neuronal-area-ac
                {:density 0.1
                 :frequency 20
                 :grid-width 20
                 :n-neurons n-neurons
                 :spacing 15
                 :transform
                 (lib/->transform [50 50] 20 20 1)})
        n-area
        (-> n-area
            (assoc
             :ac-area
             {:activations (ac/->neurons n-neurons)
              :inhibition-model
              (fn [{:keys [activations]}
                   synaptic-input]
                (ac/cap-k
                 (max (gaussian
                       60 30
                       10 (count (.valueOf
                                  activations)))
                      1)
                 synaptic-input))
              :plasticity 0.05
              :plasticity-model ac/hebbian-plasticity
              :weights
              (ac/->directed-graph-with-geometry
               n-neurons
               (ac/lin-gaussian-geometry
                {:amplitude 0.6
                 :std-deviation 30}))})
            (assoc-in
             [:on-update-map :normalize-weights]
             (lib/every-n-seconds
              5
              (fn [e _s _]
                (update-in e
                           [:ac-area :weights]
                           ac/normalize)))))
        wavemaker (->wavemaker
                   {:n 5
                    :density 0.02
                    :n-neurons n-neurons
                    :wave-speed 5})
        id-area (:id n-area)
        n-area
        (let [frequency 5]
          (assoc-in n-area
                    [:on-update-map :wavemaker-input]
                    (lib/every-n-seconds
                     (/ 1 frequency)
                     (fn [e s _k]
                       (let [wavemaker ((lib/entities-by-id s)
                                        (:id wavemaker))
                             active-elm (:active-elm wavemaker)
                             inputs (nth (:projection wavemaker) active-elm)]
                         (-> e
                             (update :ac-area ac/append-input (into-array :int inputs))))))))
        state (-> state
                  (assoc :neuronal-area (:id n-area))
                  (lib/append-ents [n-area wavemaker]))]
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
                  (lib/append-ents (show-lines s)))))))
        (assoc-in
         [:on-update-map :wavemaker-lines]
         (lib/every-n-seconds
          (/ 1 5)
          (wavemaker-lines
           (:id wavemaker)
           (:id n-area)))))))

(defmethod setup-version :bouncy-ball
  [state]
  (let [set-point (lib/mid-point)]
    (-> state
        (lib/append-ents
          [(lib/->entity
             :circle
             {:color controls/white
              :draggable? true
              :on-update-map
              {:f
               (fn [e s _]
                 (if-not (:dragged? e)
                   (-> (lib/orient-towards e set-point)
                       (assoc :acceleration 100))
                   e))}
              :transform
              (lib/->transform set-point 30 30 1)})]))))

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
