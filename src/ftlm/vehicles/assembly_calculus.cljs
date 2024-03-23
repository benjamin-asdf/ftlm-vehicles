(ns ftlm.vehicles.assembly-calculus
  (:require
   [tech.v3.datatype.argops :as argops]
   [tech.v3.datatype.functional :as dtype-fn]
   [tech.v3.datatype :as dtype]
   ["mathjs" :as mathjs]))

;; paper: https://arxiv.org/abs/2110.03171

;;
;; Assemblies!
;; Sanotsh Vempala's cell asssembly inspired model
;; "Let's assume the brain is implementing an assembly calculus"
;;

;; https://github.com/mdabagia/learning-with-assemblies

;; insert Braitenberg quote, musing about the structure of cell assemblies (1977)
;; []

;; Braibenberg is stretching them up to memes and memeplexes!
;; But it is still all the activity of the cell assemblies.
;; Thus, we firmly get into the territory of traversing intermediate space between
;; activations and cognition
;; -> the cell assemblies are a datastructure, a building material
;;
;;
;; cognition
;; perception, memory, thought, ...
;;
;;
;;
;; -----------  abstraction barrier -------------------------
;;
;; substance of cognition
;;                         you can implement the substance with cell assemblies
;;                         or hypervectors
;;                         or McCulloch-Pitts activations, too
;;

;;
;; Thus, you don't think of cell assemblies when implementing a cognition machine.
;; (it would be a level violation)
;; Having both hypervectors and cell-assemblies as possible implementations is great.
;; It allows us to think about the cognition machine separately, keeping in mind that the
;; lower level implementation might swap.

;; ---- Cell assemblies ----

;; Essentials:
;; 1. Hebbian plasticity - fire together, wire together
;; 2. Directed graph of excitatory activations with dynamic edge weights
;; 3. Discrete timesteps
;; 4. Inhibition (Braitenberg calls this a thought pump)
;;

;;
;; The ignition of this stuff is from Hebb.
;;

;;
;; I. Devide the brain into areas containg n activations connected through a G(n,p) directed graph
;; II. implement a k-cap selection, the top k excitied activations fire at the next time step


;; Implementation
;; thank you mathjs
;; https://dev.to/mrinasugosh/ml-fundamentals-in-javascript-5fm5




;; +----------------+
;; +----------------+  higher probably within a row

;; +----------------+
;; +----------------+


;; The other one I want to try

;;                /\
;;              /-  \
;;            /-     \
;;         /--        \-
;;       /-             \
;;     /-                \
;;   --                   \
;; ------------------------------
;;  coll -1, coll x , coll + 1, ...

;; then everbody inside a row (or coll, whatever) is connected more and then spreads out to the neighbours


;;
;; Allowing self-connections
;;

;; geometry model takes 3 args, the neuron that will recieve the connection (i)
;; the neurons that will provide the connection (j)
;; it returns a connection probability

(defn ->directed-graph-with-geometry
  [n-neurons geometry-model]
  (.map (mathjs/matrix (mathjs/zeros #js [n-neurons
                                          n-neurons])
                       "sparse")
        (fn [_ idx _]
          (if (< (mathjs/random 0 1)
                 (geometry-model (aget idx 0) (aget idx 1)))
            1
            0))))

(defn ->random-directed-graph
  [n-neurons density]
  (->directed-graph-with-geometry
   n-neurons
   (fn [_ _]
     (< (mathjs/random 0 1) density))))

(defn ->uni-directional-fiber
  [n-neurons-1 n-neurons-2 density]
  (.map (mathjs/matrix (mathjs/zeros #js [n-neurons-1
                                          n-neurons-2])
                       "sparse")
        (fn [_ _ _] (if (< (mathjs/random) density) 1 0))))


;; Synaptic input is the sum of the weights of the active activations
;; Everybody that is active is contributing to my chance of being active

(defn synaptic-input
  [weights activations]
  (let [n (.get (mathjs/squeeze (mathjs/size weights)) #js[1])]
    (-> (mathjs/subset weights
                       (mathjs/index activations
                                     (mathjs/range 0 n)))
        (mathjs/sum 0)
        (mathjs/squeeze))))

(defn ->indices
  [m index]
  (.subset (mathjs/range 0 (mathjs/count m)) index))

(comment
  (do
    (let [weights (mathjs/matrix
                   #js
                   [#js [0 1 0]
                    #js [0 1 1]
                    #js [1 1 1]]
                   "sparse")
          activations #js [0 2]]
      (synaptic-input weights activations)))
  (do
    (let [weights (mathjs/matrix
                   #js
                   [#js [0 1 0]
                    #js [0 1 1]
                    #js [1 1 1]]
                   "dense")
          activations #js [0 2]]
      (synaptic-input weights activations))))


;; self.recurrent_weights /= self.recurrent_weights.sum(axis=0, keepdims=True)
;; since weights is i->j
;; this basically says, 'everything that is my inputs is normalized between 0 and 1'
;; for each neuron

;; this is the perf bottleneck atm for the hebbian neuronal area
(defn normalize
  [weights]
  (let [sums (mathjs/squeeze (mathjs/sum weights 0))]
    (.map weights
          (fn [v idx _m]
            (mathjs/divide v (.get sums #js[(aget idx 1)])))
          true)))

(comment
  (do
    (defn normalize
      [weights]
      (let [sums (mathjs/squeeze (mathjs/sum weights 0))]
        (.map
         weights
         (fn [v idx _m]
           (mathjs/divide v (.get sums #js [(aget idx 1)])))
         true
         )))
    (normalize
     (mathjs/matrix
      #js
      [#js [0 1 0]
       #js [0 1 1]
       #js [1 1 1]] "sparse"))))

;; the active activations are a set of indices
(defn ->neurons [n-neurons]
  (mathjs/range 0 n-neurons))

;; ---------------------
;; Hebbian plasticity dictates that w-ij be increased by a factor of 1 + β at time t + 1
;; if j fires at time t and i fires at time t + 1,
;; --------------
;;
;; You look at all inputs from the last time step,
;; then you select the ones that are active next
;; In effect, everytime I am active, I can look who activated me. For those connections we increase the weight

(defn hebbian-plasticity
  [{:keys [plasticity weights current-activations
           next-activations]}]
  (.subset
   weights
   (mathjs/index current-activations next-activations)
   (mathjs/multiply (mathjs/subset weights
                                   (mathjs/index
                                    current-activations
                                    next-activations))
                    (+ 1 plasticity))))



;; ==============================
;; binary hebbian-plasticity
;; ==============================
;; ---
;; For each neuron that is active right now, look at the neurons that were active at the last step,
;; for each of those edges (i->j) there is a chance, proportional to β, that there is a new synapse formed between the 2 neurons.
;; Doesn't matter if there is already a synapse or not.
;;
;; ---
;;
;; In order for the network to not be overrun with synapses everywhere, in a second process prune synapses.
;; It looks to me like the simplest way to do this is to simply prune 10% or something of the existing
;; synapses.
;;
;; Intuitively, this probably preserves cell assemblies and their connections.
;;
;;
;; The concept of synapse turnover is very biologically harmonious. [insert links to Seth Grant research]
;; https://youtu.be/vWuSpIAZW9s?si=lDeQLXHed9fIBGAP
;;
;; You could also model a lifetime for each synapse formed.
;; The plasticity rule could then reset the lifetime.
;;
;; If you do pruning, you have another problem: How to not accidentally starve the network of connections?
;; A simple counter meassure would be to not let the synapse count drop below a certain number.
;; We could also say that we throw away a starved network and start fresh. (new random connections).
;;
;; It is biological intuitive, that some processes form (semi random) fresh connections. You might wonder if
;; such a thing would happen during sleep. This would re-normalize the network, make new interpretions possible again.
;;
;; We could simply do this by shooting a bit of random activation into the network. The plasticity rules
;; would then already make new synapses.
;; As a variation of this you can construct geometry in the network, by making not completely random activation, but
;; random activation with geometry.
;; For instance, if you make a wave across the network, you automatically create synapses to neighbouring neurons.
;;
;;
;; params: β `plasticity` is now the chance that a new synapse is formed, per time step.
;; (per pair of active neurons)
;;
(defn scalar? [m]
  (zero? (mathjs/count (mathjs/size m))))

(defn binary-hebbian-plasticity
  [{:keys [plasticity weights current-activations
           next-activations]}]
  (let [subset (.subset weights
                        (mathjs/index current-activations
                                      next-activations))
        new-subset (if (scalar? subset)
                     (mathjs/bitOr subset (< (mathjs/random) plasticity))
                     (mathjs/map subset
                                 (fn [v _idx _m]
                                   (mathjs/bitOr v
                                                 (< (mathjs/random)
                                                    plasticity)))))]
    (.subset weights
             (mathjs/index current-activations
                           next-activations)
             new-subset)))

;;
;; This is the simple version, return new weights with prune-factor synapses removed.
;; This potentially starves the network of connections.
;;
;; The synapse turnover rate is given by
;; --------------------------------------
;; 1. The plasticity (chance for new synapse forming when 2 neurons are active across 2 time steps)
;; 2. The prune-event-rate (how often compared to neuron steps)
;; 3. The prune-factor (how many synapses are removed at a prune-event)
;;
;;
;; ---
;; Biologically, it would be easier for me to think in terms of synapse lifespan.
;; However, intuitively, it should all average out with large numbers.
;;
;;
(defn binary-prune-synapses
  [weights prune-factor]
  (let [survival-chance (- 1 prune-factor)]
    (.map weights
          (fn [v _idx _m]
            (< (mathjs/random) survival-chance))
          true)))

(comment

  (do
    (defn hebbian-plasticity
      [{:keys [plasticity weights current-activations
               next-activations]}]
      ;; reference: def
      ;; plasticity(w,act,new_act,plasticity):
      ;;     w[np.ix_(act,new_act)] *= 1 + plasticity
      ;;     return w
      (.subset
       weights
       (mathjs/index current-activations next-activations)
       (mathjs/multiply (mathjs/subset
                         weights
                         (mathjs/index current-activations
                                       next-activations))
                        (+ 1 plasticity))))
    [(hebbian-plasticity
      {:current-activations (mathjs/matrix #js [0 1])
       :next-activations (mathjs/matrix #js [0])
       :plasticity 0.1
       :weights (mathjs/matrix #js [#js [0 0 0] #js [1 1 1]
                                    #js [1 1 1]]
                               "sparse")})])
  ;; [#object[SparseMatrix
  ;;          [[0, 0, 0]
  ;;           [1.1, 1, 1]
  ;;           [1, 1, 1]]]]
  ;; reference:
  ;; [[0.  0.  0. ]
  ;;  [1.1 1.  1. ]
  ;;  [1.  1.  1. ]]
  (do
    (defn binary-hebbian-plasticity
      [{:keys [plasticity weights current-activations
               next-activations]}]
      (.subset
       weights
       (mathjs/index current-activations next-activations)
       (-> (.subset weights
                    (mathjs/index current-activations
                                  next-activations))
           (mathjs/map (fn [v _idx _m]
                         (mathjs/bitOr v
                                       (< (mathjs/random)
                                          plasticity)))))))
    [(synaptic-input
      (binary-hebbian-plasticity
       {:current-activations (mathjs/matrix #js [0 1])
        :next-activations (mathjs/matrix #js [0])
        :plasticity 1.0
        :weights (mathjs/matrix #js [#js [0 0 0]
                                     #js [1 1 1]
                                     #js [1 1 1]]
                                "sparse")})
      #js [0 1 2])
     (binary-hebbian-plasticity
      {:current-activations (mathjs/matrix #js [0 1])
       :next-activations (mathjs/matrix #js [0])
       :plasticity 1.0
       :weights (mathjs/matrix #js [#js [0 0 0] #js [1 1 1]
                                    #js [1 1 1]]
                               "sparse")})])




  (mathjs/bitOr (< (mathjs/random) 0.9))

  (do
    (defn prune-synapses
      [weights prune-factor]
      (let [survival-chance (- 1 prune-factor)]
        (.map
         weights
         (fn [v _idx _m]
           (< (mathjs/random) survival-chance))
         true)))

    (prune-synapses
     (mathjs/matrix
      #js[#js[0 0 0]
          #js[1 1 1]
          #js[1 1 1]] "sparse")
     0.5)))

;; ---------------------------
;; Inhbition model
;; --------------------------
;; Who is active in the next time step?
;; cap-k answers this question by saying the top k neurons with the highest synaptic input

(defn cap-k
  [k synaptic-input]
  (into-array :int (take k (argops/argsort > (.valueOf synaptic-input)))))

(defn indices-above-input-cutoff [synaptic-input threshold]
  (.subset (mathjs/range 0 (mathjs/count synaptic-input))
           (mathjs/index (mathjs/larger synaptic-input threshold))))

(def threshold-inhibiton indices-above-input-cutoff)

(defn update-neuronal-area
  [{:as state
    :keys [activations weights inhibition-model
           plasticity-model]}]
  (if
      (zero? (mathjs/count activations))
      state
      (let [synaptic-input (synaptic-input weights activations)
            next-active (inhibition-model state synaptic-input)
            next-weights (if plasticity-model
                           (plasticity-model
                            (assoc state
                                   :current-activations activations
                                   :next-activations next-active))
                           weights)]
        (assoc state
               :activations next-active
               :weights next-weights))))

;; inputs are just a set of neuron indices
;; you might decide to call the inhibition model here, but whatever.
;; there will be another time step soon
(defn set-input
  [state input]
  (assoc state :activations input))

(defn append-input
  [state input]
  (update state
          :activations
          (fn [activations]
            (mathjs/setUnion activations input))))

(defn read-activations
  [{:keys [activations]}]
  (when activations
    (.valueOf activations)))

(defn ->rand-projection
  [n-neurons p-probability]
  (mathjs/subset
    (mathjs/range 0 n-neurons)
    (mathjs/index
      (.map (mathjs/matrix (mathjs/zeros #js [n-neurons]))
            (fn [v idx _]
              (if (< (mathjs/random 0 1) p-probability)
                true
                false))))))

(defn read-projection [proj]
  (.valueOf proj))

(comment
  (do (def p-probability 0.5)
    (def n-neurons 10)
    (mathjs/subset
      (mathjs/range 0 n-neurons)
      ;; (mathjs/index #js [true false false false
      ;; false false false false false true])
      (mathjs/index
        (.map (mathjs/matrix (mathjs/zeros #js [n-neurons]))
              (fn [v idx _]
                (if (< (mathjs/random 0 1) p-probability)
                  true
                  false)))))))


;; 2 weights, off and on
;; simulating inhibitory interneurons, signaling the absence of something
(defn ->input-fiber
  [input-space n-area density]
  {:input-count (:n-neurons input-space)
   :off-fibers (->uni-directional-fiber (:n-neurons
                                         input-space)
                                        (:n-neurons n-area)
                                        (/ density 10))
   :on-fibers (->uni-directional-fiber (:n-neurons
                                        input-space)
                                       (:n-neurons n-area)
                                       density)
   :output-count (:n-neurons n-area)})

(defn ->mask
      [max-index activations]
      (if (zero? (mathjs/count activations))
        (mathjs/and (mathjs/zeros max-index) 1.0)
        (mathjs/and (.subset (mathjs/zeros max-index)
                             (mathjs/index activations)
                             1.0)
                    1.0)))

(defn input-fiber-activations
  [input-space
   {:keys [input-count output-count off-fibers on-fibers]}]
  (let [activations (read-activations input-space)
        input->indices (fn [input]
                         (->indices
                          input
                          (mathjs/index
                           (mathjs/larger input 0))))]
    (mathjs/setUnion
     (input->indices
      (synaptic-input
       off-fibers
       (mathjs/not (->mask input-count activations))))
     (input->indices (synaptic-input on-fibers
                                     activations)))))



(comment
  (synaptic-input
   (->uni-directional-fiber 3 10 1.0)
   #js [0 1 2])

  (synaptic-input
   (->uni-directional-fiber 3 10 0.5)
   #js [0 1])

  input-fiber-activations

  (do
    ;; (defn invert-indices [count indices]
    ;;   (zero?
    ;;    (mathjs/count indices)
    ;;    (mathjs/range 0 count)
    ;;    (let [mask (.subset (mathjs/ones input-count)
    ;;    (mathjs/index activations) 0.0)]
    ;;      (->indices mask (mathjs/index (mathjs/larger
    ;;      mask 0))))
    ;;    (.subset
    ;;     (mathjs/range 0 count))))
    (defn ->mask
      [max-index activations]
      (if (zero? (mathjs/count activations))
        (mathjs/and (mathjs/zeros max-index) 1.0)
        (mathjs/and (.subset (mathjs/zeros max-index)
                             (mathjs/index activations)
                             1.0)
                    1.0)))
    (defn input-fiber-activations
      [input-space
       {:keys [input-count output-count off-fibers
               on-fibers]}]
      (let [activations (read-activations input-space)
            input->indices (fn [input]
                             (->indices
                              input
                              (mathjs/index
                               (mathjs/larger input 0))))]
        (mathjs/setUnion
         (input->indices
          (synaptic-input
           off-fibers
           (mathjs/not (->mask input-count activations))))
         (input->indices (synaptic-input on-fibers
                                         activations)))))
    (input-fiber-activations
     {:activations #js [0]}
     (->input-fiber {:n-neurons 3} {:n-neurons 10} 0.1)))





  )


;;
;; static-fiber:
;;
;; a function with area-1 as input and area-2 activations as output
;;












(comment
  (do
    (def mystate {:activations (->neurons 3)
                  :weights (->random-directed-graph 3 0.6)
                  :inhibition-model (fn [_ synaptic-input] (cap-k 2 synaptic-input))
                  :plasticity 0.1
                  :plasticity-model hebbian-plasticity})
    [:weights
     (.clone
      (:weights mystate))
     :activations
     (:activations mystate)
     :input
     (synaptic-input (:weights mystate)
                     (:activations mystate))
     :next-active
     ((:inhibition-model mystate) mystate (synaptic-input (:weights mystate)
                                                          (:activations mystate)))
     ;; (update-neuronal-area mystate)
     :next-weights
     (:weights (update-neuronal-area mystate))]))

;; ------------------------
;; A sensory apparatus

;; from sensory units -> neuron indices
;; With 2 versions, one off and another on
;; This models inhibitory interneurons, and represents the absence of a signal
;; This is similar to the canonical receptive fields of neuro-science

(defn sensory-apparatus-projection [n-neurons k-sensory-units projection-density]
  (into
   []
   (for [_ (range k-sensory-units)]
     ;; off / on projections (i.e the absence of a sensory unit being active is projected to some neurons)
     {false
      ;; using only 20% of the connetions for off, this way the inputs are less similar
      (into #{} (repeatedly (* n-neurons (* 0.2 projection-density)) #(rand-int n-neurons)))
      true
      (into #{} (repeatedly (* n-neurons projection-density) #(rand-int n-neurons)))})))

(defn sensory-apparatus
  [{:keys [n-neurons k-sensory-units projection-density] :as state}]
  (assoc
   state
   :sensory-projection
   (sensory-apparatus-projection n-neurons k-sensory-units projection-density)))

(defn ->sensory-inputs [input-states sensory-projection]
  (mathjs/matrix
   (into-array :int
               (into #{}
                     (mapcat
                      (fn [[idx input-active?]]
                        (get-in sensory-projection [idx input-active?]))
                      (map-indexed vector input-states))))))


(comment
  (->sensory-inputs
   [true false true]
   (sensory-apparatus-projection 100 3 0.1))
  (mathjs/matrix
   (into-array :int (->sensory-inputs
                     [true false true]
                     (sensory-apparatus-projection 100 3 0.1))))
  (time
   (do (sensory-apparatus-projection 10000 10 0.1) nil)))

;; ==================
;; 1 bit version
;; - use geometry
;; - synapses are 1 bit,
;; - no plasticity
;;

(defn gaussian [amplitude mean std-deviation x]
  (* amplitude (Math/exp
                (-
                 (/ (Math/pow (- x mean) 2)
                    (* 2 (Math/pow std-deviation 2)))))))


;;
;; You have the highest probability of connecting to yourself
;; then you count the distance, just the linear distance
;;
;; +--------------------------------+
;; |                                |
;; +------------+----+--------------+
;; | .. i - 1   | i  | i + 1, ..    |
;; +------------+----+--------------+
;;                ^
;;                |
;;                |
;;            highest connection probability to iself
;;  basically each neuron has a strip connectivity probably following a gaussian
;;  (consider the colls I draw in the ui purely visual with this model.
;;  the connectivity goes across the width boundary)
;;
;;
;; This creates something like 'columns' of std-deviation length neurons,
;; they overlap
;;
(defn lin-gaussian-geometry
  [{:keys [amplitude std-deviation density-factor]}]
  (fn [i j]
    (gaussian
     (or amplitude (+ 0.5 density-factor))
     0
     std-deviation
     (- j i))))

(def identity-plasticity :weights)

(comment
  (do
    (def mystate {:activations (->neurons 3)
                  :weights (->directed-graph-with-geometry
                            3
                            (lin-gaussian-geometry {:amplitude 0.7 :std-deviation 1}))
                  :inhibition-model (fn [_ synaptic-input] (cap-k 2 synaptic-input))
                  :plasticity nil
                  :plasticity-model identity-plasticity})
    [:weights
     (:weights mystate)
     :activations
     (:activations mystate)
     :input
     (synaptic-input (:weights mystate)
                     (:activations mystate))
     :next-active
     ((:inhibition-model mystate) mystate (synaptic-input (:weights mystate)
                                                          (:activations mystate)))
     ;; (update-neuronal-area mystate)
     :next-weights
     (:weights (update-neuronal-area mystate))]))
