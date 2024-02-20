(ns ftlm.vehicles.assembly-calculus
  (:require
   [tech.v3.datatype.functional :as dtype-fn]
   [tech.v3.datatype :as dtype]
   [tech.v3.dataset :as ds]))

;; paper: https://arxiv.org/abs/2110.03171

;;
;; Assemblies!
;; Sanotsh Vempala's cell asssembly inspired model
;; "Let's assume the brain is implementing an assembly calculus"
;;

;; https://github.com/mdabagia/learning-with-assemblies

;; 1. In assembly calculus, you have a
;;

;; insert Braitenberg quote, musing about the structure of cell assemblies (1977)
;; []

;; Braibenberg is stretching them up to memes and memeplexes!
;; But it is still all the activity of the cell assemblies.
;; Thus, we firmly get into the territory of traversing intermediate space between
;; neurons and cognition
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
;;                         or McCulloch-Pitts neurons, too
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
;; 2. Directed graph of excitatory neurons with dynamic edge weights
;; 3. Discrete timesteps
;; 4. Inhibition (Braitenberg calls this a thought pump)
;;

;;
;; the ignition of this stuff is from Hebb.
;;

;;
;; I. Devide the brain into areas containg n neurons connected through a G(n,p) directed graph
;; II. implement a k-cap selection, the top k excitied neurons fire at the next time step


;; each node is called a neuron
;; p is the propability of a connection
;; Each edge (i,j) in this graph, called a synapse, has a dynamic non-negative weigth w-ij(t), initially 1
;; E is the set of all edges
;; inhibit and disinhibit areas

;; state:
;; area state
;;

(defn state [t]
  ;; X
  {:area-state [true false]
   :neuron-state [true false]
   :weights [1 1 1]})

;; ====== parameters ======
;; n = 10^7 (neurons per area)
;; k = 10^4 (cap k k)
;; p = 10^-3 (connection probability)
;; β = 0.1 (plasticity)

#_(def parameters
  {:plasticity 0.1
   :neurons-per-area (long 1e7)
   ;; the cap-k k
   :k (long 1e4)
   :connection-probability 0.001})

;; they say you can also get a way with much smaller params,
;; lets try to start small.
;;

(def parameters
  {:plasticity 0.1
   ;; what 10^3 that is so tiny!
   :neurons-per-area (long 1e3)
   ;; the cap-k k
   ;; 100 incidentally overlaps with the claimend number of cortical 'minicolumns'
   :k (long 1e2)
   ;; I called this 'connection-probability'
   :sparsity (double 1e-2)})




(defn ->area [state])


;; initially 0
;; a bit whether the area is inhibited
(defn area-inhibited? [state area t])

;; a bit whether the neuron i spikes at time t
;; zero if the area X of i is inhibited
;;
(defn fires? [state neuron-i t])

(defn weight [state neuron-i neuron-j t])

(defn disinhibited-areas [state t])

;; indices
(defn neurons [area t])

;; is this across areas, too?
;; returns a sequence of [i j] pairs
(defn edges [state neuron])

;; === Inhibition ===
;; --------------------------

;; returns the indices of the next active neurons
;; (top-k)
(defn cap-k [synaptic-inputs state]
  ;;
  (:k state)
  [])

;; (defn inhibition-model [state])

;; This is the first place I want to extend the model,
;; allowing dynamic inhibition.
(defn inhibition-model [state area t] cap-k)

(defn next-active
  [state area t synaptic-inputs]
  ;; to say who is firing in the next time step, we use
  ;; cap-k
  ;; (or whatever inhibition model we are choosing)
  ((inhibition-model state area t) synaptic-inputs state))


;; === Plasticity ===
;; ---------------------------------
(defn next-weights [state])

;; if j fires at time t and i fires at time t + 1,
;; Hebbian plasticity dictates that w-ij be increased by a factor of 1 + β at time t + 1
;;
;; -- hebbian plasticity impl. --
(defn next-weight [state i j t]
  (let [plasticity (:plasticity state)
        firing-j? (fires? state j t)
        firing-i-next? (fires state i (inc t))]
    (*
     (weight state i j t)
     (+ 1
        (* plasticity firing-j? firing-i-next?)))))


;; homeostatis
;; renormalize the weights at a slower time scale
;;
(defn renormalize-weights [])

;; ---
;; State transition

(defn step
  [state t]
  (let [disinhibited-areas (disinhibited-areas state t)]
    ;; for each disinhibited-area,
    (for [area disinhibited-areas]
      (let [synaptic-inputs
            ;; for each neuron in area a
            (for [n (neurons area t)]
              ;; define the synaptic input at time t + 1
              ;; SI(i, t + 1)
              (let [synaptic-input
                    (sum
                     (map (fn [[i j]]
                            (* (fires? state i j t)
                               (weight state i j t)))
                          (edges state n)))

                    fires?]))
            next-active (next-active state area t synaptic-input)]))))



;; ---- AC system
;; --- commands ---
;;

;; x is the name of an assembly in a disinhibited area
;; overrides the selection by k-cap and
;; causes the k neurons of assembly x to fire at time t
;; this is how you have inputs into this
(defn fire [state assembly])

;; toggle the inhibition
;; (defn disinhibit [state x])

(defn disinhibit [state area t disinhibit?])


;; ======== Assembly =========
;; An assembly is a highly interconnected (in terms of both number of synapses and their weights)
;; set of k neurons in an area encoding a real world entity.
;;
;; I call k the `assembly-neuron-count` (not to be confused with the k from the cap-k algorithm)
;;

;; Initially we put that into a special 'sensory area'
;;

;; assemblies in the remaining areas emerge from the model

;; ======= assembly commands ===================
;; (very similar to what hypervectors can do, too)
;; A dynamic information storage substance.
;; Half of a piece of information is allowed, association between 2 is allowed, merging 2 is allowed


;; (x, Y, y)
;; starting from an assembly x in area X
;; create a new area Y (where there is connectivity from X to Y)
;; a new assembly y, which has strong synaptic connectivity from x to y
;; (y will fire when x fires from now on. Unless Y is inhibited).
(defn project [state assembly]
  (let [area-x (assembly->area state assembly)
        new-area (->area state)
        ;; guessing you take here random neurons from the new area
        new-assembly []]
    ;; 1. dishinhibit X and Y
    ;; (do I inhibit everything else so X and Y fire together alone rn?)
    ;; 2. fire the neurons in x for the next t time steps
    ;; -> a stable assembly in Y will emerge (that is shown theoretically) (ok this really is quite cool)
    ;; it is densly interconnected and has high connectivity to x
    ;;
    ;; (this is the most sophisticated contribution to the theory to date)
    ;; Legenstein (2018), Papadimitriou and Vempala (2019)
    ))


;; --- the sensory area
;; make a special area to encode the 'stimuli'
;; -> sequences of stimuli
;;

;; ---- stimulus ----
;; a stimulus is a set of about k neurons firing simultaneously ("presented") in the sensory area
;; - you can fire a different number of neurons than k in the sensory area
;; (in other words they excempt the sensory area from the inhibition model)
;; perhaps maps to LGN, perhaps to sensory areas, who knows.
;;
;; ---
;;
;; A `stimulus-class` A is a distribution over stimuli, defined by three parameters:
;; two scalars  r, q ∈ [0,1], r > q and a set of k neurons S-a in the sensory area
;;
;;
;; (kinda funny how they use the same letters over and over. Thats where you know these are math people, not programmers)
;;

;; set of k neurons inside the sensory area
(defn stimulus-neurons [sensory-area stimulus-class])

;; select

(defn activate-stimulus-neurons [state propability-r propability-q])



;; `input-neurons-count`: (n_in)
;; I think that is the neurons in the input area?

;;
;; eye:
;; ```
;; | 1 0 0 |
;; | 0 1 0 |
;; | 0 0 1 |
;; ```
;; ```
;; | False True  True  |
;; | True  False True  |
;; | True  True  False |
;; ```

;; 'mask'

;; Idea:
;; The brain is an activation organizer
;; You can say the car engine is about gasoline
;; Inhibition is how the brain can shape, transform, organize this activation
;;
;; Consider the brain to be about activation the way a car engine is about gasoline,
;; to understand the whole is to understand both the content and the surrounding machinery.
;;





;; n_in = 1000
;; n_classes = 4
;; n_neurons = 1000
;; cap_size = 100
;; sparsity = 0.1
;; n_rounds = 5
;; beta = 1e-1
;; p_r = 0.9
;; p_q = 0.1 * cap_size / n_in
;; mask = (rng.random((n_neurons, n_neurons)) < sparsity) & np.logical_not(np.eye(n_neurons, dtype=bool))
;; W = np.ones((n_neurons, n_neurons)) * mask
;; W /= W.sum(axis=0)
;; mask_a = (rng.random((n_in, n_neurons)) < sparsity)
;; A = np.ones((n_in, n_neurons)) * mask_a
;; A /= A.sum(axis=0)

;; ____ tmdjs Implementation ____

(defn ->edges
  [neurons-per-area sparsity]
  (ds/->dataset
   (into
    {}
    (map
     (fn [coll]
       [coll
        (dtype/emap
         (fn [idx]
           (boolean (when (not= coll idx) (< (rand) sparsity))))
         :boolean
         (dtype/make-container :int32 (range neurons-per-area)))])
     (range neurons-per-area)))))

(ds/->dataset
 (map (fn [coll] {:coll coll :row [1 2 3]})
      (range neurons-per-area)))



;; def k_cap(input, cap_size):
;;     if np.all(input <= 0):
;;         return []
;;     else:
;;         return input.argsort(axis=-1)[...,-cap_size:]


(defn k-cap [input cap-size]
  (if (every? (partial <= 0) input)
    []
    (->> input
         (sort)
         (take-last cap-size))))




;; feed forward are
(defn ->ffa-area
  [{:keys [neurons-per-area sparsity]}]


  )


(def neurons-per-area 5)
(def sparsity 0.1)
(time (->edges 1e3 sparsity))
(def edges (->edges 1e3 sparsity))
(dtype/emap)
(->edges 5 1)
