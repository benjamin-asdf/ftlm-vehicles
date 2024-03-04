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
;; the ignition of this stuff is from Hebb.
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

;; (defn ->random-directed-graph-with-geometry-per-row
;;   [n-neurons density row-length]
;;   (let [row (fn [i] (quot i row-length))
;;         high-probablity density
;;         low-probablity (/ density 10)]
;;     (let [neuron-row (fn [i] (quot i row-length))
;;           w (mathjs/range 0 (* n-neurons n-neurons))
;;           ;; p



;;           ]
;;       (-> (mathjs/map
;;            (mathjs/range 0 (* n-neurons n-neurons))
;;            (fn [idx]
;;              (let [i (quot idx n-neurons)
;;                    j (mod idx n-neurons)]
;;                (if (<
;;                     (mathjs/random 0 1)
;;                     (if (= (neuron-row i) (neuron-row j))
;;                       high-probablity
;;                       low-probablity))
;;                  1.0
;;                  0.0))))
;;           (mathjs/reshape #js [n-neurons n-neurons])))))


;;
;; Allowing self-connections
;;

(defn ->random-directed-graph
  [n-neurons density]
  (-> (mathjs/map
        (mathjs/ones (* n-neurons n-neurons))
        (fn [_]
          (if (< (mathjs/random 0 1) density) 1.0 0.0)))
      (mathjs/reshape #js [n-neurons n-neurons])))

(defn synaptic-input
  [weights activations]
  (let [n (.get (mathjs/size weights) #js [0])]
    (-> (mathjs/subset weights
                       (mathjs/index activations
                                     (mathjs/range 0 n)))
        (mathjs/sum 0))))

(comment
  (do (defn ->synaptic-input
        [weights activations]
        (let [n (.get (mathjs/size (mathjs/matrix weights))
                      #js [0])]
          (-> (mathjs/subset
               weights
               (mathjs/index activations (mathjs/range 0 n)))
              (mathjs/sum 0))))
      (let [d #js [#js [0 1 0] #js [0 1 1] #js [1 1 1]]
            activations #js [0 2]]
        (->synaptic-input (mathjs/matrix d) activations))))


;; self.recurrent_weights /= self.recurrent_weights.sum(axis=0, keepdims=True)
;; since weights is i->j
;; this basically says, 'everything that is my inputs is normalized between 0 and 1'
;; for each neuron

(defn normalize
  [weights]
  ;; weights
  (let [sums (mathjs/sum weights 0)]
    (mathjs/dotDivide weights sums))
  )

(comment
  (do
    (defn normalize
      [weights]
      (let
          [d #js[#js[0 1 0]
                 #js[0 1 1]
                 #js[1 1 1]]
           weights (mathjs/matrix d)
           sums (mathjs/sum weights 0)]
          (mathjs/dotDivide weights sums)))
    (let [a (mathjs/matrix #js [1 2])
          b (mathjs/matrix #js [#js [3] #js [4]])]
      (mathjs/add a 3)
      (mathjs/add a b)))

  (do
    (defn normalize1
      [weights]
      (let [sums (mathjs/sum weights 0)]
        (-> (mathjs/dotDivide weights sums)
            (mathjs/add 0.5)
            (mathjs/round))))
    (normalize1 #js [#js [0 1 0]
                     #js [0 1 1]
                     #js [1 1 1]]))



  )

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
  [{:keys [plasticity weights current-activations next-activations]}]
  (-> (mathjs/ones (mathjs/size weights))
      (mathjs/subset (mathjs/index current-activations
                                   next-activations)
                     (+ plasticity 1.0))
      (mathjs/dotMultiply weights)))

(comment
  (do
    (defn ->hebbian-plasticity
      [plasticity]
      (fn [weights current-activations next-activations]
        (-> (mathjs/ones (mathjs/size weights))
            (mathjs/subset (mathjs/index current-activations
                                         next-activations)
                           (+ plasticity 1.0))
            (mathjs/dotMultiply weights))))
    ((->hebbian-plasticity 0.1)
     (mathjs/matrix #js [#js [0 0 0] #js [1 1 1]
                         #js [1 1 1]])
     (mathjs/matrix #js [0 1])
     (mathjs/matrix #js [0])))

  ;; (for [i (mathjs/matrix #js [0 1])]
  ;;   i)

  ;; #object[DenseMatrix [[0, 0, 0],
  ;;                      [1.1, 1, 1],
  ;;                      [1, 1, 1]]]

  ;; reference:
  ;; [[0.  0.  0. ]
  ;;  [1.1 1.  1. ]
  ;;  [1.  1.  1. ]]
  )

(defn cap-k [k synaptic-input]
  (into-array :int (take k (argops/argsort > (.valueOf synaptic-input)))))

(defn update-neuronal-area
  [{:as state
    :keys [activations weights inhibition-model
           plasticity-model]}]
  (let [synaptic-input (synaptic-input weights activations)
        next-active (inhibition-model state synaptic-input)
        next-weights (plasticity-model
                      (assoc state
                             :current-activations activations
                             :next-activations next-active))]
    (assoc state
           :activations next-active
           :weights next-weights)))

;; inputs are just a set of neuron indices
;; you might decide to call the inhibition model here, but whatever.
;; there will be another time step soon
(defn set-input
  [state input]
  (assoc state :activations input))

(defn read-activations
  [state]
  (.valueOf (:activations state)))

(comment
  (do
    (def mystate {:activations (->neurons 3)
                  :weights (->random-directed-graph 3 0.6)
                  :inhibition-model (fn [_ synaptic-input] (cap-k 2 synaptic-input))
                  :plasticity 0.1
                  :plasticity-model hebbian-plasticity})
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


;; ------------------------
;; A sensory apparatus

;; from sensory units -> neuron indices
;; With 2 versions, one off and another on
;; This models inhibitory interneurons, and represents the absence of a signal
;; This is similar to the canonical receptive fields of neuro science

(defn sensory-apparatus-projection [n-neurons k-sensory-units projection-density]
  (into
   []
   (for [_ (range k-sensory-units)]
     ;; off / on projections (i.e the absence of a sensory unit being active is projected to some neurons)
     {false
      (into #{} (repeatedly (* n-neurons projection-density) #(rand-int n-neurons)))
      true
      (into #{} (repeatedly (* n-neurons projection-density) #(rand-int n-neurons)))})))

(defn sensory-apparatus
  [{:keys [n-neurons k-sensory-units projection-density] :as state}]
  (print n-neurons k-sensory-units projection-density)
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

(defn ->directed-graph-with-geometry [])

(comment


  (.map
   (mathjs/matrix
    #js [#js [1 0 1] #js [1 1 1] #js [1 0 1]]
    "sparse")
   (fn [v i m] (def i i) v)
   true)


  (mathjs/SparseMatrix.diagonal #js[3 3] 1 0)
  (mathjs/SparseMatrix.diagonal #js[3 3] 1 0)

  )

(comment
  (def n-neurons 4)
  (mathjs/reshape #js [1 1 1 1] #js [2 2])
  (mathjs/random 0 1)

  ;; (def weights (mathjs/map (mathjs/zeros #js[n-neurons n-neurons]) (fn [_] (mathjs/random 0 1))))

  (def weights
    (mathjs/map
     (mathjs/range n-neurons)
     (fn [_] (mathjs/random 0 1))))

  (def row-length 2))

(comment
  (def w (mathjs/ones #js [2 2]))

  (.subset w )
  (.. w (subset (mathjs/index 0 1) 0))

  ;; numpy reference:
  ;; def normalize(self):
  ;;   for w, inp in zip(self.input_weights, self.inputs):
  ;;       w /= w.sum(axis=0, keepdims=True)


  ;; mathjs:

  (mathjs/subset w (mathjs/index 0 1) 0)
  (mathjs/subset w (mathjs/index 0 0) 0)
  (mathjs/subset w (mathjs/index 0 0) 1)
  (.subset w (mathjs/index 0 0) 1)


  (def current-activations (mathjs/matrix #js [1 0]))
  (def next-activations (mathjs/matrix #js [1 0]))

  (def weights
    (mathjs/matrixFromRows
     #js[1 0]
     #js[0 1]))
  (mathjs/transpose current-activations)



  (do
    (defn ->synaptic-input
      [weights activations]
      (let [n (.get (mathjs/size (mathjs/matrix weights)) #js [0])]
        (-> (mathjs/subset
             ;; (mathjs/matrix weights)
             weights
             (mathjs/index
              activations
              (mathjs/range 0 n)))
            (mathjs/sum 0))))
    (let [d #js [#js [0 1 0]
                 #js [0 1 1]
                 #js [1 1 1]]
          activations #js [0 2]]
      (->synaptic-input (mathjs/matrix d) activations)))

  (let
      [d #js[#js[0 1 0]
             #js[0 1 1]
             #js[1 1 1]]
       weights (mathjs/matrix d)
       sums (mathjs/sum weights 0)]

    ;; (mathjs/divide weights sums)
    ;; (mathjs/apply weights 0 (fn [w] 1))
    ;; (mathjs/subset weights (mathjs/index (mathjs/range 0 3) 0))
    ;; (mathjs/forEach
    ;;  sums
    ;;  (fn [s idx _]
    ;;    (mathjs/subset weights (mathjs/index idx) s)))
      (mathjs/dotDivide weights sums))

  ;; #object
  ;; [DenseMatrix [[0, 0.3333333333333333, 0],
  ;;               [0, 0.3333333333333333, 0.5]
  ;;               [1, 0.3333333333333333, 0.5]]]

  (mathjs/resize (mathjs/matrix) #js [3] 3))
