(ns ftlm.vehicles.assembly-calculus
  (:require
   [tech.v3.datatype.argops :as argops]
   ["mathjs" :as mathjs]))

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
  (->
   (mathjs/map
    (mathjs/ones (* n-neurons n-neurons))
    (fn [_] (if (< (mathjs/random 0 1) density) 1.0 0.0)))
   (mathjs/reshape #js [n-neurons n-neurons])))

(defn synaptic-input
  [weights activations]
  (let [n (.get (mathjs/size (mathjs/matrix weights))
                #js [0])]
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
  (let [sums (mathjs/sum weights 0)]
    (mathjs/dotDivide weights sums)))

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
      (mathjs/add a b))))


(defn ->neurons [n-neurons]
  (mathjs/ones n-neurons))


;; ---------------------
;; Hebbian plasticity dictates that w-ij be increased by a factor of 1 + β at time t + 1
;; if j fires at time t and i fires at time t + 1,
;; --------------
;;
;; You look at all inputs from the last time step,
;; then you select the ones that are active next
;; In effect, everytime I am active, I can look who activated me. For those connections we increase the weight

(defn hebbian-plasticity
  [plasticity-rate synaptic-weights current-activations next-activations]
  (-> (mathjs/ones (mathjs/size synaptic-weights))
      (mathjs/subset (mathjs/index current-activations
                                   next-activations)
                     (+ plasticity-rate 1.0))
      (mathjs/dotMultiply synaptic-weights)))

(comment
  (do
    (defn ->hebbian-plasticity
      [plasticity-rate]
      (fn [synaptic-weights
           current-activations
           next-activations]
        (-> (mathjs/ones (mathjs/size synaptic-weights))
            (mathjs/subset (mathjs/index current-activations
                                         next-activations)
                           (+ plasticity-rate 1.0))
            (mathjs/dotMultiply synaptic-weights))))
    ((->hebbian-plasticity 0.1)
     (mathjs/matrix #js [#js [0 0 0]
                         #js [1 1 1]
                         #js [1 1 1]])
     (mathjs/matrix #js [0 1])
     (mathjs/matrix #js [0])))

  ;; #object[DenseMatrix [[0, 0, 0],
  ;;                      [1.1, 1, 1],
  ;;                      [1, 1, 1]]]

  ;; reference:
  ;; [[0.  0.  0. ]
  ;;  [1.1 1.  1. ]
  ;;  [1.  1.  1. ]]
  )

;; just needs to work
(defn ->cap-k [k]
  (fn [synaptic-input]
    (into-array :int (take k (argops/argsort > (.valueOf synaptic-input))))))

(defn update-neurons
  [{:as state :keys [neurons weights inhibition-model]}]
  (let [current-activations neurons
        synaptic-input (synaptic-input weights
                                       current-activations)
        next-active (inhibition-model state synaptic-input)
        next-weights (plasticity-model current-activations
                                       nex-active
                                       weights)]
    (assoc state
      :neurons next-active
      :weights next-weights)))



(comment
  (normalize (mathjs/ones 10))
  (normalize )
  (mathjs/norm v)
  (normalize-matrix  )


  (mathjs/sum (mathjs/ones 10))
  (mathjs/sum (mathjs/reshape (mathjs/ones 20) #js [10 2]) 0)

  (mathjs/divide
   (mathjs/reshape (mathjs/ones 20) #js [2 10])
   (mathjs/sum (mathjs/reshape (mathjs/ones 20) #js [2 10]) 1))

  (mathjs/multiply
   (mathjs/reshape (mathjs/ones 20) #js [2 10])
   #js [1 2])

  (defn normalize-matrix1 [input-weights]
    (mathjs/map
     input-weights
     (fn [w]
       (let [column-sum (mathjs/sum w)]
         (mathjs/divide w column-sum)))))

  (mathjs/sum #js [1 2 3])

  (mathjs/map w2 (fn [w] 0
                   ;; (mathjs/sum w)
                   ))

  (def w2 (->random-directed-graph 10 0.6))
  (count w2)

  (time (def w2 (->random-directed-graph 1e3 0.6)))

  (.-value (first (mathjs/size w2)))


  (normalize (normalize))

  (time (def weights (->random-directed-graph 10 1.0)))
  (def inputs (mathjs/ones 10))
  (mathjs/multiply weights inputs))


(comment

  (def n-neurons 4)
  (mathjs/reshape #js [1 1 1 1] #js [2 2])
  (mathjs/random 0 1)

  ;; (def weights (mathjs/map (mathjs/zeros #js[n-neurons n-neurons]) (fn [_] (mathjs/random 0 1))))

  (def weights
    (mathjs/map
     (mathjs/range n-neurons)
     (fn [_] (mathjs/random 0 1))))

  (def row-length 2)
  )

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

  (normalilize (mathjs/matrixFromRows #js[0 1 0] #js[0 2 2]))

  (def current-activations (mathjs/matrix #js [1 0]))
  (def next-activations (mathjs/matrix #js [1 0]))

  (def weights
    (mathjs/matrixFromRows
     #js[1 0]
     #js[0 1]))

  ((->hebbian-plasticity 0.1) current-activations next-activations weights)

  (mathjs/transpose current-activations)

  (.. (mathjs/chain 3) (add 4) (subtract 2) done)

  (.. (mathjs/chain #js [1 2 3 ])
      (subset (mathjs/index 0) 8)
      (subtract 2)
      done)

  (.subset
   (mathjs/matrix #js [1 2 3])
   (mathjs/index 0))

  (mathjs/sum
   (.subset
    (mathjs/matrix #js [1 2 3])
    (mathjs/index #js [0 1])))

  (->synaptic-input (mathjs/matrix #js[1 2 3]) #js[0 1])

  (->synaptic-input
   (mathjs/matrix (mathjs/ones #js[2 2]))
   #js[0 1])

  (.subset
   (mathjs/matrix (mathjs/ones #js[2 2]))
   ;; #object[DenseMatrix [[1, 1], [1, 1]]]
   (mathjs/index 0 0))

  (let [weights (mathjs/matrix (mathjs/ones #js[3 3]))
        activations (mathjs/matrix #js[0 1 0])]
    ;; this is a dimension error:
    ;; (.subset weights (mathjs/index activations))
    ;; correct:
    (.subset weights (mathjs/index #js[0 1])))



  (.subset
   (mathjs/matrix (mathjs/ones #js[2 2]))
   (mathjs/index #js [0]))

  (mathjs/matrix
   (mathjs/ones #js [2 2]))

  (mathjs/ones #js [2 2])



  (mathjs/bitAnd #js[0 1 1] #js[0 1 0])
  (-> (mathjs/bitAnd #js[0 1 1] #js[0 1 0]) (mathjs/add))
  (-> (mathjs/bitAnd current-activations next-activations))

  (..
   (mathjs/chain weights)
   (subset (mathjs/index 0 0) 1)
   done)

  (..
   (mathjs/chain weights)
   (subset (mathjs/index 0 0) 1)
   done)


  (mathjs/index 0 (mathjs/range 0 ))

  (let [d #js [#js [1 2]
               #js [3 4]]]
    (mathjs/subset d (mathjs/index 1 0)))


  (let [d #js [#js [1 2]
               #js [3 4]]]
    (mathjs/subset d (mathjs/index 1 0) 1))

  ;; selects the cols
  (let [d #js [#js [1 2]
               #js [3 4]]]
    (mathjs/subset d (mathjs/index #js[false true] 0)))

  ;; both cols and then the first
  (let [d #js [#js [1 2]
               #js [3 4]]]
    (mathjs/subset d (mathjs/index #js[0 1] 0)))


  (let [d #js [#js [1 2]
               #js [3 4]]]
    (mathjs/subset d (mathjs/index #js[0 1] #js [0 1])))

  (let [d #js [#js [1 2]
               #js [3 4]]]
    (mathjs/subset d (mathjs/index #js[0 1] #js [0 1])))


  (let [d #js [#js [1 2]
               #js [3 4]]]
    (mathjs/subset d (mathjs/index #js[false true] true)))

  (let [d #js [#js [1 2]
               #js [3 4]]]
    (mathjs/subset d (mathjs/index #js[false true] #js [true true])))




  (let [d #js [#js [1 2]
               #js [3 4]]]
    (->
     (mathjs/subset
      d
      (mathjs/index #js[0 1] (mathjs/range 0 2)))
     (mathjs/sum 0)))

  (let [d #js [#js [1 2]
               #js [3 4]]]
    (->
     (mathjs/subset
      d
      (mathjs/index #js[0 1] (mathjs/range 0 2)))
     (mathjs/sum 1)))



  (let [d #js [#js [0 1]
               #js [0 1]]]
    (first (mathjs/size d)))



  (let [d #js [#js [0 1 0]
               #js [0 1 1]]]
    (->
     (mathjs/subset
      d
      (mathjs/index #js[0 1] (mathjs/range 0 (nth (mathjs/size d) 1))))
     (mathjs/sum 0)))


  (let [d #js [#js [0 1 0]
               #js [0 1 1]]
        activations #js [0 1]]
    (->
     (mathjs/subset
      d
      (mathjs/index activations (mathjs/range 0 (nth (mathjs/size d) 1))))
     (mathjs/sum 0)))

  (let [d #js [#js [0 1 0]
               #js [0 1 1]]]
    (mathjs/matrix d))


  (let [d #js [#js [0 1 0]
               #js [0 1 1]
               #js [1 1 1]]
        activations #js [0 2]]
    ;; (->synaptic-input d activations)
    ;; (.get 0)
    (.-value (first (mathjs/size (mathjs/matrix d))))
    (.get (mathjs/size (mathjs/matrix d)) #js [0]))


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


  (mathjs/matrix #js [1 2 3 4])



  ;; #object[DenseMatrix [1, 3, 2]]


  ;; (normalize weights)

  ;; (mathjs/resize #js[3 3])



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
