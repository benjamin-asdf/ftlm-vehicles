(ns ftlm.vehicles.assembly-calculus
  (:require
   [tech.v3.datatype.argops :as argops]
   [tech.v3.datatype.functional :as dtype-fn]
   [tech.v3.datatype :as dtype]
   ["mathjs" :as mathjs]))




;; -----------------------------------------------

;; paper: https://arxiv.org/abs/2110.03171

;;
;; Assemblies!
;; Sanotsh Vempala's cell asssembly inspired model
;; "Let's assume the brain is implementing an assembly calculus"
;;
;; https://github.com/mdabagia/learning-with-assemblies

;; Braibenberg is stretching them up to memes and memeplexes!

;; ---- Cell assemblies ----

;; Essentials:
;; 1. Hebbian plasticity - fire together, wire together
;; Update: - actually it's not hebbian after all
;; Tzitzitlini Alejandre-GarcíaSamuel KimJesús Pérez-OrtegaRafael Yuste (2022)
;; And perhaps fair to call them Ensembles because that name was around before Hebb
;; 2. Directed graph of excitatory activations with dynamic edge weights
;; 3. Discrete timesteps
;; 4. Idealized inhibition model. That is we don't model the inhibitory neurons but a 'sheet inhibition', for instance with a cap-k algorithm
;; (Braitenberg calls this a thought pump)
;;
;;
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


;; this is the perf bottleneck atm for the hebbian neuronal area
(defn normalize
  [weights]
  (let [sums (mathjs/squeeze (mathjs/sum weights 0))]
    (.map weights
          (fn [v idx _m]
            (mathjs/divide v (.get sums #js[(aget idx 1)])))
          true)))

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


;; ---------------------------
;; Inhbition model
;; --------------------------
;; Who is active in the next time step?
;; cap-k answers this question by saying the top k neurons with the highest synaptic input

(defn cap-k
  [k synaptic-input]
  (into-array :int (take k (argops/argsort > (.valueOf (mathjs/filter synaptic-input #(mathjs/larger % 0)))))))

(defn indices-above-input-cutoff [synaptic-input threshold]
  (.subset (mathjs/range 0 (mathjs/count synaptic-input))
           (mathjs/index (mathjs/larger synaptic-input threshold))))

(def threshold-inhibiton indices-above-input-cutoff)

(defn update-neuronal-area
  [{:as state
    :keys [activations weights inhibition-model
           plasticity-model]}]
  (if (zero? (mathjs/count activations))
    state
    (let [synaptic-input (synaptic-input weights activations)
          next-active (inhibition-model state synaptic-input)
          next-weights (if plasticity-model
                         (plasticity-model
                          (assoc state
                                 :current-activations
                                 activations
                                 :next-activations next-active))
                         weights)]
      (assoc state
             :activations next-active
             :weights next-weights))))


;; same but different calling conventions
;; inhibition model takes a state, returns a state
(defn update-neuronal-area-2
  [{:as state
    :keys [activations weights inhibition-model
           n-neurons
           plasticity-model]}]
  (let [synaptic-input
        (if
            (zero? (mathjs/count activations))
            (mathjs/zeros #js [n-neurons])
            (synaptic-input weights activations))
        state (inhibition-model
               (assoc state
                      :synaptic-input
                      synaptic-input))
        next-weights
        (if plasticity-model
          (plasticity-model
           (assoc state
                  :current-activations activations
                  :next-activations (:activations state)))
          weights)]
    (assoc state :weights next-weights)))



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

(defn append-input-2
  [state inputs]
  (update state
          :activations
          (fn [activations]
            (reduce (fn [activations input] (mathjs/setUnion activations input)) activations inputs))))

(defn read-activations
  [{:keys [activations]}]
  (when activations
    (.valueOf activations)))

(defn activation-intersection [set1 set2]
  (mathjs/setIntersect set1 set2))

(defn count-intersection
  [a b]
  (mathjs/count (activation-intersection a b)))

(defn ->projection
  "`projection-model` is a function that takes an index and returns
   wheter there neuron `i` is a projection neuron.
  If it returns a number, that is taken a probability between 0 and 1"
  [n-neurons projection-model]
  (mathjs/subset
   (mathjs/range 0 n-neurons)
   (mathjs/index
    (.map (mathjs/matrix (mathjs/zeros #js [n-neurons]))
          (fn [v idx _]
            (let [p (projection-model idx)]
              (boolean (cond (number? p)
                             (< (mathjs/random 0 1) p)
                             :else p))))))))

(def count-projection mathjs/count)
(def count-activation mathjs/count)

(defn ->rand-projection
  [n-neurons p-probability]
  (->projection n-neurons (constantly p-probability)))

(defn read-projection [proj]
  (.valueOf proj))



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
    (gaussian (or amplitude (+ 0.5 density-factor))
              0
              std-deviation
              (- j i))))
;;
;; Making it wrap like a torus would
;;
(defn lin-gaussian-geometry-wrap
  [{:keys [amplitude std-deviation density-factor
           n-neurons]}]
  (fn [i j]
    (gaussian (or amplitude (+ 0.5 density-factor))
              0
              std-deviation
              (min (abs (- j i))
                   (abs (+ (- n-neurons j) i))))))

(def identity-plasticity :weights)

(defn map-weights
  [area op]
  (time (let [r (atom [])]
          (.forEach (:weights area) (comp #(swap! r conj %) op) true)
          (take 10 @r))))



;; less relevant stuff:


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


(defn ->uni-directional-fiber
  [n-neurons-1 n-neurons-2 density]
  (.map (mathjs/matrix (mathjs/zeros #js [n-neurons-1
                                          n-neurons-2])
                       "sparse")
        (fn [_ _ _] (if (< (mathjs/random) density) 1 0))))


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


;; ----------------------------------------------------------------------------------------------------------


;; ================
;; attenuation
;; ================
;;
;;
;; See https://faster-than-light-memes.xyz/biological-notes-on-the-vehicles-cell-assemblies.html
;; (attenuation)
;;
;;
;; Here, I do 1 simpler that is just carry over an attenuation malus
;;
;;
;; 'Rolling malus' implementation
;;
;; 1. Everytime you are active, your malus goes up.
;; 2. Every time step the mulus decays.
;; 3. The malus is applied to the synaptic input divisively
;;
;; kinda simplest thing, 1-3 could also be more complicated functions.
;;

(defn attenuation
  [{:as state
    :keys [attenuation-malus attenuation-decay
           attenuation-malus-factor synaptic-input n-neurons
           activations]}]
  (let [attenuation-malus (or attenuation-malus
                              (mathjs/matrix
                               (mathjs/zeros
                                #js [n-neurons])))
        ;; decay the malus from previous step
        attenuation-malus (mathjs/multiply
                           attenuation-malus
                           (- 1 attenuation-decay))
        attenuation-malus
        ;; accumulate the malus on everybody active
        (.subset attenuation-malus
                 (mathjs/index activations)
                 (mathjs/add (mathjs/subset
                              attenuation-malus
                              (mathjs/index activations))
                             attenuation-malus-factor))]
    (assoc state
           :synaptic-input (mathjs/dotDivide
                            synaptic-input
                            (mathjs/add 1 attenuation-malus))
           :attenuation-malus attenuation-malus)))


;; ================
;; intrinsic excitability plasticity
;; ================
;;
;; https://faster-than-light-memes.xyz/biological-notes-on-the-vehicles-cell-assemblies.html
;; *Iceberg Cell Assemblies*
;;
;; Paper: https://www.biorxiv.org/content/10.1101/2020.07.29.223966v1
;;
;;
;; quote:
;; we find increases in neuronal excitability, accompanied by increases in membrane resistance and a reduction in spike threshold. We conclude that the formation of neuronal ensemble by photostimulation is mediated by cell-intrinsic changes in excitability, rather than by Hebbian synaptic plasticity or changes in local synaptic connectivity. We propose an “iceberg” model, by which increased neuronal excitability makes subthreshold connections become suprathreshold, increasing the functional effect of already existing synapses and generating a new neuronal ensemble.
;;___
;;
;;
;; So instead of hebbian plasticity, we can try to model a cell-intrinsic `excitability`, `intrinsic-excitability`.
;;
;; The easiest model coming to mind is the same as the attenuation above. (but inverted).
;; A cummulative excitability, with a decay.
;;
;;
;; `excitability-growth`: Could have been called learning rate, to make it sound like machine learning
;;
;; `excitability-decay`: Relative decay each time step
;;
;; `excitabilities`: You could imagine pre-allocating this, probably this is somewhat random in biology.
;;
;;
;; 0. Grow excitability for all active neurons, (add excitability-growth)
;; 1. Decay excitabilities multiplicatively by excitability-decay
;; 2. excitabilities multiplicatively on the sum of the inputs for each neuron.
;;
;;
;; 0-2 could also be more complecated functions
;;
;; An excitability of 0.0 means your inputs are at baseline.
;; An excitability of 1.0 means your inputs count double and so forth.
;; In principle, negative numbers are allowed. (but not utilized by this step model here).
;; In this case this would flip into an attenuation or depression model.
;;
;; -1.0 is the minimal meaningful number, saying that synaptic input is 0, beyond that you would
;; get negative synaptic-inputs, which are not defined behaviour.
;;
;;

(defn intrinsic-excitability
  [{:as state
    :keys [excitabilities excitability-growth
           excitability-decay synaptic-input n-neurons
           activations]}]
  (let [excitabilities
        (or excitabilities
            (mathjs/matrix
             (mathjs/zeros
              #js
              [n-neurons]
              )))
        ;; decay excitabilities
        excitabilities (mathjs/multiply
                        excitabilities
                        (- 1 excitability-decay))
        excitabilities
        ;; accumulate the excitabilities on everybody
        ;; active
        (.subset excitabilities
                 (mathjs/index activations)
                 (mathjs/add (mathjs/subset
                              excitabilities
                              (mathjs/index activations))
                             excitability-growth))]
    (assoc state
           ;; if you have excitability, your inputs count
           ;; more
           :synaptic-input
           (mathjs/dotMultiply
            synaptic-input
            (mathjs/add 1 excitabilities))
           :excitabilities excitabilities)))

;; ================
;; Skipping Rate
;; ================
;;
;; Neuron 'failure rate' to fire.
;;
;; This is biologically plausible. At first glance this looks like an imperfection,
;; but I am reasononing that this would modify the dynamism of the substrate.
;;
;; Higher neuron failure rate is similar to attenuation, it makes the ensembles spread more
;; and be less stuck. But high attenuation makes the move around, where the skip rate makes them spread more
;; if applied before threshold model, and simply smaller if applied after the threshold model.
;;
;; Intuitively, this means you might find other attractor states, if you have some neurons that usually strongly
;; contribute to one interpretation, now you have the chance to find a new interpretation.
;;
;; This has been described under the topic of 'assembly shift' [insert paper]
;;
;; This could be applied before or after the threshold model, with distinct results.
;;
;; Skip rate is counterintiuitive, if one thinks of single neurons, and might (or not) be implemented 'explicitly' biochemically in brain.

(defn neuron-skip
  "Give every neuron at each timestep a chance to =skip=.

  `skip-rate`: The chance for each neuron to recieve 0 inputs, effectively skipping it.
  "
  [{:as state
    :keys [synaptic-input n-neurons activations skip-rate]}]
  (update
   state
   :synaptic-input
   (fn [inputs]
     (mathjs/dotMultiply
      inputs
      (.map (mathjs/zeros n-neurons)
            (fn [v idx _]
              (if (< (mathjs/random) skip-rate) 0 1)))))))


(defn neuron-skip-inhibition
  "Give every neuron at each timestep a chance to =skip=.

  This is an inhibition model modifier, applied `after` the threshold model.
  "
  [{:as state :keys [skip-rate n-neurons]}]
  (update
   state
   :activations
   (fn [activations]
     (let
         [skip? (mathjs/filter
                 (mathjs/range 0 n-neurons)
                 (fn [_] (< (mathjs/random) skip-rate)))]
         (if-not (< 0 (mathjs/count skip?))
           activations
           (mathjs/setDifference
            activations
            skip?))))))


;; ==================================
;; random, intrinsic firing rate
;; ==================================

(defn intrinsic-firing-rate
  "Applied after the threshold model.

  Says that neurons are allowed to fire at each time step with chance `intrinsic-firing-rate`.
  "
  [{:as state :keys [n-neurons intrinsic-firing-rate]}]
  (update state
          :activations
          (fn [activations]
            (mathjs/setUnion
              (or activations #js [])
              (mathjs/filter (mathjs/range 0 n-neurons)
                (fn [_]
                  (< (mathjs/random)
                     intrinsic-firing-rate)))))))
