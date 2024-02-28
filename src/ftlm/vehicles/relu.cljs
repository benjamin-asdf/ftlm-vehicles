(ns ftlm.vehicles.relu
  (:require
   [tech.v3.datatype.functional :as dtype-fn]
   [tech.v3.datatype :as dtype]))

(defn dot-product [v1 v2]
  (dtype-fn/sum (dtype/emap (fn [a b] (* a b)) :int32 v1 v2)))

(defn linear-1 [target]
  (fn [theta]
    (+ (dot-product (nth theta 0) target) (nth theta 1))))

(defn rectify [x]
  (if (>= x 0) x 0))

;; linear 1 is equivalent to weighting the inputs, summing them and adding a bias

;; ((linear-1 [0 1]) [[10 5] 1])

;; ------------------------------------------------------
;; An artificial neuron is a linear combination of the inputs, composed with a non-linear decider function
;; -------------------------------------------------------

(defn relu [target]
  (fn [theta]
    (rectify ((linear-1 target) theta))))

(defn line [target]
  (fn [[w b]] (+ (* w target) b)))

;; a 1 dim version
(defn relu-1 [input]
  (fn [theta]
    (rectify ((line input) theta))))


(comment
  (map (relu-1 3) (map (juxt identity (constantly 1)) (range 10)))
  '(1 4 7 10 13 16 19 22 25 28)

  ((relu-1 10) [1 -10])
  ((relu-1 1) [1 -10])
  ((relu-1 20) [1 -10])
  )

(defn half-strip
  "

  (map (fn [i] (half-strip [[1 -2] [1 -4]] i)) (range 10))
  '(0 0 0 1 2 2 2 2 2 2)

  This is called a half strip

  ^
  |
  |              ---------------------   2
  |             /
  +------------/-----------------------  0
  |           2  3

  "
  [thetas input]
  (-
   ((relu-1 input) (first thetas))
   ((relu-1 input) (second thetas))))



;; ---------------------------------------------------------------------------------
;; (map (fn [i] (full-strip [[1 -2] [1 -4] [1 -6] [1 -8]] i)) (range 10))
;; '(0 0 0 1 2 2 2 1 0 0)

;; A full strip:

;; ^
;; |
;; |              ----------------\
;; |             /                 \
;; +------------/-------------------\---->
;; |           2  3

;; Now you can glue (sum) multiple full strips together like in a cake and approximate any function!
;; ------------------------------------------------------------------------------------------

(defn full-strip
  [thetas input]
  (- (half-strip (take 2 thetas) input)
     (half-strip (drop 2 thetas) input)))

(defn sum-of-full-strips
  [thetas input]
  (transduce (map #(full-strip % input))
             +
             input
             (partition 4 thetas)))

(defn ->random-full-strip-thetas
  [n]
  (partition-all 2
                 (dtype/emap
                  (fn [] (* (rand-nth [-1 1]) (rand)))
                   :int32
                   (dtype/make-container :int32 (* 2 n)))))

(defn ->random-full-strip
  [n]
  (let [thetas (->random-full-strip-thetas n)]
    (fn [i] (full-strip thetas i))))

(comment
  (half-strip [1 -10] 10)
  (half-strip [1 -10] 10)
  (map (fn [i] (half-strip [[1 -1] [1 -3]] i)) (range 10))
  (map (fn [i] (half-strip [[1 -3] [0 0]] i)) (range 10))
  (map (fn [i] (half-strip [[1 -2] [1 0]] i)) (range 10))

  (map (fn [i] (full-strip [[1 -2] [1 -4] [1 -6] [1 -8]] i)) (range 10))


  (0 0 0 1 2 2 2 1 0 0)

  (->random-full-strip 1)

  (let [strip (comp rectify (->random-full-strip 10))]
    (map (fn [i] (strip (- 1 (/ 1 i)))) (range 1 20)))

  (full-strip (->random-full-strip-thetas 1) 10)
  (full-strip (->random-full-strip-thetas 1) 10)

  (def random-fullstrip
    (repeatedly
     10
     (fn []
       (dtype/emap (fn [_] (rand-int 10)) :int32 (dtype/make-container :int32 8))))))

(defn k-relu-1
  [thetas input]
  (reduce (fn [input [w b]] ((relu-1 input) [w b]))
          input
          thetas))

(defn sum-of-relus-1 [thetas input]
  (reduce + (map (fn [theta] ((relu-1 input) theta)) thetas)))

(comment
  (map (fn [i] (sum-of-relus-1 [[5 0] [1 -10] [-1 0]] i))
       (range 10))
  (map (fn [i] (sum-of-relus-1 [[5 0]] i)) (range 10))
  (map (fn [i] (sum-of-relus-1 [[1 0]] i)) (range 10))
  (map (fn [i] (sum-of-relus-1 [[1 -1]] i)) (range 10))
  (map (fn [i] (sum-of-relus-1 [[1 -1] [1 -3]] i))
       (range 10))
  (map (fn [i] (sum-of-relus-1 [[1 -1] [-1 -3]] i))
       (range 10))
  (map (fn [i] (sum-of-relus-1 [[1 0] [1 0]] i)) (range 10))
  (map (fn [i] (sum-of-relus-1 [[5 0] [5 -10]] i))
       (range 10)))


(comment
  (k-relu-1 [[5 0] [1 -10] [-1 0]] 10)

  (k-relu-1 [[1 0]] 10)


  (map (fn [i] (k-relu-1 [[1 0] [1 -3] [1 0]] i)) (range 10))
  (map (fn [i] (k-relu-1 [[1 0] [1 -3]] i)) (range 10))
  (map (fn [i] (k-relu-1 [[1 0] [1 -3] [4 0]] i)) (range 10))

  (map (fn [i] (k-relu-1 [[1 0] [1 -3] [4 0] [1 -4]] i)) (range 10))


  (map (fn [i] (k-relu-1 [[1 0] [1 -3] [2 -5]] i)) (range 10))

  )



;; ((relu [0 1]) [[10 5] 1])
;; ((relu [-1 -1]) [[10 5] 1])


;; (defn k-relu
;;   [k]
;;   (fn [target]
;;     (fn [theta]
;;       (cond
;;         (zero? k) target
;;         :else
;;         ((k-relu (dec k))
;;          ((relu target) theta)
;;          (drop 2 theta))))))

;; (define k-relu
;;   (λ (k)
;;     (λ (t)
;;       (λ (theta)
;;         (cond
;;           ((zero? k) t)
;;           (else (((k-relu (sub1 k))
;;                   ((relu t) theta))
;;                  (refr theta 2))))))))

;; (defn k-relu
;;   [k target theta]
;;   (cond
;;     (zero? k) target
;;     :else
;;     (k-relu (dec k) ((relu target) theta) (drop 2 theta))))

;; a 1 + 2 + 3 with a k-relu
;; (k-relu 1 [1 2 3] [[1 1 1] 0])

;; (k-relu 2 [1 2 3] [[1 1 1] 0 [1 1 1] 0])



;; (defn feedforward-layer
;;   [inputs weights biases]
;;   (mapv (fn [neuron-weights neuron-bias]
;;           ((relu-neuron neuron-weights neuron-bias) inputs))
;;         weights
;;         biases))

;; (let [inputs [1.0 2.0 3.0]
;;       weights [[0.2 0.3 0.5] [0.1 0.4 0.4] [-0.3 0.1 0.2]]
;;       biases [0.1 -0.2 0.05]]
;;   (feedforward-layer inputs weights biases))

;; (defn deep-relu-network
;;   [input weights biases]
;;   (reduce (fn [layer-input [layer-weights layer-biases]]
;;             (feedforward-layer layer-input
;;                                layer-weights
;;                                layer-biases))
;;     input
;;     (map vector weights biases)))

;; (deep-relu-network
;;  [1.0 1.0]
;;  [[[10 2.0] [10 2.0]]
;;   [[10 2.0] [10 2.0]]]
;;  [[0.0 0.0] [0.0 0.0]])
