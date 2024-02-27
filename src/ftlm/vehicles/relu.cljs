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

((linear-1 [0 1]) [[10 5] 1])

;; ------------------------------------------------------
;; An artificial neuron is a linear combination of the inputs, followed by a non-linear decider function
;; -------------------------------------------------------

(defn relu [target]
  (fn [theta]
    (rectify ((linear-1 target) theta))))

((relu [0 1]) [[10 5] 1])
((relu [-1 -1]) [[10 5] 1])


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

(defn k-relu
  [k target theta]
  (cond
    (zero? k) target
    :else
    (k-relu (dec k) ((relu target) theta) (drop 2 theta))))

;; a 1 + 2 + 3 with a k-relu
(k-relu 1 [1 2 3] [[1 1 1] 0])

(k-relu 2 [1 2 3] [[1 1 1] 0 [1 1 1] 0])



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
