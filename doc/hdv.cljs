;; hyperdimensional vectors
;; thanks GigaSquid
;; ---------------------------
;; reference impl in pure cljs
;; ---------------------------


(ns ftlm.vehicles.hdv
  (:require
   [tech.v3.datatype.functional :as dtype-fn]
   [tech.v3.datatype :as dtype]))

(def size (long 1e4))

(defn binary-rand
  "Choose a random binary magnitude for the vector +1 or -1"
  []
  (if (> (rand) 0.5) -1 1))

(defn bundle-op
  "The bundle operation of addition"
  [v1 v2]
  (into [] (map + v1 v2)))

(defn clip
  "Clips the hyperdimensional vector magnitude to 1 or -1.
   We can discard these because of the nature of the large vectors
   that the mangitudes do not matter"
  [v]
  (into []
        (comp (map #(min 1 %))
              (map #(max -1 %))
              (map #(if (zero? %) (binary-rand) %)))
        v))

(defn hdv []
  (into [] (repeatedly size binary-rand)))

(defn bundle
  [v1 v2]
  (->
   (bundle-op v1 v2)
   (clip)))

(defn bind [v1 v2]
  (into [] (map * v1 v2)))

(defn protect
  "Protects a bundle value from additional bundling by rotating the hdv. Akin to a list / array function. More akin to push in a stack."
  [v]
  (into [] (concat (rest v) [(first v)])))

(defn unprotect
  "Reverse the rotation of the bundle. Like pop of a stack"
  [v]
  (into [] (concat [(peek v)] (butlast v))))

(defn protect-n
  "Calls protect n times"
  [v n]
  (loop [new-v v
         i n]
    (if (zero? i)
      new-v
      (recur (protect new-v) (dec i)))))

(defn magnitude
  "The magnitude of a hyperdimensional vector"
  [v]
  (Math/sqrt (reduce + (map #(* % %) v))))

(defn dot-product [v1 v2]
  (reduce + (map * v1 v2)))

(defn similarity
  [query-v v]
  (let [dotx (dot-product v query-v)]
    {:cos-sim (/ dotx (* (magnitude query-v) (magnitude v)))
     :dot dotx}))
