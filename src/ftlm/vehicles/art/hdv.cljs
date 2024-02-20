;; hyperdimensional vectors
;; thanks GigaSquid
;; --------------------------------------
;; fooling around with proof of concept for hyperdimensional vectors
;; later we use high performance libraries for this

;; reference impl in pure cljs

(ns ftlm.vehicles.art.hdv
  (:require
   [tech.v3.datatype.functional :as dtype-fn]))


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

(comment

  (dotimes [_ 100]
    (bundle (hdv) (hdv)))

  (let [x (hdv)
        y (hdv)
        xy (bundle x y)
        x+y (bind x y)]
    [(similarity x xy)
     (similarity x (hdv))
     (similarity x (protect x))
     (similarity x x+y)
     (similarity x (bind y x+y))])


  (unprotect (protect [1 2 3]))

  (do
    (def m-line bundle)
    (def e-line (fn [a b] (bundle (protect a) b)))
    (def s-line bind)
    (let [
          a (hdv)
          b (hdv)
          x (bundle a b)
          x-more-a (bundle x a)
          y (hdv)
          s (s-line x y)
          s-mix
          (->
           s
           (bundle x)
           (bundle y))]
      [(similarity x s)
       (similarity x s-mix)
       (similarity y s-mix)
       (similarity x (bind s-mix y))
       (similarity a (bind s-mix y))
       (similarity b (bind s-mix y))
       (similarity x-more-a (bind s-mix y))]))

  (do
    (def m-line bundle)
    (def e-line (fn [a b] (bundle (protect a) b)))
    (def s-line bind)
    (let [
          a (hdv)
          b (hdv)
          x (bundle a b)
          x-more-a (bundle x a)
          y (hdv)
          s (s-line x y)
          s-mix
          (->
           s
           (bundle x)
           (bundle y))]
      [(similarity x-more-a a)
       (similarity x-more-a b)
       (similarity x a)]))

  )
