;; hyperdimensional vectors
;; thanks GigaSquid
;; thanks Chris Neuremberger
;; --------------------------------------
;;
;; Just a first version using tmdjs
;; it is faster than pure cljs
;;

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
  (dtype/emap (fn [a b] (+ a b)) :in8 v1 v2))

(defn clip
  "Clips the hyperdimensional vector magnitude to 1 or -1.
   We can discard these because of the nature of the large vectors
   that the mangitudes do not matter.

  We also resolve ties with a 'fill with rands' strategy.
  (Not sure how useful that is).
  "
  [v]
  (->>
   v
   (dtype/emap #(min 1 %) :int8)
   (dtype/emap #(max -1 %) :int8)
   (dtype/emap #(if (zero? %) (binary-rand) %) :int8)
   dtype/clone))

(defn hdv []
  (dtype/clone (dtype/emap (fn [_] (binary-rand)) :int8 (dtype/make-container :int8 size))))

(defn bundle
  "Returns a new hypervector that is similar to both inputs.

  Thus this is an association implementation.

  (Braitenberg Mnemotrix, m-lines).

  "
  [v1 v2]
  (->
   (bundle-op v1 v2)
   (clip)))

(defn bind
  "Return a new hypervector that is dissimilar to both inputs.

  Bind can be reversed,

  (bind (bind A B) A) -> B

  Thus, this is akin to a associating a symbol to a value.
  Or making a key-value pair.

  ? Isn't it like the area of a rectangle, both sides contribute but it is something else?

  "
  [v1 v2]
  (dtype/clone (dtype/emap (fn [a b] (* a b)) :int8 v1 v2)))

(defn rotate-n
  [rdr n]
  (let [n-elems (count rdr)
        n (long n)]
    (dtype/reify-reader
     (dtype/elemwise-datatype rdr)
     n-elems
     (fn [idx]
       (nth rdr
            (mod (- idx n) n-elems))))))

(def protect-n rotate-n)

(defn protect
  "Protects a bundle value from additional bundling by rotating the hdv. Akin to a list / array function. More akin to push in a stack.

  ----

  Protect may also serve as `quote`.

  Thus (protect A) -> 'A.

  Where 'A refers to the symbol A, not its value.

  ----

  Ergo-lines, e-lines, sequence-association, directed-association:

  (bundle 'A B)

  A bundle of a protected hypervector 'A with a hypervector B may serve to represent a directed association.

  Thus, B follows A and so forth.

  Thus Alice kisses Bob, not the other way around.

  (Braitenberg Ergotrix, e-lines).

  ----

  You could also get e-lines by mixing with random noice, or dedicated 'time-step' hyper-data.

  See also: https://faster-than-light-memes.xyz/technical-notes-on-synthetic-psychology.html

  "
  [v]
  (rotate-n v 1))

(defn unprotect
  "Reverse the rotation of the bundle. Like pop of a stack."
  [v]
  (rotate-n v -1))

(defn magnitude
  "The magnitude of a hyperdimensional vector."
  [v]
  (dtype-fn/magnitude v))

(defn dot-product [v1 v2]
  (dtype-fn/sum (dtype/emap (fn [a b] (* a b)) :int8 v1 v2)))

(defn similarity
  [query-v v]
  (let [dotx (dot-product v query-v)]
    {:cos-sim (/ dotx (* (magnitude query-v) (magnitude v)))
     :dot dotx}))


(comment
  (dotimes [_ 100]
    (bundle (hdv) (hdv)))
  (magnitude [1 2 3])
  (dtype-fn/magnitude [1 2 3])

  (dotimes [_ 1000]
    (bundle (hdv) (hdv)))

  (dotimes [_ 100]
    (bind (hdv) (hdv)))

  (protect [1 2 3  ])

  (let [last (peek [1 2 3])]
    (dtype/set-value!
     [
      last]
     0
     (dtype-fn/shift [1 2 3] 1)))


  (dtype/clone (dtype/emap (fn [_] (binary-rand)) :int8 (dtype/make-container :int8 size)))
  (dtype/clone (dtype/make-container :int8 [1 2 3]))

  (dotimes [_ 100] (dot-product (hdv) (hdv)))

  (dotimes [_ 100]
    (dtype-fn/sum
     (dtype/emap (fn [a b] (* a b)) :int8 (hdv)
                 (hdv))))

  (let [x (hdv)]
    (=
     (protect-n x 1)
     (protect-n x 1)))


  (let [x (hdv)
        x-1 (protect-n x 1)]
    (=
     x
     (protect-n x-1 -1)))

  (dtype-fn/shift
   (dtype/clone
    (dtype/make-container :int8 [1 2 3]))
   1)

  (dotimes
      [_ 1000]
      (into [] (dtype/emap
                (fn [_] (binary-rand))
                :int8
                (dtype/make-container :int8 size))))

  (let [x (dtype/clone (dtype/emap (fn [_] (binary-rand)) :int8 (dtype/make-container :int8 size)))]
    (apply = [(into [] x)
              (into [] x)]))


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
          ]
      [(similarity x-more-a a)
       (similarity x-more-a b)
       (similarity x a)])))
