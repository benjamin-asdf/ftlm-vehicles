(ns ftlm.vehicles.art.vehicles.cerebellum.purkinje
  (:require
   [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
   [quil.core :as q :include-macros true]
   [ftlm.vehicles.art.extended :as elib]
   [ftlm.vehicles.art.controls :as controls :refer
    [versions]]
   [ftlm.vehicles.art.user-controls :as
    user-controls]
   [clojure.set :as set]
   [ftlm.vehicles.art.grid]
   [goog.style]
   [ftlm.vehicles.hdv]
   [ftlm.vehicles.audio :as audio]
   [ftlm.vehicles.assembly-calculus :as ac]
   ["mathjs" :as mathjs]))


;; cerebellar cortex :

;;
;;
;;            slaps of purkinje cell dendrites
;;                  (I just call them p-slaps)
;;               |
;;               |
;;               |
;;               |
;;               v
;;           -------                   ---- -------+                                                   - pf are slow conducting
;;          /  /  /                               /|                   molecular layer                 - pf never bend (anatomy)
;;         /  /  /                               / |
;;        /  /  /                               /  |              --+
;;       /--/--/-------------------------------/   |                |           <--------o--------> pf
;;       |  |  |                        +-     |                    |           |-----------------|
;;       |  |  |                        +--    |                    |
;;       +--+--+-------+----------------+------+-------------> pf1  |           contacts 200.000 Purkinje cell dendrites
;;       +--+--+----+--+----------------+------+-------------> pf2  |
;;       |  |  |    |  |                +-     |  /                 |           o-----------------> pf
;;       |  |  |    |  |                +-     | /                 -+           |-----------------|
;;       +--+--+----+--+----------------+------+/
;;        p1 p2     g g2                | p   p  granular layer                 assymetric, if cell body is located to a side.
;;                  ^                   | ^   |
;;                  |                   | |   |
;;                  |                     |   |
;;       -----------+           climbing  |   +---------------|   [ deep cerebellar nuclei ]
;;     Mossy fiber              fibres    |                       [ vestibular nuclei]
;;                                        |
;;                                        |                       The sole output of the whole thing are
;;                                                                the inhibotory axons of Purkinje cells
;;                            [inferior olivary nucleus]
;;
;;                            1 fiber, 1 Purkinje cell
;;                            ++ excitation
;;                            synchronized
;;
;;
;;  p - Purkinje cell body
;;  pf - parallel fibre
;;
;;
;;
;;

;; Purkinje cells (p1,p2...) make large flat slaps of dendritic trees stacked next to each other
;; Granular cells (g) make axons that devide to =parallel-fibers= (pf), in a T shape.
;;
;; These are slow conducting fibres that go through many dendritic trees of Purkinje cells.
;; In the frog the parallel fibres go all the way from the left to the right.
;; In human, they are 'staggered in all possible realtive positions'.
;;
;; Parallel fibres make synapses to the dendrites of Purkinje cells where they cross.
;; (They make inputs to molecular layer).
;; Several hundred in succession. In human 200.000 when you make the lenght calculation.
;;
;;
;; Mossy fibres:
;; - Come from outside cerebellum,
;; - Each mossy fibres goes to many granular cells, in a restricted area of cerebellum
;;
;; Input to Purkinje cells is solely from inferior olivary nucleus via so called climing fibres.
;; Interestingly, these make the strongest EPSPs in brain. Also, the neurons of olivary nucleus make gap junctions,
;; so you get a strongly correlated, intense acivation for some reason.
;; Climbing fibres are 1-10:1 early in development and 1:1 in adult.
;; This 1 companion climbing fibres branches together with the dendritic tree of it's Purkinje cell.
;; (Quite remarkable).
;; (Note I have not quite understood yet whether inferior olivary nucleus always excites everthing at ones, or whether
;; it sort of have segments, but apparently inferior olivary nucleus is remarketly correlated.
;;
;; Purkinje cells make inhibitory axons to deep cerebellar nuclei.
;;
;;
;; Whenever cerebellum is folded, it is folded in a way that preserves the parallel fibres,
;; basically the parallel fibres don't bend.
;; (Compared to cerebrum, this is very different. In cerebrum the axons just bend, and what is preserved is the height
;; of a cortical column).
;; The molecular layer has a constant thickness.
;; The anatomy preserves the arrangment of parallel fibres and Purkinje cells. I.e the distances of the p-slaps
;; are held constant. So that when you count how many p-slaps a parallel fibre is passing, this is constant.
;; (We can conclude that this is plays an essential role in the computation being done).
;;
;;
;; Braitenberg 1977:
;; In contrast to other folded planar brain structures, which look like
;; the surface of a walnut, or like a carelessly wrinkled cloth (for exam-
;; ple, the cerebral cortex of larger mammals, the olivary nucleus of pri-
;; mates, the dentate nucleus), this parallel folding is a particularly strik-
;; ing characteristic of the cerebellar cortex. How may we interpret the
;; folding of cortex-like structures, and how can we explain that in close-
;; ly related species the same organ makes its appearance now as
;; a smooth plate, and now as a folded structure? When there is such
;; a difference, it is striking that the folding makes its appearance in the
;; larger of two related species. If one had to design an enlarged version
;; of a small monkey, one evidently could not simply copy the whole
;; organism on a larger scale, just as an ocean liner cannot be just
;; a larger edition of a fisherman's skiff. One of the reasons for this is
;; that oars cannot be enlarged proportionally, since their size must
;; maintain a certain relation to particles of constant size, namely to the
;; people by whom they will be handled.
;; It seems that the thickness of cortices of animals of different sizes
;; remains fairly constant: evidently the thickness is connected with the
;; fundamental operation that takes place there. The increase in size of
;; the cortex of larger animals is achieved mainly through an increase of
;; the surface extension. If one assumes that the number of problems for
;; which the cortex is responsible increases with the volume of the ani-
;; mal, or with the third power of the linear dimensions, a true-to-scale
;; enlargement of the surface of the cortex, which goes with the second
;; power of the linear dimensions, would not be enough. In large ani-
;; mals the cortex has to be folded.
;;
;;
;; - Looks like the same computational trick is done over and over, for all functions of cerebellum
;; - Cerebellar cortex continues across the midline, this is probably relevant because usually
;;   brain has bilateral symmetry
;; -
;;
;; - 'The difference in the time of arrival of signals between neighboring Purkinje cell trees turns out to be about
;;    a tenth of a millisecond'
;;
;; - 'I maintain that with this thought of the cerebellar cortex as a clock in the millisecond range, all essential peculiarities of the cerebellar structure can be explainend.'
;;
;; - https://en.wikipedia.org/wiki/Climbing_fiber
;; - https://www.sciencedirect.com/topics/immunology-and-microbiology/climbing-fiber
;; - https://www.sciencedirect.com/science/article/abs/pii/B9780123742452000097i/B9780123742452000097
;; - https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1460-9568.1995.tb00653.x
;; - Valentino Braitenberg: On the Texture of Brains 1977 (one of the most fascinating neuroscience books I have read).
;;
;;
;; 'The movement must be grasped as a whole, and it is then programmed in detail by an unconscious mechanism.'
;;
;;
;;
;;
;; Modern neuroscience discerns a bunch of other neuron types in this arragnment.
;; called basket cells and so forth.
;;

(defn ->single-row-purkinjes
  [{:as opts
    :keys [->activations draw-i grid-width spacing]}]
  (lib/->entity
    :neurons
    (merge
      {:color (:cyan controls/color-map)
       :draw-functions
         {:1 (fn [e]
               (let [neurons (.valueOf (->activations e))
                     i->pos (fn [i] ((e :i->pos) e i))
                     i->color (if (:i->color e)
                                ((:i->color e) e)
                                (constantly (lib/->hsb
                                              (:color e))))
                     active? (into #{} neurons)]
                 (q/with-stroke
                   nil
                   (doall
                     (for [i neurons
                           :let [pos (i->pos i)]]
                       (q/with-fill
                         (i->color i)
                         (q/with-translation
                           pos
                           (if draw-i
                             (draw-i i (active? i))
                             (q/rect 0 0 4 30 3)))))))))}
       :i->pos (fn [{:keys [transform]} i]
                 (let [[x y] (:pos transform)
                       coll (mod i grid-width)
                       row (quot i grid-width)
                       x (+ x (* coll spacing))
                       y (+ y (* row spacing))]
                   [x y]))
       :spacing spacing}
      opts)))

(defn allocate-segment
  [model how-many side]
  (update model
          :activations
          (fn [act]
            (mathjs/setDifference
             act
             (case side
               :left
               (mathjs/range 0 how-many)
               :right
               (mathjs/range
                (- (:n-neurons model) how-many)
                (:n-neurons model)))))))
