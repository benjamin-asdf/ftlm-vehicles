(ns ftlm.vehicles.art.neuronal-area
  (:require
   [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
   [quil.core :as q :include-macros true]
   [ftlm.vehicles.art.extended :as elib]
   [ftlm.vehicles.art.controls :as controls :refer
    [versions]]
   [ftlm.vehicles.art.user-controls :as
    user-controls]
   [ftlm.vehicles.art.grid]
   [goog.style]
   [ftlm.vehicles.hdv]
   ["mathjs" :as mathjs]
   [ftlm.vehicles.assembly-calculus :as ac]))

(defn ->neuronal-area-ac-1
  [{:as opts :keys [spacing grid-width draw-i]}]
  (lib/->entity
    :nn-area
    (merge
      {:color (:cyan controls/color-map)
       :draw-functions
       {:1
        (fn [e]
          (let [neurons (ac/read-activations (:ac-area e))
                i->pos (fn [i] ((e :i->pos) e i))
                i->color (if (:i->color e)
                           ((:i->color e) e)
                           (constantly (lib/->hsb (:cyan controls/color-map))))]
            (q/with-stroke
              nil
              (doall
               (for [i neurons
                     :let [pos (i->pos i)]]
                 (q/with-fill (i->color i)
                   (q/with-translation
                     pos
                     (if draw-i
                       (draw-i i)
                       (q/rect 0 0 15 15 3)))))))))}
       :i->pos (fn [{:keys [transform]} i]
                 (let [[x y] (:pos transform)
                       coll (mod i grid-width)
                       row (quot i grid-width)
                       x (+ x (* coll spacing))
                       y (+ y (* row spacing))]
                   [x y]))
       :next-color (constantly (:cyan controls/color-map))
       :spacing spacing}
      opts)))

(defn ->neuronal-area-ac
  [{:as opts :keys [frequency]}]
  (assoc
   (->neuronal-area-ac-1 opts)
   :on-update-map
   {:update-neurons
    (lib/every-n-seconds
     (/ 1 frequency)
     (fn [e _s _]
       (if-not (-> e
                   :ac-area
                   :inhibition-model)
         e
         (-> e
             (update :ac-area ac/update-neuronal-area)
             (assoc :color ((:next-color e)))))))}))

(defn ->flash-lines
  [s indices {:as e :keys [color]} n-area]
  (lib/append-ents s
                   (for [i indices]
                     (assoc (elib/->flash-of-line
                             (lib/position e)
                             ((:i->pos n-area) n-area i))
                            :color color))))

(defn ->stimulus [color]
  (lib/->entity
   :circle
   {:stimulus? true
    :color color
    :draggable? true
    :transform (lib/->transform [100 100] 30 30 1)}))



;; -------------------------------------
;; Little happy sensoric field
;; ----------------------------------
;; a place, when you move a stimulus inside it,
;; we say that the system can percieve the stimulus
;; The model is a stimulus bag, a is set of currently percieved stimuli
;;
(defn ->sensoric-field
  [pos]
  (let [flash-inputs (atom {})]
    (->
      (lib/->entity
        :rect
        {:color (lib/with-alpha (lib/->hsb controls/white)
                  0)
         :corner-r 5
         :draw-functions
           {:stimuli (fn [e]
                       (let [stimuli (:stimuli e)]
                         (doall
                           (for [[idx {:keys [color]}]
                                   (map-indexed
                                     vector
                                     (map
                                       (lib/entities-by-id
                                         (q/state))
                                       stimuli))]
                             (q/with-fill
                               (lib/->hsb color)
                               (q/with-stroke
                                 (lib/->hsb color)
                                 (q/with-translation
                                   (lib/position e)
                                   (q/ellipse
                                     (+ 20
                                        (* -1
                                           (/ (-> e
                                                  :transform
                                                  :width)
                                              2))
                                        (* idx 20))
                                     (* 0.8
                                        (/ (-> e
                                               :transform
                                               :height)
                                           2))
                                     10
                                     10))))))))}
         :on-excite
           {:1 (fn [e s]
                 {:updated-state
                    (-> s
                        (lib/append-ents
                          (into
                            []
                            (repeatedly
                              3
                              #(merge
                                 (lib/clone-entity e)
                                 {:lifetime 1
                                  :on-update-map
                                    {:dance
                                       (lib/every-n-seconds
                                         0.1
                                         (fn [e _ _]
                                           (update-in
                                             e
                                             [:transform
                                              :scale]
                                             +
                                             (rand-nth
                                               [-0.05
                                                0.05]))))}
                                  :stroke-weight 1})))))})}
         :sensoric-field? true
         :stimuli #{}
         :stroke controls/white
         :stroke-weight 2
         :transform (lib/->transform pos 100 200 1)
         :z-index -10})
      (lib/live
        [:sensitivity
         (lib/every-n-seconds
           1
           (fn [e s _]
             (let [current (:stimuli e)
                   next-stimuli
                     (into #{}
                           (comp (filter :stimulus?)
                                 (filter #(lib/point-inside?
                                            e
                                            (lib/position
                                              %)))
                                 (map :id))
                           (lib/entities s))
                   new (clojure.set/difference next-stimuli
                                               current)]
               {:updated-state
                  (cond-> s
                    :next (assoc-in [:eid->entity (:id e)
                                     :stimuli]
                            next-stimuli)
                    (seq new)
                      (lib/append-ents
                        (map (fn [id]
                               (let
                                 [color
                                    (:color
                                      ((lib/entities-by-id
                                         s)
                                        id))]
                                 (merge
                                   (lib/clone-entity e)
                                   {:lifetime 10
                                    :on-update-map
                                      {:fade
                                         (lib/->fade-pulse-2
                                           2.0
                                           :stroke)}
                                    :stroke color
                                    :stroke-weight 2})))
                             new)))})))]))))


;; Makes a neuron step when you tab it
;; Makes neuron time steps, when it is inside the sensory area
(defn ->touch-me-gate
  [pos
   ;; sensoric-excite
  ]
  (lib/->entity
    :circle
    {:clickable? true
     :z-index 10
     :color (lib/with-alpha (lib/->hsb controls/white) 0)
     :draggable? true
     :last-clicked nil
     :on-click-map
       {:1 (fn [e s _]
             (when-not (when-let [last-clicked
                                    (:last-clicked e)]
                         (< (- (:last-tick s) last-clicked)
                            200))
               {:updated-state
                  ((lib/update-update-functions-map-1
                     :on-excite)
                    s)}))
        :2 (fn [e s _]
             (when-not (when-let [last-clicked
                                    (:last-clicked e)]
                         (< (- (:last-tick s) last-clicked)
                            200))
               {:updated-state
                  (-> s
                      (update-in
                        [:eid->entity (:id e)]
                        (fn [e]
                          (-> e
                              (assoc :last-clicked
                                       (:last-tick s))
                              (assoc :color controls/white)
                              (assoc-in
                                [:on-update-map :flash]
                                (lib/for-n-seconds
                                  1.0
                                  (lib/->fade-pulse-2 0.1)
                                  (fn [e _ _]
                                    (assoc e
                                      :color
                                        (lib/with-alpha
                                          (lib/->hsb
                                            controls/white)
                                          0)))))))))}))}
     ;; :on-update-map
     ;; {:flashing-stimuli
     ;;  (lib/every-n-seconds
     ;;   5.0
     ;;   (fn [e s _]
     ;;     (when-let [st (first
     ;;                  (shuffle
     ;;                   (sequence
     ;;                    (comp (keep :stimuli)
     ;;                          (mapcat identity)
     ;;                          (map
     ;;                          (lib/entities-by-id
     ;;                                s)))
     ;;                    (lib/entities s))))]
     ;;       (-> e
     ;;           (assoc :color (lib/with-alpha (:color
     ;;           st) 0))
     ;;           (assoc-in
     ;;            [:on-update-map :flash]
     ;;            (lib/for-n-seconds
     ;;             2.0
     ;;             (lib/->fade-pulse-2 2.0)
     ;;             (fn [e _ _]
     ;;               (assoc e
     ;;                      :color
     ;;                      (lib/with-alpha
     ;;                        (lib/->hsb
     ;;                         controls/white)
     ;;                        0))))))
     ;;       e)))}
     :stroke controls/white
     :stroke-weight 2
     :transform (lib/->transform pos 30 30 1)}))
