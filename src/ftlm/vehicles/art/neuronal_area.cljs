(ns ftlm.vehicles.art.neuronal-area
  (:require
   [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [ftlm.vehicles.art.extended :as elib]
   [ftlm.vehicles.art.controls :as controls :refer
    [versions]]
   [ftlm.vehicles.art.user-controls :as
    user-controls]
   [ftlm.vehicles.art.grid]
   [goog.style]
   [ftlm.vehicles.hdv]
   [tech.v3.datatype.argops :as argops]
   [tech.v3.datatype.functional :as dtype-fn]
   ;; [tech.v3.dataset :as ds]
   [tech.v3.datatype :as dtype]
   ["mathjs" :as mathjs]
   [ftlm.vehicles.assembly-calculus :as ac]))

(defn ->neuronal-area-ac-1
  [{:as opts :keys [spacing grid-width draw-i]}]
  (lib/->entity
    :nn-area
    (merge
      {:color (:cyan controls/color-map)
       :draw-functions
         {:1 (fn [e]
               (let [neurons (ac/read-activations (:ac-area
                                                    e))
                     i->pos (fn [i] ((e :i->pos) e i))]
                 (q/with-stroke
                   nil
                   (doall (for [i neurons
                                :let [pos (i->pos i)]]
                            (q/with-translation
                              pos
                              (if draw-i
                                (draw-i i)
                                (q/rect 0 0 15 15 3))))))))}
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
