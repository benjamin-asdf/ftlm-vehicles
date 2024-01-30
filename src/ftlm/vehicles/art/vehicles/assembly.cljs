(ns ftlm.vehicles.art.vehicles.assembly
  (:require
   [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
   [ftlm.vehicles.art :as art]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [ftlm.vehicles.art.controls :refer [color-map]]
   [ftlm.vehicles.art.controls :as controls :refer [versions]]
   [ftlm.vehicles.art.user-controls :as user-controls]))

(defn env [state]
  {:ray-sources
   (into [] (filter :ray-source?) (lib/entities state))})

;; (defmulti build-entity first)
;; (defmethod build-entity :default [[kind opts]] ((builders kind) opts))
;; (defn ref? [v] (and (sequential? v) (= (first v) :ref)))

(defn ->ray-source [opts]
  (lib/->ray-source
   (assoc opts :shinyness false)))

(defn draw-state [state]
  (q/background (lib/->hsb (-> state :controls :background-color)))
  (lib/draw-entities state))

(defn update-entity [entity state]
  (let [env (env state)]
    (-> entity

        (lib/update-body state)
        lib/brownian-motion
        lib/friction

        lib/dart-distants-to-middle

        lib/move-dragged
        lib/update-rotation
        lib/update-position

        (lib/update-sensors env)
        lib/activation-decay
        lib/activation-shine
        lib/shine
        lib/update-lifetime)))

(defn update-state
  [state]
  (let [current-tick (q/millis)
        state (update state :controls merge (user-controls/controls))
        dt (* (:time-speed (lib/controls))
              (/ (- current-tick (:last-tick state)) 1000.0))]
    (binding [*dt* dt]
      (-> state
          (assoc :last-tick current-tick)
          lib/update-update-functions
          lib/update-state-update-functions
          lib/apply-events
          (lib/update-ents #(update-entity % state))
          lib/transduce-signals
          lib/track-components
          lib/track-conn-lines
          lib/ray-source-collision-burst
          lib/kill-entities))))

(defn ->block []
  [ ;; (merge
   ;;  (lib/->entity :rect)
   ;;  {:color {:b 100 :h 100 :s 100}
   ;;   :transform (lib/->transform [200 200] 100 100 1)})
   (merge
    (lib/->entity :rect)
    {
     :color
     ;; (:orange color-map)
     ;; (:hit-pink color-map)
     (:heliotrope color-map)
     :corner-r 10
     :transform (lib/->transform [200 200] 40 40 1)})])

(defn setup
  [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (q/background (lib/->hsb (-> controls :background-color)))
  (let [state {:controls controls
               :on-update []}]
    (-> state
        (lib/append-ents (->block)))))

(defn on-double-click
  [state id]
  (let [e ((lib/entities-by-id state) id)
        explosion (lib/->explosion {:color (:color e)
                                    :n 20
                                    :pos (lib/position e)
                                    :size 10
                                    :spread 10})]
    (-> state
        (assoc-in [:eid->entity id :lifetime] 0.6)
        (update-in [:eid->entity id :on-update] conj (lib/->grow 0.2))
        (lib/append-ents explosion))))

(defn double-clicked? [{id-1 :id time-old :time} {id-2 :id time-new :time}]
  (and
   (= id-2 id-1)
   (< (- time-new time-old) 300)))

(defn mouse-pressed
  [state]
  (if-let [draggable (lib/find-closest-draggable state)]
    (let [new-selection {:id (:id draggable) :time (q/millis)}
          old-selection (:selection state)
          state (-> state
                    (assoc :pressed true)
                    (assoc-in [:eid->entity (:id draggable) :dragged?] true)
                    (assoc :selection new-selection))]
      (cond-> state
        (double-clicked? old-selection new-selection)
        (on-double-click (:id draggable))))
    state))

(defn mouse-released
  [state]
  (-> state
      (assoc :pressed false)
      (lib/update-ents (fn [e] (dissoc e :dragged?)))))

(defn rotate-entity
  [state id rotation]
  (update-in state [:eid->entity id :transform :rotation] + rotation))

(defn mouse-wheel [state rotation]
  (if-let [ent ((lib/entities-by-id state) (-> state :selection :id))]
    (rotate-entity state (:id ent) (/ rotation 60 2.5))
    state))

(defn sketch
  [host {:keys [width height]} controls]
  (let [[screen-width screen-height] (lib/window-dimensions)
        width (cond (= width "max") screen-width width width :else screen-width)
        height (cond (= height "max") screen-height height height :else screen-height)]
    (q/sketch :host host
              :renderer :p3d
              :size [width height]
              :setup (partial setup controls)
              :update update-state
              :draw draw-state
              :features [:keep-on-top]
              :middleware [m/fun-mode]
              :mouse-pressed mouse-pressed
              :mouse-released mouse-released
              :mouse-wheel mouse-wheel
              :frame-rate 30)))

(defonce restart-fn (atom nil))
(defmethod art/view "assembly"
  [{:as opts :keys [place version]}]
  (let [f (fn []
            (sketch place
                    opts
                    (merge (controls/default-versions "assembly")
                           (get-in versions ["assembly" version])
                           @user-controls/!app)))]
    (reset! restart-fn f)
    (f)))
(defmethod user-controls/action-button ::restart
  [_]
  (some-> @restart-fn (apply nil)))
