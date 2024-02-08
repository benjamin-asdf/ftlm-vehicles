(ns ftlm.vehicles.art.vehicles.cell-assemblies
  (:require [clojure.walk :as walk]
            [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
            [ftlm.vehicles.art :as art]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [ftlm.vehicles.art.controls :as controls :refer
             [versions]]
            [ftlm.vehicles.art.user-controls :as
             user-controls]
            [goog.style]))

;; -> The pyramidal cells
;; I decide it makes sense to think of pyramidal cell activity as the center
;; of explanation of cortex.
;; See Braitenberg 1986
;; --
;; There are good reasons to consider the most numerrous cell type, the pyramidal cells,
;; as the basic neuronal equipment of the cortex.
;; The great majoriyt of the synapses in the cerebral cortex have pyramidal neurons
;; on both the presynaptic and postsynaptic sides.
;; ---
;; He then goes on to explain why he thinks pyramidal cells are excitatory.
;; (Very insightful)
;; 1. Cerebral cortex and especially the hippocampal region is susceptible to epileptic activity
;; If you have most neurons in hiccopumppus that maps onto the most numerous cell type being excitatory.
;; 2. Corpus callosum is excitatory because you can prevent epileptic activity from spreading by cutting it.
;; 3. The axons of cortical pyramidal cells that go to distant places, such as the spinal cord, make excitatory connections.
;;
;; -> In my words, everything speaks for, and nothing speaks against, the pyramidal cells being excitatory.
;; I think it is expecially striking, that the main cell type makes connections to the outside motor neurons.
;; It just fits with the rest of how I would interpret the system, that whatever is making output is the driver
;; of cerebral function.
;;
;; (he was right, they are excitatory).
;; Another cool insight from Braitenberg:
;; Imagine you sprinkle a few inhibitory neurons in between, now you can describe the absence of something.
;;

;; I supoose McCulloch drew them as black triangles, because of the golgi stain of the time. This is how they looked.
;; But now we have this cool brainbow in our minds, so I am allowed to give my neurons all kinds of colors, too.
;; [youtube Heinz von Foerster 'Tanz Mit Der Welt' why I think McCulloch drew black triangles]

(defn ->ray-source [opts]
  (lib/->ray-source
   (assoc opts
          :shinyness false
          :color controls/white)))

(defn draw-state
  [state]
  (q/background (lib/->hsb (-> state :controls :background-color)))
  (q/stroke-weight 1)
  (q/stroke 0.3)
  (lib/draw-entities state))

(defn update-entity
  [entity state env]
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
      lib/update-lifetime))

(def the-state (atom {}))

(defn update-state
  [state]
  (let [current-tick (q/millis)
        state (update state :controls merge (user-controls/controls))
        dt (*
            (:time-speed (lib/controls))
            (/ (- current-tick (:last-tick state)) 1000.0))
        state
        (binding [*dt* dt]
          (-> state
              (assoc :last-tick current-tick)
              lib/update-update-functions
              lib/update-state-update-functions
              lib/apply-events
              (lib/update-ents #(update-entity % state (lib/env state)))
              lib/transduce-signals
              lib/track-components
              lib/track-conn-lines
              lib/ray-source-collision-burst
              lib/kill-entities))]
    (reset! the-state state)
    state))

(defn ->neuron
  [{:keys [col-k pos]}]
  (merge (lib/->entity :triangle)
         {:activation 1000
          :activation-shine? false
          :color (col-k controls/color-map)
          :draggable? true
          :on-update-map
          (merge
           (lib/->breath 1 1.2 1)
           (when (= col-k :orange)
             {:activation
              (lib/every-n-seconds
               5
               (fn [e s k]
                 (update-in e [:activation] + 2000)))}))
          :on-double-click-map
          {:activation
           (fn [e s k]
             (update-in e [:activation] + 1000))}
          :transform (assoc (lib/->transform pos 40 30 1)
                            :rotation (lib/normal-distr
                                       0
                                       (/ q/QUARTER-PI 20)))}))

(defn rand-neurons
  [layers per-layer]
  (for [l (range layers)
        n (range per-layer)]
    (let [roughly-y (* 50 (inc (inc l)))
          roughly-x (* 30 (inc (inc (inc n))))]
      (->neuron {:col-k (rand-nth [:cyan :heliotrope :red
                                   :mint :yellow :orange
                                   :magenta])
                 :pos [(lib/normal-distr roughly-x 20)
                       (lib/normal-distr roughly-y 20)]}))))

(defn setup
  [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (q/background (lib/->hsb (-> controls
                               :background-color)))
  (let [state {:controls controls :on-update []}]
    (-> state
        (lib/append-ents
         (rand-neurons 10 20)))))

(defn on-double-click
  [state id]
  (lib/call-double-clicks state ((lib/entities-by-id state) id)))

(defn double-clicked? [{id-1 :id time-old :time} {id-2 :id time-new :time}]
  (and
   (= id-2 id-1)
   (< (- time-new time-old) 300)))

(defn mouse-pressed
  [state]
  (if-let
      [draggable (lib/find-closest-draggable state)]
      (let [new-selection {:id (:id draggable) :time (q/millis)}
            old-selection (:selection state)
            state (-> state
                      (assoc :pressed true)
                      (assoc-in [:eid->entity (:id draggable) :dragged?] true)
                      (assoc :selection new-selection))
            state ((lib/->call-callbacks :on-click-map) state draggable)]
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
    (update-in state [:eid->entity (:id ent) :angular-acceleration] + (/ rotation 60 2.5))
    state))

(defn sketch
  [host {:keys [width height]} controls]
  (let [[screen-width screen-height] (lib/window-dimensions)
        width (cond (= width "max") screen-width width width :else screen-width)
        height (cond (= height "max") screen-height height height :else screen-height)
        width 1000
        height 800]
    (q/sketch :host host
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
(defmethod art/view "cell-assemblies"
  [{:as opts :keys [place version]}]
  (let [f (fn []
            (let [controls (merge (controls/default-versions "cell-assemblies")
                                  (get-in versions ["cell-assemblies" version])
                                  @user-controls/!app)]
              (sketch place opts controls)))]
    (reset! restart-fn f)
    (f)))

(defmethod user-controls/action-button ::restart
  [_]
  (some-> @restart-fn (apply nil)))
