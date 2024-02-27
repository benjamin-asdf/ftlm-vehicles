(ns ftlm.vehicles.art.vehicles.cell-assemblies
  (:require
   [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
   [ftlm.vehicles.art :as art]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [ftlm.vehicles.art.extended :as elib]
   [ftlm.vehicles.art.controls :as controls :refer
    [versions]]
   [ftlm.vehicles.art.user-controls :as
    user-controls]
   [goog.style]
   [ftlm.vehicles.hdv]))

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
  ;; (lib/grid)
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

;; represents a signal
;; is a bit like a ray source

(defn ->toggle-active
  [on off]
  (fn
    ([e] ((:toggle-active e) e (not (:active? e))))
    ([e active?]
     (assoc
      (if active? (on e) (off e))
      :active? active?))))

(defn ->signal
  [opts]
  (let [toggle-active
          (->toggle-active
            (fn [e]
              (-> e
                  (assoc :particle? true)
                  (assoc :kinetic-energy 0.05)
                  (assoc :on-update [(lib/->circular-shine 1.5 (/ 15 3))])))
            (fn [e]
              (-> e
                  (assoc :particle? false)
                  (dissoc :on-update))))]
    [(->
      (lib/->ray-source
       (merge
        {:active? false
         :draggable? true
         :intensity 2
         :on-double-click-map
         {:toggle-active (fn [e _ _]
                           ((:toggle-active e) e))}
         :particle? false
         :shinyness false
         :toggle-active toggle-active}
        opts))
      first
      (toggle-active true))]))

(defn ->neuron
  [{:keys [col-k pos active?]}]
  (let [toggle-active
        (fn
          ([e] ((:toggle-active e) e (not (:active? e))))
          ([e active?]
           (-> e
               (assoc :active? active?)
               (assoc :on-update-map (when active? (elib/->breath 1 1.3 1)))
               (assoc-in [:transform :scale] (if active? 1.3 0.8))
               (assoc-in [:transform :rotation] (if active? 0 q/PI)))))
        e (merge (lib/->entity :triangle)
                 {:active? active?
                  :triangle-neuron? true
                  :color (col-k controls/color-map)
                  :activated-by #{col-k}
                  :draggable? true
                  :on-double-click-map
                  {:toggle-active
                   (fn [e _ _] ((:toggle-active e) e))}
                  :on-update-map (elib/->breath 1 1.2 1)
                  :toggle-active toggle-active
                  :transform
                  (assoc (lib/->transform pos 40 30 1)
                         :rotation (lib/normal-distr
                                    0
                                    (/ q/QUARTER-PI 20)))})]
    (toggle-active e (:active? e))))

(defn rand-neurons
  [{:keys [layers per-layer top left active?]}]
  (for [l (range layers)
        n (range per-layer)]
    (let [roughly-y (+ top (* 50 l))
          roughly-x (+ left (* 30 n))
          neuron
          (->neuron
           {:col-k (rand-nth [:cyan :heliotrope :red
                              :mint :yellow :orange
                              :magenta])
            :pos [(lib/normal-distr roughly-x 20)
                  (lib/normal-distr roughly-y 20)]})]
      (assoc neuron :active? (active? neuron l n)))))

(defmulti setup-version (comp :v :controls))

(defmethod setup-version :just-triangles
  [state]
  (-> state
      (lib/append-ents (rand-neurons {:layers 10 :per-layer 20 :top 150 :left 90 :active? (constantly true)}))))

(defn ->neighbour-lines
  []
  (let
    [adj-matrix (atom nil)
     neighbors-lines
       (fn [s]
         (let
           [adj-matrix
              (or
                @adj-matrix
                (reset! adj-matrix
                  (into
                    {}
                    (comp
                      (filter :triangle-neuron?)
                      (map
                        (fn [e]
                          (let
                            [position (lib/position e)
                             my-color (:color e)
                             neighbors
                               (->
                                 (into
                                   #{}
                                   (map :id
                                     (take
                                       4
                                       (sort-by
                                         (comp
                                           (fn [b]
                                             (lib/distance
                                               position
                                               b))
                                           lib/position)
                                         (sequence
                                           (comp
                                             (filter
                                               :triangle-neuron?)
                                             (filter
                                               (comp
                                                 #{my-color}
                                                 :color)))
                                           (lib/entities
                                            s))))))
                                 (disj (:id e)))]
                            [(:id e) neighbors]))))
                    (lib/entities s))))]
           (-> s
               (lib/append-ents
                 (into []
                       (comp
                         (filter :triangle-neuron?)
                         (map
                           (fn [e]
                             (let [neighbors (adj-matrix
                                               (:id e))
                                   to-e
                                     ((lib/entities-by-id s)
                                       (first (shuffle
                                                neighbors)))
                                   from-e e]
                               (merge
                                (lib/->entity :line)
                                {:transform
                                 (lib/->transform (lib/position from-e) 0 0 1)
                                 :end-pos (lib/position to-e)
                                 :color (lib/with-alpha (:color from-e controls/color-map) 0)
                                 :lifetime 3
                                 :on-update-map
                                 {:fade
                                  (lib/->fade-pulse 1.5)
                                  }})))))
                       (lib/entities s))))))]
    (fn [s _ _] (neighbors-lines s))))

(defmethod setup-version :neighbours-lines
  [state]
  (-> state
      (lib/append-ents (rand-neurons {:active? (constantly
                                                 false)
                                      :layers 10
                                      :left 90
                                      :per-layer 20
                                      :top 150}))
      (assoc :on-update-map {:neighbors-lines
                               (lib/every-n-seconds
                                 3
                                 (->neighbour-lines))})))

(defmethod setup-version :world
  [state]
  (->
    state
    (lib/append-ents (rand-neurons {:active? (constantly
                                               false)
                                    :layers 10
                                    :left 90
                                    :per-layer 20
                                    :top 150}))
    (lib/append-ents (->signal {:color (:cyan
                                         controls/color-map)
                                :pos [100 50]
                                :signal-identity :cyan}))
    (assoc-in
      [:on-update-map]
      {:activation-tick
         (lib/every-n-seconds
           3
           (fn [s _]
             (let [who-activates-who-map
                     (into {}
                           (map (fn [{:keys [signal-identity
                                             id]}]
                                  [id
                                   (map :id
                                     (filter
                                       (comp signal-identity
                                             :activated-by)
                                       (lib/entities s)))])
                             (filter :signal-identity
                               (lib/entities s))))
                   active?
                     (into (set (keys
                                  who-activates-who-map))
                           (mapcat identity
                             (vals who-activates-who-map)))
                   activate-entities
                     (fn [s]
                       (-> s
                           (lib/update-ents
                             (fn [e]
                               (if-not (:toggle-active e)
                                 e
                                 (if-not (and
                                           (active? (:id e))
                                           (= (:active? e)
                                              (active?
                                                (:id e))))
                                   ((:toggle-active e)
                                     e
                                     (active? (:id e)))
                                   e))))))
                   make-lines?
                     (zero? (mod (or (:activation-tick s) 0)
                                 2))]
               (->
                 s
                 activate-entities
                 (update-in [:activation-tick] (fnil inc 0))
                 (lib/append-ents
                   (mapcat
                     (fn [[from tos]]
                       (let [from-e ((lib/entities-by-id s)
                                      from)
                             angle-step (/ 360 (count tos))]
                         (mapcat identity
                           (map-indexed
                             (fn [idx to-e]
                               (concat
                                 (elib/->plasma-balls
                                   {:color
                                      (:cyan
                                        controls/color-map)
                                    :duration 1
                                    :from
                                      (lib/position-on-circle
                                        (lib/position
                                          from-e)
                                        (* (-> from-e
                                               :transform
                                               :width)
                                           (-> from-e
                                               :transform
                                               :scale))
                                        (* idx angle-step))
                                    :start-entity from-e
                                    :to (lib/position
                                          to-e)})
                                 (when make-lines?
                                   [(merge
                                      (lib/->connection-line
                                        from-e
                                        to-e)
                                      {:lifetime 1})])))
                             (map (lib/entities-by-id s)
                               tos)))))
                     who-activates-who-map))))))
       :neighbors-lines
         (lib/every-n-seconds 0.5 (->neighbour-lines))})))


(defmethod setup-version :grid
  [state]
  (-> state)
  )

(defn setup
  [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (q/background (lib/->hsb (-> controls
                               :background-color)))
  (let [state {:controls controls :on-update []}]
    (-> state setup-version)))

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
          state ((lib/->call-callbacks :on-click-map) state draggable)
          state ((lib/->call-callbacks :on-drag-start-map) state draggable)]
      (cond-> state
        (double-clicked? old-selection new-selection)
        (on-double-click (:id draggable))))
      state))

(defn mouse-released
  [{:as state :keys [selection]}]
  (if-not selection
    state
    (let [{:as selection :keys [dragged?]}
            ((lib/entities-by-id state) (:id selection))]
      (if-not selection
        state
        (cond-> state
          dragged? ((lib/->call-callbacks :on-drag-end-map)
                     state
                     selection)
          :regardless (update-in
                        [:eid->entity (:id selection)]
                        (fn [e]
                          (assoc e :dragged? false))))))))

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
            (let [controls
                  (merge
                   (controls/default-versions "cell-assemblies")
                   (get-in versions ["cell-assemblies" version])
                   @user-controls/!app)]
              (sketch place opts controls)))]
    (reset! restart-fn f)
    (f)))

(defmethod user-controls/action-button ::restart
  [_]
  (some-> @restart-fn (apply nil)))
