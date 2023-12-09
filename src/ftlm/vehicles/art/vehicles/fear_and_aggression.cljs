(ns ftlm.vehicles.art.vehicles.fear-and-aggression
  (:require
   [clojure.walk :as walk]
   [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
   [ftlm.vehicles.art :as art]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [ftlm.vehicles.art.controls :as controls :refer [versions]]
   [ftlm.vehicles.art.user-controls :as user-controls]))

(defn env [state]
  {:ray-sources (into [] (filter :ray-source?) (lib/entities state))})

(def body-plans
  {:aggression (fn [{:as opts :keys [baseline-arousal]}]
                 {:body opts
                  :components
                    [[:cart/motor :ma
                      {:anchor :bottom-right
                       :corner-r 5
                       :on-update [(lib/->cap-activation)]
                       :rotational-power 0.02}]
                     [:cart/motor :mb
                      {:anchor :bottom-left
                       :corner-r 5
                       :on-update [(lib/->cap-activation)]
                       :rotational-power 0.02}]
                     [:cart/sensor :sa {:anchor :top-right :modality :rays}]
                     [:cart/sensor :sb {:anchor :top-left :modality :rays}]
                     [:brain/neuron :arousal
                      {:on-update [(lib/->baseline-arousal (or baseline-arousal
                                                               0.8))]}]
                     [:brain/connection :_
                      {:destination [:ref :mb]
                       :f rand
                       :hidden? true
                       :source [:ref :arousal]}]
                     [:brain/connection :_
                      {:destination [:ref :ma]
                       :f rand
                       :hidden? true
                       :source [:ref :arousal]}]
                     [:brain/connection :_
                      {:destination [:ref :ma] :f :excite :source [:ref :sb]}]
                     [:brain/connection :_
                      {:destination [:ref :mb] :f :excite :source [:ref :sa]}]]})
   :explore
     (fn [{:as opts}]
       {:body (assoc opts :color 200)
        :components
          [[:cart/motor :ma
            {:anchor :bottom-right
             :corner-r 5
             :on-update [(lib/->cap-activation)]
             :rotational-power 0.02}]
           [:cart/motor :mb
            {:anchor :bottom-left
             :corner-r 5
             :on-update [(lib/->cap-activation)]
             :rotational-power 0.02}]
           [:cart/sensor :sa {:anchor :top-right :modality :rays}]
           [:cart/sensor :sb {:anchor :top-left :modality :rays}]
           [:brain/neuron :arousal {:on-update [(lib/->baseline-arousal 1)]}]
           [:brain/connection :_
            {:destination [:ref :ma]
             :f :excite
             :hidden? true
             :source [:ref :arousal]}]
           [:brain/connection :_
            {:destination [:ref :mb]
             :f :excite
             :hidden? true
             :source [:ref :arousal]}]
           [:brain/connection :_
            {:destination [:ref :ma] :f (lib/->weighted -1) :source [:ref :sb]}]
           [:brain/connection :_
            {:destination [:ref :mb]
             :f (lib/->weighted -1)
             :source [:ref :sa]}]]})
   :fear (fn [{:as opts :keys [baseline-arousal]}]
           {:body (merge {:color 50} opts)
            :components [[:cart/motor :ma
                          {:anchor :bottom-right
                           :corner-r 5
                           :on-update [(lib/->cap-activation)]
                           :rotational-power 0.03}]
                         [:cart/motor :mb
                          {:anchor :bottom-left
                           :corner-r 5
                           :on-update [(lib/->cap-activation)]
                           :rotational-power 0.03}]
                         [:cart/sensor :sa {:anchor :top-right :modality :rays}]
                         [:cart/sensor :sb {:anchor :top-left :modality :rays}]
                         [:brain/neuron :arousal
                          {:on-update [(lib/->baseline-arousal
                                         (or baseline-arousal 0.6))]}]
                         [:brain/connection :_
                          {:destination [:ref :mb]
                           :f rand
                           :hidden? true
                           :source [:ref :arousal]}]
                         [:brain/connection :_
                          {:destination [:ref :ma]
                           :f rand
                           :hidden? true
                           :source [:ref :arousal]}]
                         [:brain/connection :_
                          {:destination [:ref :ma]
                           :f (lib/->weighted 0.5)
                           :source [:ref :sa]}]
                         [:brain/connection :_
                          {:destination [:ref :mb]
                           :f (lib/->weighted 0.5)
                           :source [:ref :sb]}]]})
   :love (fn [{:as opts}]
           {:body (assoc opts :color 0)
            :components [[:cart/motor :ma
                          {:anchor :bottom-right
                           :corner-r 5
                           :on-update [(lib/->cap-activation)]
                           :rotational-power 0.02}]
                         [:cart/motor :mb
                          {:anchor :bottom-left
                           :corner-r 5
                           :on-update [(lib/->cap-activation)]
                           :rotational-power 0.02}]
                         [:cart/sensor :sa {:anchor :top-right :modality :rays}]
                         [:cart/sensor :sb {:anchor :top-left :modality :rays}]
                         [:brain/neuron :arousal
                          {:on-update [(lib/->baseline-arousal 1)]}]
                         ;; The love cart moves with arousal, but more focused
                         [:brain/connection :_
                          {:destination [:ref :ma]
                           :f :excite
                           :hidden? true
                           :source [:ref :arousal]}]
                         [:brain/connection :_
                          {:destination [:ref :mb]
                           :f :excite
                           :hidden? true
                           :source [:ref :arousal]}]
                         [:brain/connection :_
                          {:destination [:ref :ma]
                           :f (lib/->weighted -0.5)
                           :source [:ref :sa]}]
                         [:brain/connection :_
                          {:destination [:ref :mb]
                           :f (lib/->weighted -0.5)
                           :source [:ref :sb]}]]})})


(def builders
  {:brain/connection
   (comp lib/->connection
         #(walk/prewalk-replace {:excite lib/excite :inhibit lib/inhibit} %))
   :brain/neuron
   lib/->neuron
   :cart/body (fn [opts]
                (lib/->body (merge {:color {:h 200 :s 100 :v 100}
                                    :corner-r 10
                                    :draggable? true
                                    :darts? true
                                    :pos (lib/rand-on-canvas-gauss 0.3)
                                    :rot (* (rand) q/TWO-PI)
                                    :scale 1}
                                   opts)))
   :cart/motor lib/->motor
   :cart/sensor lib/->sensor})

(defmulti build-entity first)

(defmethod build-entity :default [[kind opts]] ((builders kind) opts))

(defn ref? [v] (and (sequential? v) (= (first v) :ref)))

;; only have maps 1 deep right now

(defn resolve-refs
  [temp-id->ent form]
  (update-vals form
               (fn [v]
                 (if (ref? v)
                   (or (temp-id->ent (second v))
                       (throw (js/Error. (str (second v) " is not resolved"))))
                   v))))

(defn ->cart
  [{:keys [body components]}]
  (let [body (build-entity [:cart/body body])
        {:keys [comps]}
        (reduce (fn [{:keys [comps temp-id->ent]} [kind temp-id opts]]
                  (let [entity (build-entity [kind
                                              (resolve-refs temp-id->ent opts)])]
                    {:comps (into comps
                                  (if (map? entity)
                                    [entity]
                                    entity))
                     :temp-id->ent (if (= temp-id :_)
                                     temp-id->ent
                                     (assoc temp-id->ent temp-id entity))}))
                {:comps []
                 :temp-id->ent {}}
                components)]
    (into [(assoc body :components (into [] (map :id) comps))]
          comps)))

(defmethod lib/event! ::spawn
  [{:keys [what]} {:as state :keys [controls]}]
  (def what what)
  (def state state)
  (def controls controls)
  (lib/append-ents state
                   (->cart ((body-plans what) (controls :what)))))

(defn draw-state
  [state]
  (q/background (lib/->hsb (-> state :controls :background-color)))
  (q/stroke-weight 1)
  (q/stroke 0.3)
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

(defn setup
  [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (q/background (lib/->hsb (-> controls :background-color)))
  (let [state {:controls controls
               :on-update (concat
                           (when-not (zero? (controls :ray-sources-spawn-rate))
                             [(lib/every-n-seconds
                               (/ 1 (controls :ray-sources-spawn-rate))
                               (fn [state]
                                 state
                                 (lib/append-ents
                                  state
                                  (lib/->ray-source
                                   {:intensity (+ 5 (rand 30))
                                    :pos (lib/rand-on-canvas-gauss (controls :ray-source-spread))
                                    :scale (controls :ray-source-scale)
                                    :z-index 10}))))]))}]
    (-> state
        (lib/append-ents
         (->>
          #{:fear :aggression :love :explore}
          (sequence
           (comp
            (map (juxt identity controls))
            (mapcat (fn [[kind {:keys [amount] :as opts}]]
                      (repeatedly amount #((body-plans kind) opts))))
            (map ->cart)
            cat))))
        (lib/append-ents (mapcat identity
                                 (repeatedly
                                  ;; (:ray-source-count controls)
                                  0
                                  #(lib/->ray-source
                                    {:intensity (+ 5 (rand 30))
                                     :pos (lib/rand-on-canvas-gauss 0.4)
                                     :scale 0.3
                                     :z-index 10})))))))

(defn on-double-click
  [state id]
  (let [e ((lib/entities-by-id state) id)
        explosion (lib/->explosion {:color (:color e)
                                    :n 20
                                    :pos (lib/position e)
                                    :size 10
                                    :spread 10})]
    (-> state
        (assoc-in [:eid->entity id :lifetime] 0.8)
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

(let [restart-fn (atom nil)]
  (defmethod art/view "fear_and_aggression"
    [{:keys [place version] :as opts}]
    (let
        [f (fn []
             (sketch
              place
              opts
              (merge
               (controls/default-versions "fear_and_aggression")
               (get-in versions ["fear_and_aggression" version])
               @user-controls/!app)))]
        (reset! restart-fn f)
        (f)))
  (defmethod user-controls/action-button ::restart
    [_]
    (some-> @restart-fn (apply nil))))

(defmethod user-controls/action-button ::spawn [_ what]
  (swap! lib/event-queue conj {:kind ::spawn :what what}))
