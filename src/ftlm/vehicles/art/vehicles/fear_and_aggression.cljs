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

(defn base
  [{:keys [scale baseline-arousal]}]
  {:body {:scale scale :z-index -1}
   :components
   [[:cart/motor :ma
     {:anchor :bottom-right :corner-r 5 :rotational-power 0.02}]
    [:cart/motor :mb
     {:anchor :bottom-left :corner-r 5 :rotational-power 0.02}]
    [:cart/sensor :sa {:anchor :top-right :modality :rays}]
    [:cart/sensor :sb {:anchor :top-left :modality :rays}]
    [:brain/neuron :arousal {:on-update [(lib/->baseline-arousal (or baseline-arousal 0.8))]}]
    [:brain/connection :_
     {:destination [:ref :mb] :f rand :hidden? true :source [:ref :arousal]}]
    [:brain/connection :_
     {:destination [:ref :ma]
      :f rand
      :hidden? true
      :source [:ref :arousal]}]]})

(defn ->cart-2-a
  [opts]
  (update (base opts)
          :components
          #(into %
                 [[:brain/connection :_
                   {:destination [:ref :ma] :f :exite :source [:ref :sa]}]
                  [:brain/connection :_
                   {:destination [:ref :mb] :f :exite :source [:ref :sb]}]])))

(defn ->cart-2-b
  [opts]
  (update (base opts)
          :components
          #(into %
                 [[:brain/connection :_ {:destination [:ref :ma] :f :exite :source [:ref :sb]}]
                  [:brain/connection :_ {:destination [:ref :mb] :f :exite :source [:ref :sa]}]])))


(defn ->love
  [{:keys [scale]}]
  {:body {:scale scale :z-index -1}
   :components
     [[:cart/motor :ma
       {:anchor :bottom-right :corner-r 5 :rotational-power 0.02
        :on-update [(lib/->cap-activation)]}]
      [:cart/motor :mb
       {:anchor :bottom-left :corner-r 5 :rotational-power 0.02
        :on-update [(lib/->cap-activation)]}]
      [:cart/sensor :sa {:anchor :top-right :modality :rays}]
      [:cart/sensor :sb {:anchor :top-left :modality :rays}]
      ;; You don't get love if you are too fidgety... huh
      [:brain/neuron :arousal {:on-update [(lib/->baseline-arousal 1)]}]
      ;; The love cart moves with arousal, but more focused
      [:brain/connection :_ {:destination [:ref :ma] :f :exite :hidden? true :source [:ref :arousal]}]
      [:brain/connection :_ {:destination [:ref :mb] :f :exite :hidden? true :source [:ref :arousal]}]
      [:brain/connection :_
       {:destination [:ref :ma] :f (lib/->weighted -1.2) :source [:ref :sa]}]
      [:brain/connection :_
       {:destination [:ref :mb] :f (lib/->weighted -1.2) :source [:ref :sb]}]]})

;; (defn ->love
;;   [opts]
;;   (update (base (assoc opts :baseline-arousal 2))
;;           :components
;;           #(into %
;;                  [[:brain/connection :_ {:destination [:ref :ma] :f :inhibit :source [:ref :sa]}]
;;                   [:brain/connection :_ {:destination [:ref :mb] :f :inhibit :source [:ref :sb]}]])))


(def builders
  {:brain/connection
   (comp lib/->connection
         #(walk/prewalk-replace {:exite lib/exite :inhibit lib/inhibit} %))
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

(defn draw-state
  [state]
  (q/background (lib/->hsb (-> state :controls :background-color)))
  (q/stroke-weight 1)
  (q/stroke 0.3)
  (lib/draw-entities state))

(defn update-entity [entity state]
  (let [env (env state)]
    (-> entity
        (lib/update-update-functions state)
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
        state (update state :controls merge @user-controls/!app)
        dt (* (:time-speed (lib/controls))
              (/ (- current-tick (:last-tick state)) 1000.0))]
    (binding [*dt* dt]
      (-> state
          (assoc :last-tick current-tick)
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
  (q/background (lib/->hsb (-> controls
                               :background-color)))
  (lib/append-ents
    {:controls controls}
    (concat

     (mapcat identity
             (repeatedly (/ (:spawn-amount controls) 2)
                         (comp ->cart
                               #(->cart-2-b {:scale 1
                                             :shine (when (:carts-shine? controls)
                                                      (:cart-shinyness controls))}))))

     ;; (mapcat identity
     ;;         (repeatedly (:covards-spawn controls)
     ;;                     (comp ->cart
     ;;                           #(->cart-2-a {:scale 1
     ;;                                         :shine (when (:carts-shine? controls)
     ;;                                                  (:cart-shinyness controls))}))))


     ;; (mapcat identity
     ;;         (repeatedly (/ (:spawn-amount controls) 2)
     ;;                     (comp ->cart
     ;;                           #(->love
     ;;                             {:scale 1
     ;;                              :shine (when (:carts-shine? controls)
     ;;                                       (:cart-shinyness controls))}))))


     (mapcat identity
             (repeatedly (:ray-source-count controls)
                         #(lib/->ray-source
                          {:pos
                           (lib/rand-on-canvas-gauss 0.8)
                           :z-index 10
                           :intensity
                           (+ 5 (rand 30))}))))))

(defn mouse-pressed
  [state]
  (if-let
      [draggable (lib/find-closest-draggable state)]
    (-> state
          (assoc :pressed true)
          (assoc-in [:eid->entity (:id draggable) :dragged?] true))
      state))

(defn mouse-released
  [state]
  (-> state
      (assoc :pressed false)
      (lib/update-ents (fn [e] (dissoc e :dragged?)))))

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
               @user-controls/!app
               )))]
        (reset! restart-fn f)
        (f)))
  (defmethod user-controls/action-button ::restart
    [_]
    (some-> @restart-fn (apply nil))))
