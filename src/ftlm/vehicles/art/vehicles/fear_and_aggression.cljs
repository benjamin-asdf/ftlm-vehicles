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

(defn draw-state
  [state]
  (q/background (lib/->hsb (-> state :controls :background-color)))
  (q/stroke-weight 1)
  (q/stroke 0.3)
  (lib/draw-entities state))

(defn ->arousal-neuron []
  (merge
   (lib/->entity :neuron)
   {:activation 0
    :baseline-arousal (:baseline-arousal (lib/controls))
    :neuron? true
    :hidden? true}))

(defn baseline-arousal [e]
  e
  ;; (if (:baseline-arousal e)
  ;;   (update e :activation + (lib/normal-distr  0.1))
  ;;   e)
  )

(defn update-entity [entity state]
  (let [env (env state)]
    (-> entity
        (lib/update-body state)
        lib/brownian-motion
        lib/friction

        lib/dart-distants-to-middle
        ;; dart-middle-always
        ;; dart-everyboy

        lib/move-dragged

        lib/update-rotation
        lib/update-position

        (lib/update-sensors env)
        baseline-arousal
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
          ;; lib/ray-source-collision-burst
          lib/kill-entities))))

;; (defn ->cart-2-a
;;   [pos scale sinyness]
;;   (let [body (lib/->body pos scale (* (rand) q/TWO-PI) {:h 220 :s 100 :v 100})
;;         sensor-right (lib/->sensor :top-right :rays)
;;         sensor-left (lib/->sensor :top-left :rays)
;;         motor-right (assoc (lib/->motor :bottom-right 0.02) :corner-r 10)
;;         motor-left (assoc (lib/->motor :bottom-left 0.02) :corner-r 10)
;;         arousal-neuron (->arousal-neuron)
;;         cart (lib/->cart
;;               body
;;               [sensor-right sensor-left]
;;               [motor-right motor-left]
;;               {:corner-r 10 :draggable? true :shinyness 5})]
;;     (into
;;       cart
;;       [(lib/->connection sensor-right motor-right)
;;        (lib/->connection sensor-left motor-left) arousal-neuron
;;        (lib/->hidden-connection arousal-neuron motor-left #(* % (rand)))
;;        (lib/->hidden-connection arousal-neuron motor-right #(* % (rand)))])))

;; (defn ->cart-2-c
;;   [pos scale]

;;   ;; (let [sensor-right (lib/->sensor :top-right :rays)
;;   ;;       sensor-left (lib/->sensor :top-left :rays)
;;   ;;       motor-right (assoc (lib/->motor :bottom-right 0.01) :corner-r 10)
;;   ;;       motor-left (assoc (lib/->motor :bottom-left 0.01) :corner-r 10)
;;   ;;       cart (lib/->cart
;;   ;;             (lib/->body pos scale (* (rand) q/TWO-PI) {:h 200 :s 100 :v 100})
;;   ;;             [sensor-right sensor-left]
;;   ;;             [motor-right motor-left]
;;   ;;             {:corner-r 10 :draggable? true})]
;;   ;;   (into cart
;;   ;;         [(lib/->connection sensor-right motor-left)
;;   ;;          (lib/->connection sensor-right motor-right)
;;   ;;          (lib/->connection sensor-left motor-right)
;;   ;;          (lib/->connection sensor-left motor-left)]))
;;   )

(def body-plan
  {:body {:scale 1}
   :components [[:cart/motor :ma
                 {:anchor :bottom-right :corner-r 5 :rotational-power 0.01}]
                [:cart/motor :mb
                 {:anchor :bottom-left :corner-r 5 :rotational-power 0.01}]
                [:cart/sensor :sa {:anchor :top-right :modality :rays}]
                [:cart/sensor :sb {:anchor :top-left :modality :rays}]
                [:brain/connection :_
                 {:entity-a [:ref :sa] :entity-b [:ref :ma] :f :exite}]
                [:brain/connection :_
                 {:entity-a [:ref :sb] :entity-b [:ref :mb] :f :exite}]]})

(def builders
  {:brain/connection (comp lib/->connection #(walk/prewalk-replace {:exite lib/exite} %))
   :cart/body (fn [opts]
                (lib/->body
                 (merge {:color {:h 200 :s 100 :v 100}
                         :corner-r 10
                         :draggable? true
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

(defn resolve-refs [temp-id->ent form]
  (update-vals
   form
   (fn [v]
     (if (ref? v)
       (temp-id->ent (second v) :unresolved)
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

(defn ->cart-2-b
  [pos scale shinyness]
  ;; (let [body (assoc
  ;;              (lib/->body pos scale (* (rand) q/TWO-PI) {:h 0 :s 100 :v 100})
  ;;              :corner-r 20)
  ;;       sensor-right (lib/->sensor :top-right :rays)
  ;;       sensor-left (lib/->sensor :top-left :rays)
  ;;       motor-right (assoc (lib/->motor :bottom-right 0.01) :corner-r 10)
  ;;       motor-left (assoc (lib/->motor :bottom-left 0.01) :corner-r 10)
  ;;       arousal-neuron (->arousal-neuron)
  ;;       cart (lib/->cart body
  ;;                        [sensor-right sensor-left]
  ;;                        [motor-right motor-left]
  ;;                        {:draggable? true :shinyness shinyness})]
  ;;   (into
  ;;     cart
  ;;     [(lib/->connection sensor-right motor-left)
  ;;      (lib/->connection sensor-left motor-right)
  ;;      arousal-neuron
  ;;      (lib/->hidden-connection arousal-neuron motor-left #(* % (rand)))
  ;;      (lib/->hidden-connection arousal-neuron motor-right #(* % (rand)))]))
  (let [c (->cart body-plan)]
    (def the-cart c)
    c))

(defn setup
  [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (q/background (lib/->hsb (-> controls :background-color)))
  (lib/append-ents {:controls controls}
                   (concat
                    ;; (mapcat identity
                    ;;         (repeatedly (/ (:spawn-amount controls) 2)
                    ;;                     #(->cart-2-a
                    ;;                     (lib/rand-on-canvas-gauss 0.3) 1)))
                    (mapcat identity
                            (repeatedly (/ (:spawn-amount controls) 2)
                                        #(->cart-2-b (lib/rand-on-canvas-gauss 0.3)
                                                     1
                                                     (when (:carts-shine? controls)
                                                       (:cart-shinyness controls)))))
                    (mapcat identity
                            (repeatedly (:ray-source-count controls)
                                        #(lib/->ray-source (lib/rand-on-canvas-gauss
                                                            0.8)
                                                           (+ 5 (rand 30))))))))

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
