(ns ftlm.vehicles.art.vehicles.fear-and-aggression
  (:require
   [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
   [ftlm.vehicles.art :as art]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [ftlm.vehicles.art.controls :refer [versions]]
   [ftlm.vehicles.art.user-controls :as user-controls]
   [ftlm.vehicles.art.controls :as controls]))

(def default-controls
  {:baseline-arousal 0.4
   :brownian-factor 1
   :cart-spawn-amount 10
   :ray-source-count 20
   :time-speed 5})

(defn env [state]
  {:ray-sources (into [] (filter :ray-source?) (lib/entities state))})

(defn draw-state
  [state]
  (q/background 0
                ;; (lib/->hsb 0 ;; (-> state :controls :background-color)
                ;;            )
                )
  (q/stroke-weight 1)
  (q/stroke 0.3)
  (lib/draw-entities state))

(defn ->arousal-neuron []
  (merge
   (lib/->entity :neuron)
   {:activation 0
    :baseline-arousal-neuron? true
    :neuron? true
    :hidden? true}))

(defn baseline-arousal [e]
  (if (:baseline-arousal-neuron? e)
    (update e :activation + (lib/normal-distr (:baseline-arousal (lib/controls)) 0.1))
    e))

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
          ;; make-trails
          lib/kill-entities))))

(defn ->cart-2-a
  [pos scale]
  (let [body (lib/->body pos scale (* (rand) q/TWO-PI) {:h 220 :s 100 :v 100})
        sensor-right (lib/->sensor :top-right :rays)
        sensor-left (lib/->sensor :top-left :rays)
        motor-right (assoc (lib/->motor :bottom-right 0.02) :corner-r 10)
        motor-left (assoc (lib/->motor :bottom-left 0.02) :corner-r 10)
        arousal-neuron (->arousal-neuron)
        cart (lib/->cart
              body
              [sensor-right sensor-left]
              [motor-right motor-left]
              {:corner-r 10 :draggable? true :shinyness 10})]
    (into
      cart
      [(lib/->connection sensor-right motor-right)
       (lib/->connection sensor-left motor-left) arousal-neuron
       (lib/->hidden-connection arousal-neuron motor-left #(* % (rand)))
       (lib/->hidden-connection arousal-neuron motor-right #(* % (rand)))])))

(defn ->cart-2-b
  [pos scale]
  (let [body (assoc
               (lib/->body pos scale (* (rand) q/TWO-PI) {:h 0 :s 100 :v 100})
               :corner-r 20)
        sensor-right (lib/->sensor :top-right :rays)
        sensor-left (lib/->sensor :top-left :rays)
        motor-right (assoc (lib/->motor :bottom-right 0.01) :corner-r 10)
        motor-left (assoc (lib/->motor :bottom-left 0.01) :corner-r 10)
        arousal-neuron (->arousal-neuron)
        cart (lib/->cart body
                         [sensor-right sensor-left]
                         [motor-right motor-left]
                         {:draggable? true :shinyness 100})]
    (into
      cart
      [(lib/->connection sensor-right motor-left)
       (lib/->connection sensor-left motor-right) arousal-neuron
       (lib/->hidden-connection arousal-neuron motor-left #(* % (rand)))
       (lib/->hidden-connection arousal-neuron motor-right #(* % (rand)))])))

(defn ->cart-2-c
  [pos scale]
  (let [sensor-right (lib/->sensor :top-right :rays)
        sensor-left (lib/->sensor :top-left :rays)
        motor-right (assoc (lib/->motor :bottom-right 0.01) :corner-r 10)
        motor-left (assoc (lib/->motor :bottom-left 0.01) :corner-r 10)
        cart (lib/->cart
               (lib/->body pos scale (* (rand) q/TWO-PI) {:h 200 :s 100 :v 100})
               [sensor-right sensor-left]
               [motor-right motor-left]
               {:corner-r 10 :draggable? true})]
    (into cart
          [(lib/->connection sensor-right motor-left)
           (lib/->connection sensor-right motor-right)
           (lib/->connection sensor-left motor-right)
           (lib/->connection sensor-left motor-left)])))


;; {:motors
;;  [{:id :ma :anchor :bottom-right} {:id :mb :anchor :bottom-left}]
;;  :connections [[0 1] [1 1]]}

(defn setup
  [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (lib/append-ents
    {:controls controls}
    (concat
     (doall
      (mapcat
       identity
       (repeatedly (/ (:cart-spawn-amount controls) 2)
                   #(->cart-2-a (lib/rand-on-canvas-gauss 0.3) 1))))
     (doall
      (mapcat
       identity
       (repeatedly (/ (:cart-spawn-amount controls) 2)
                   #(->cart-2-b (lib/rand-on-canvas-gauss 0.3) 1))))

      ;; (->cart-2-a (lib/rand-on-canvas-gauss 0.1) 1)
      (mapcat identity
        (repeatedly (:ray-source-count controls)
                    #(lib/->ray-source
                      (lib/rand-on-canvas-gauss 0.8)
                      (+ 5 (rand 30))))))))

(defn mouse-pressed
  [state]
  (let [draggable (lib/find-closest-draggable state)]
    (-> state
        (assoc :pressed true)
        (assoc-in [:eid->entity (:id draggable) :dragged?] true))))

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
               default-controls
               ;; (controls/default-versions "fear_and_aggression")
               ;; (get-in versions ["fear_and_aggression" version])
               ;; @user-controls/!app
               )))]
        (reset! restart-fn f)
        (f)))
  (defmethod user-controls/action-button ::restart
    [_]
    (some-> @restart-fn (apply nil))))
