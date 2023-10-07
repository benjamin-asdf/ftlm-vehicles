(ns ftlm.vehicles.art.vehicles.fear-and-aggression
  (:require
   [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
   [ftlm.vehicles.art :as art]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [ftlm.vehicles.art.controls :refer [versions]]
   [ftlm.vehicles.art.user-controls :as user-controls]
   [ftlm.vehicles.art.controls :as controls]))

(def default-controls {:time-speed 1})

(defn env [state]
  {:ray-sources (into [] (filter :ray-source?) (lib/entities state))})

(defn draw-state
  [state]
  (q/background 230
                ;; (lib/->hsb 0 ;; (-> state :controls :background-color)
                ;;            )
                )
  (q/stroke-weight 1)
  (q/stroke 0.3)
  (lib/draw-entities state))

(defn update-entity [entity state]
  (let [env (env state)]
    (-> entity
        (lib/update-body state)
        lib/friction

        ;; dart-distants-to-middle
        ;; dart-middle-always
        ;; dart-everyboy
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
          ;; transduce-signals
          lib/track-components
          ;; track-conn-lines
          ;; make-trails
          lib/kill-entities))))

(defn setup
  [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (lib/append-ents
   {:controls controls}
   (concat
    (lib/->cart
     (lib/->body
      (lib/rand-on-canvas-gauss 0.1)
      1
      q/HALF-PI

      {:h 100 :s 100 :v 100})
     [(lib/->sensor :top-right :rays)
      ;; (lib/->sensor :top-left :rays)
      ]
     []
     {:shinyness 0.1 :draggable? true })
    (lib/->ray-source [200 200] 1))))


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
