(ns ftlm.vehicles.art.vehicles.fear-and-aggression
  (:require
   [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
   [ftlm.vehicles.art :as art]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [ftlm.vehicles.art.controls :refer [versions]]
   [ftlm.vehicles.art.user-controls :as user-controls]
   [ftlm.vehicles.art.controls :as controls]
   [ftlm.vehicles.art.vehicles.carts :as c]))

(defn draw-state
  [state]
  (q/background 230
                ;; (lib/->hsb 0 ;; (-> state :controls :background-color)
                ;;            )
                )
  (q/stroke-weight 1)
  (q/stroke 0.3)
  (lib/draw-entities state))

(defn update-state
  [state]
  (let [current-tick (q/millis)
        state (update state :controls merge @user-controls/!app)
        dt (* (:time-speed (lib/controls))
              (/ (- current-tick (:last-tick state)) 1000.0))]
    (binding [*dt* dt]
      (-> state
          (assoc :last-tick current-tick)
          ;; (lib/update-ents #(update-entity % state))
          ;; transduce-signals
          lib/track-components
          ;; track-conn-lines
          ;; make-trails
          lib/kill-entities))))

(defn setup [controls]
  (lib/append-ents
   {:controls controls}
   (c/->cart
    (c/->body
     (lib/rand-on-canvas-gauss 0.1)
     1
     0
     {:h 255 :s 100 :v 100})
    [(lib/->sensor :top-middle :rays)
    ;; (lib/->sensor :top-right :rays)
     ]
    []
    {:shinyness 0.1})))

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
              ;; :mouse-pressed mouse-pressed
              ;; :mouse-released mouse-released
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
