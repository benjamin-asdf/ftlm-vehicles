(ns ftlm.vehicles.art.pareidolia
  (:require [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
            [ftlm.vehicles.art :as art]
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
            [ftlm.vehicles.assembly-calculus :as ac]
            [ftlm.vehicles.art.neuronal-area :as na]
            [ftlm.vehicles.relay-model :as relay]))

(defn draw-state
  [state]
  (q/background (lib/->hsb (-> state :controls :background-color)))
  (q/stroke-weight 0)
  ;; (q/stroke 0.3)
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
      lib/activation-decay
      lib/activation-shine
      lib/shine
      lib/update-lifetime))

(defn update-state-inner
  [state]
  ;; state
  (let [current-tick (q/millis)
        state (update state
                      :controls
                      merge
                      (user-controls/controls))
        dt (* (:time-speed (lib/controls))
              (/ (- current-tick (:last-tick state))
                 1000.0))
        state (binding [*dt* dt]
                (-> state
                    (assoc :last-tick current-tick)
                    lib/apply-update-events
                    lib/update-update-functions
                    lib/update-state-update-functions
                    (lib/update-ents
                     #(update-entity % state {}))
                    lib/apply-events
                    lib/update-late-update-map
                    lib/transduce-signals
                    lib/track-components
                    lib/track-conn-lines
                    lib/ray-source-collision-burst
                    lib/kill-entities))]
    state))

(defn update-state
  [_]
  (let [state @lib/the-state
        state (update-state-inner state)]
    (reset! lib/the-state state)
    state))

(defmethod lib/setup-version :pareidolia-1
  [state]
  (->
    state
    (lib/append-ents
      [(->
         (lib/->entity
           :circle
           {:color controls/black
            :kinetic-energy 0.1
            :particle? true
            :transform
              (lib/->transform (lib/mid-point) 300 300 1)})
         (lib/live (lib/every-n-seconds
                     (fn [] (+ 0.3 (lib/normal-distr 1 1)))
                     (fn [e s k]
                       (assoc e
                         :kind (rand-nth [:circle :triangle
                                          :rect]))))))])))


(defn setup
  [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (q/background (lib/->hsb (-> controls
                               :background-color)))
  (let [state {:controls controls :on-update []}
        state (-> state lib/setup-version)]
    (reset! lib/the-state state)))


(defn sketch
  [host {:keys [width height]} controls]
  (let [[screen-width screen-height] (lib/window-dimensions)
        _width (cond (= width "max") screen-width
                     width width
                     :else screen-width)
        _height (cond (= height "max") screen-height
                      height height
                      :else screen-height)
        width 1000
        height 800]
    (q/sketch :host host
              :size [width height]
              :setup (partial setup controls)
              :update update-state
              :draw draw-state
              :features [:keep-on-top]
              :middleware [m/fun-mode]
              :mouse-pressed (comp #(reset! lib/the-state %)
                                   lib/mouse-pressed)
              :mouse-released (comp #(reset! lib/the-state
                                             %)
                                    lib/mouse-released)
              :mouse-wheel (comp #(reset! lib/the-state %)
                                 lib/mouse-wheel)
              :frame-rate 30)))

(defonce restart-fn (atom nil))
(defmethod art/view "pareidolia"
  [{:as opts :keys [place version]}]
  (println version)
  (println place)
  (let [f (fn []
            (let [controls
                  (merge
                   (controls/default-versions "pareidolia")
                   (get-in versions ["pareidolia" version])
                   @user-controls/!app)]
              (sketch place opts controls)))]
    (reset! restart-fn f)
    (f)))
