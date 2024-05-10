(ns ftlm.vehicles.art.vehicles.cerebellum
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
   [ftlm.vehicles.art.grid]
   [goog.style]
   [ftlm.vehicles.hdv]
   [tech.v3.datatype.argops :as argops]
   [tech.v3.datatype.functional :as dtype-fn]
   ["mathjs" :as mathjs]))

;;
;; https://www.sciencedirect.com/science/article/abs/pii/S0079612308609711?via%3Dihub

;;
;; Based on Braitenberg about the arrangment of Cerebellum.
;;
;;

(defn env [_state] {})

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
                    lib/apply-events
                    ;; (lib/apply-events (:event-q state))
                    (lib/update-ents
                     #(update-entity % state (env state)))
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

(defn ->fish
  []
  (->
    (lib/->entity
      :fish
      {:color (:woodsmoke controls/color-map)
       :draggable? true
       :draw-functions
         {:1 (fn [e]
               (q/stroke-weight (:stroke-weight e))
               (q/with-translation
                 (lib/position e)
                 (q/with-fill
                   (lib/->hsb (:color e))
                   (q/with-rotation
                     [(lib/rotation e)]
                     (let [{:keys [scale width height]}
                             (:transform e)
                           [w h] [(* scale width)
                                  (* scale height)]
                           [x1 y1] [0 (- (/ h 2))]
                           [x2 y2] [(- (/ w 2)) (+ (/ h 2))]
                           [x3 y3] [(+ (/ w 2))
                                    (+ (/ h 2))]]
                       (q/with-translation
                         [15 0]
                         (q/with-rotation
                           [q/HALF-PI]
                           (q/triangle x1 y1 x2 y2 x3 y3)))
                       (q/with-translation
                         [-15 0]
                         (q/with-rotation
                           [(- q/HALF-PI)]
                           (q/triangle x1 y1 x2 y2 x3 y3)))
                       (q/with-translation
                         [0 -40]
                         (q/with-rotation
                           [q/TWO-PI]
                           (q/triangle x1 y1 x2 y2 x3 y3))))
                     (q/stroke-weight (:stroke-weight e))
                     (q/with-stroke (:stroke e)
                                    (q/ellipse
                                      0
                                      0
                                      (-> e
                                          :transform
                                          :width)
                                      (-> e
                                          :transform
                                          :height)))))))}
       :stroke (lib/->hsb (:orange controls/color-map))
       :stroke-weight 4
       :transform
         (lib/->transform (lib/mid-point) 80 100 1)})
    (lib/live [:fade (lib/->fade-pulse-2 10 :stroke)])
    (lib/live [:water
               (lib/every-n-seconds
                 1.5
                 (fn [e]
                   (update e
                           :angular-acceleration
                           (fnil + 0)
                           (lib/normal-distr
                             0
                             ;; 0.1
                             (get (lib/controls)
                                  :water-force
                                  0.5)))))])))

(defmethod lib/setup-version :cerebellum1
  [state]
  (-> state
      (lib/append-ents
       [
        (->fish)
        ;; (lib/->entity :circle
        ;;               {:transform (lib/->transform [200 200] 20 20 1)
        ;;                :color controls/white})
        ])))

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
        _width (cond (= width "max") screen-width width width :else screen-width)
        _height (cond (= height "max") screen-height height height :else screen-height)
        width 1000
        height 800]
    (q/sketch :host host
              :size [width height]
              :setup (partial setup controls)
              :update update-state
              :draw draw-state
              :features [:keep-on-top]
              :middleware [m/fun-mode]
              :mouse-pressed (comp #(reset! lib/the-state %) lib/mouse-pressed)
              :mouse-released (comp #(reset! lib/the-state %) lib/mouse-released)
              :mouse-wheel (comp #(reset! lib/the-state %) lib/mouse-wheel)
              :frame-rate 30)))

(defonce restart-fn (atom nil))
(defmethod art/view "cerebellum"
  [{:as opts :keys [place version]}]
  (let [f (fn []
            (let [controls
                  (merge
                   (controls/default-versions "cerebellum")
                   (get-in versions ["cerebellum" version])
                   @user-controls/!app)]
              (sketch place opts controls)))]
    (reset! restart-fn f)
    (f)))

(defmethod user-controls/action-button ::restart
  [_]
  (some-> @restart-fn (apply nil)))
