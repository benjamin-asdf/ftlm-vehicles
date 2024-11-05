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
  (q/stroke-weight 0)
  ;; (q/stroke 0.3)
  (if ((q/state) :background-fades?)
    (do (q/color-mode :hsb)
        (lib/draw-entities state)
        (q/color-mode :rgb)
        ;; (q/fill 0 0 0 10)
        #_(q/fill (lib/->hsb
                   (lib/with-alpha
                     (lib/->hsb
                      (or (state :background-color)
                          (-> state
                              :controls
                              :background-color)))
                     0.1)))

        (if
            (=
             (-> state :controls :background-color)
             controls/black)
            (q/fill 0 0 0 10)
            (q/fill 256 256 256 10))

        (q/rect 0 0 (* 2 (q/width)) (* 2 (q/height))))
    (do (q/color-mode :hsb)
        (q/background (lib/->hsb
                        (or (state :background-color)
                            (-> state
                                :controls
                                :background-color))))
        (lib/draw-entities state))))

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
        dt (*
            (or
             (:time-speed state)
             (:time-speed (lib/controls)))
            (/ (- current-tick (:last-tick state))
               1000.0))
        ;; dt (/ dt 5)
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
  [state]
  (update-state-inner state))

(defn dancer
  []
  (->
   (lib/->entity :circle
                 {:dancer? true
                  :color
                  ;; (:mint controls/color-map)
                  controls/black
                  :kinetic-energy 2
                  :no-stroke? true
                  :particle? true
                  :transform
                  (lib/->transform
                   (lib/rand-on-canvas-gauss 0.1)
                   ;; (lib/mid-point)
                   (min (float (/ (q/width) 2)) 300)
                   (min (float (/ (q/width) 2)) 300)
                   1)})
   #_(lib/live (lib/every-now-and-then
                0.1
                (fn [e s k]
                  (update-in e
                             [:transform :scale]
                             (fn [scale]
                               (* scale
                                  (if (< 0.5 (q/random 1))
                                    1.3
                                    0.4)))))))
   (lib/live
    (lib/every-n-seconds
     (fn [] (+ 0.3 (lib/normal-distr 1 1)))
     (fn [e s k]
       (if-not (< 2e3 (q/millis))
         e
         (-> e
             (assoc :kind (rand-nth [:circle :triangle
                                     :rect]))
             (assoc-in [:transform :scale]
                       (rand-nth [1 0.5]))
             (assoc-in [:kinetic-energy]
                       (q/pow 2 (rand-int 3)))
             (update-in [:transform :pos]
                        (if (< 0.5 (rand 1))
                          (constantly (lib/mid-point))
                          identity)))))))

   (lib/live (lib/every-now-and-then
              1
              (fn [e]
                (if-not (< 10e3 (q/millis))
                  e
                  (assoc e
                         :color ((rand-nth [:cyan
                                            :black
                                            :red])
                                 controls/color-map))))))

   #_(lib/live
      (lib/every-now-and-then 10
                              (fn [e]
                                (update-in
                                 e
                                 [:color]
                                 (fn [color]
                                   controls/white
                                   ;; (lib/with-alpha
                                   ;;   (lib/->hsb
                                   ;;   color)
                                   ;;   (*
                                   ;;   (rand-nth
                                   ;;   [0.9
                                   ;;   1.1])
                                   ;;      (q/alpha
                                   ;;      (lib/->hsb
                                   ;;      color))))
                                   )))))
   ;; (lib/live
   ;;  (lib/every-now-and-then
   ;;   0.1
   ;;   (fn [e]
   ;;     (->
   ;;      e
   ;;      (assoc-in [:angular-velocity] 0)
   ;;      (assoc-in
   ;;       [:angular-acceleration]
   ;;       0
   ;;       ;; (fn [acc]
   ;;       ;;   (* (rand-nth [-1 -0.5 0.5 0]) acc))
   ;;       )))))
   ))

(defmethod lib/setup-version :pareidolia-1
  [state]
  (-> state
      (assoc :background-fades? true)
      (lib/live
       (lib/every-now-and-then
        5
        (fn [s k]
          (let [fades? (not (:background-fades? s))]
            (assoc-in s [:background-fades?] fades?)
            (assoc-in s [:time-speed] (if fades? 4 2))))))
      (lib/live (lib/every-now-and-then
                 60
                 (fn [s k]
                   (lib/append-ents s [(dancer)]))))
      (lib/append-ents [(dancer)])))

(defmethod lib/setup-version :pareidolia-2
  [state]
  (-> state
      (assoc :background-fades? true)
      (lib/live
        (lib/every-now-and-then
          5
          (fn [s k]
            (let [fades? (not (:background-fades? s))]
              (assoc-in s [:background-fades?] fades?)
              (assoc-in s [:time-speed] (if fades? 4 2))))))
      (lib/live (lib/every-now-and-then
                  5
                  (fn [s k]
                    (lib/append-ents s [(dancer)]))))
      (lib/append-ents [(dancer)])))



(defmethod lib/setup-version :pareidolia-3
  [state]
  (->
    state
    (assoc :background-fades? true)
    (lib/live
      (lib/every-now-and-then
        5
        (fn [s k]
          (let [fades? (not (:background-fades? s))]
            (assoc-in s [:background-fades?] fades?)
            (assoc-in s [:time-speed] (if fades? 4 2))))))
    (lib/live (lib/every-now-and-then
                20
                (fn [s k] (lib/append-ents s [(dancer)]))))
    (lib/append-ents [(dancer)])
    (lib/live
      (lib/every-now-and-then
        0.1
        (fn [s k]
          (let [e (lib/clone-entity
                    (first (filter :dancer?
                             (shuffle (lib/entities s)))))]
            (lib/append-ents
              s
              [(-> e
                   (assoc-in [:transform :pos]
                             (lib/rand-on-canvas-gauss 0.7))
                   (assoc :lifetime (lib/normal-distr
                                      10
                                      5)))])))))))


(defmethod lib/setup-version :pareidolia-4
  [state]
  (let [dancer (fn []
                 (-> (dancer)
                     (assoc-in [:transform :scale] 0.5)
                     (lib/live (fn [e s k]
                                 (assoc e
                                   :color
                                     controls/white)))))]
    (->
      state
      (assoc :background-fades? true)
      (lib/live
        (lib/every-now-and-then
          5
          (fn [s k]
            (let [fades? (not (:background-fades? s))]
              (assoc-in s [:background-fades?] fades?)
              (assoc-in s [:time-speed] (if fades? 4 2))))))
      (lib/live (lib/every-now-and-then
                  20
                  (fn [s k]
                    (lib/append-ents s [(dancer)]))))
      (lib/append-ents [(dancer)])
      (lib/live
        (lib/every-now-and-then
          2
          (fn [s k]
            (let [e (lib/clone-entity
                      (first (filter :dancer?
                               (shuffle (lib/entities
                                          s)))))]
              (lib/append-ents
                s
                [(-> e
                     (assoc-in [:transform :pos]
                               (lib/rand-on-canvas-gauss
                                 0.7))
                     (assoc :lifetime (lib/normal-distr
                                        10
                                        5)))]))))))))


(defn setup
  [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (q/background (lib/->hsb (-> controls
                               :background-color)))
  (let [state {:controls controls :on-update []}
        state (-> state
                  lib/setup-version)]
    state))

(defn sketch
  [host {:keys [width height]} controls]
  (let [[screen-width screen-height] (lib/window-dimensions)
        width (cond (= width "max") screen-width
                    width width
                    :else screen-width)
        height (cond (= height "max") screen-height
                     height height
                     :else screen-height)]
    (q/sketch :host host
              :size [width height]
              :setup (partial setup controls)
              :update update-state
              :draw draw-state
              :features [:keep-on-top]
              :middleware [m/fun-mode]
              :frame-rate 30)))

;; --------------------
;; I need to update 'lib/the-state' to have multiple of these in the gallery
;; - or put iframes in the gallery !
;; --------------------

(defonce restart-fn (atom (constantly nil)))
(defmethod art/view "pareidolia"
  [{:as opts :keys [place version]}]
  (let [controls (merge
                   (controls/default-versions "pareidolia")
                   (get-in versions ["pareidolia" version])
                   @user-controls/!app)]
    (sketch place opts controls)))
