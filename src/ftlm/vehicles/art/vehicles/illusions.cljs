(ns ftlm.vehicles.art.vehicles.illusions
  (:require
   [clojure.walk :as walk]
   [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
   [ftlm.vehicles.art :as art]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [ftlm.vehicles.art.controls :as controls :refer
    [versions]]
   [ftlm.vehicles.art.user-controls :as
    user-controls]
   [goog.style]))

(defn draw-state
  [state]
  (q/clear)
  (q/background (lib/->hsb (-> state :controls :background-color)))
  (q/stroke-weight 1)
  (q/stroke 0.3)
  (lib/draw-entities state))

(defn update-entity
  [entity state]
  (let [env (lib/env state)]
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

(def the-state (atom {}))

(defn update-state
  [state]
  (def state state)
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
              (lib/update-ents #(update-entity % state))
              lib/transduce-signals
              lib/track-components
              lib/track-conn-lines
              lib/ray-source-collision-burst
              lib/kill-entities))]
    (reset! the-state state)
    state))

;; ----------------------------------------
;; I would call this the green wiper
;; --------------------------------------
;; (defn ->chaser
;;   [pos]
;;   (fn [n-circle circle-opts]
;;     (let [circs
;;           (lib/->clock-circles
;;            pos 250 22
;;            (merge circle-opts {:particle? true :kinetic-energy 0.1}))
;;           chaser
;;           (merge
;;            (lib/->entity :circle)
;;            {:color [0 0 0]
;;             :on-update-map
;;             {:chaser
;;              (let [circs (map :id circs)
;;                    circ-cycle (atom (cycle circs))]
;;                (lib/every-n-seconds
;;                 (/ 1 20)
;;                 (fn [e s _]
;;                   (let [next-inactive
;;                         (do (swap! circ-cycle
;;                                    rest)
;;                             (first @circ-cycle))]
;;                     {:updated-state
;;                      (reduce
;;                       (fn [s eid]
;;                         (assoc-in s
;;                                   [:eid->entity eid
;;                                    :hidden?]
;;                                   (= eid next-inactive)))
;;                       s
;;                       circs)}))))}
;;             :transform (lib/->transform pos 20 20 1)})]
;;       (def thecircs circs)
;;       (into circs [chaser]))))

(defn ->chaser
  [pos]
  (fn [n-circle circle-opts]
    (let [circs (lib/->clock-circles
                 pos
                 250
                 22
                 (merge circle-opts {:no-stroke? true}))
          chaser
            (merge
              (lib/->entity :circle)
              {:color [0 0 0]
               :on-update-map
                 {:chaser
                    (let [circs (map :id circs)
                          circ-cycle (atom (cycle circs))]
                      (lib/every-n-seconds
                        (/ 1 10)
                        (fn [e s _]
                          (let [next-inactive
                                  (do (swap! circ-cycle
                                        rest)
                                      (first @circ-cycle))]
                            {:updated-state
                               (reduce
                                 (fn [s eid]
                                   (assoc-in s
                                     [:eid->entity eid
                                      :hidden?]
                                     (= eid next-inactive)))
                                 s
                                 circs)}))))}
               :transform (lib/->transform pos 20 20 1)})]
      (into circs [chaser]))))

(defn ->chaser-variation
  [pos]
  (fn [n-circle circle-opts]
    (let
      [circs
         (lib/->clock-circles pos 250 22 (merge
         circle-opts {:particle? true
         :kinetic-energy 0.1}))
       ;; (lib/->clock-circles pos 250 22 circle-opts)
       chaser
         (merge
           (lib/->entity :circle)
           {:color [0 0 0]
            :on-update-map
              (let [how-many (atom 1)]
                (let [circs (map :id circs)
                      circ-cycle (atom (cycle circs))]
                  {:chaser
                     (lib/every-n-seconds
                       (/ 1 10)
                       (fn [e s _]
                         (let [next-inactive
                                 (into
                                   #{}
                                   (do (swap! circ-cycle
                                         (fn [xs]
                                           (drop @how-many
                                                 xs)))
                                       (take @how-many
                                             @circ-cycle)))]
                           {:updated-state
                              (reduce (fn [s eid]
                                        (assoc-in s
                                          [:eid->entity eid
                                           :hidden?]
                                          (next-inactive
                                            eid)))
                                s
                                circs)})))
                   :chaser-center-lines
                     (lib/every-n-seconds
                      (/ 1 20)
                       (fn [chaser s _]
                         {:updated-state
                            (->
                              s
                              (lib/append-ents
                               (into
                                []
                                (comp
                                 (filter :hidden?)
                                 (map
                                  (fn [e]
                                    (merge
                                     (lib/->connection-line e chaser)
                                     {:lifetime 0.5}))))
                                (map (s :eid->entity) circs))))}))
                   :chaser-inc (lib/every-n-seconds 5 (fn [_ _ _] (swap! how-many inc) nil))
                   :chaser-soft-reset (lib/every-n-seconds 20 (fn [_ _ _] (swap! how-many (constantly 4)) nil))}))
            :transform (lib/->transform pos 20 20 1)})]
      (into circs [chaser]))))

(defn ->chaser-variation-soft
  [pos]
  (fn [n-circle circle-opts]
    (let
      [circs (lib/->clock-circles pos
                                  250
                                  22
                                  (merge circle-opts
                                         {:kinetic-energy
                                          0.1
                                          :particle? true}))
       ;; (lib/->clock-circles pos 250 22 circle-opts)
       hide-some
       (fn [s circs]
         (reduce
          (fn [s eid] (assoc-in s [:eid->entity eid :hidden?] true))
          s
          circs))
       chaser
       (merge
        (lib/->entity :circle)
        {:color [0 0 0]
         :on-update-map
         (let [how-many (atom 1)]
           (let [circs (map :id circs)
                 circ-cycle (atom (cycle circs))
                 hidden-circs (atom #{})]
             {:chaser
              (lib/every-n-seconds
               (/ 1 10)
               (fn [e s _]
                 (let [next-inactive
                       #{}
                       ;; (into #{} (do (swap! circ-cycle (fn [xs] (drop @how-many xs))) (take @how-many @circ-cycle)))
                       ]
                   (do (swap! circ-cycle (fn [xs] (drop @how-many xs))) (take @how-many @circ-cycle))
                   {:updated-state
                    (reduce
                     (fn [s eid]
                       (assoc-in s
                                 [:eid->entity eid :hidden?]
                                 (or
                                  (@hidden-circs eid)
                                  (next-inactive eid))))
                     s
                     circs)})))
              :chaser-center-lines
              (lib/every-n-seconds
               (/ 1 20)
               (fn [chaser s _]
                 {:updated-state
                  (->
                   s
                   (lib/append-ents
                    (into
                     []
                     (comp
                      (filter :hidden?)
                      (map
                       (fn [e]
                         (merge
                          (lib/->connection-line
                           chaser
                           e)
                          {:lifetime
                           0.1}))))
                     (map (s :eid->entity)
                          circs))))}))
              :chaser-inc (lib/every-n-seconds
                           10
                           (fn [_ _ _]
                             (swap! how-many inc)
                             nil))
              :chaser-soft-reset (lib/every-n-seconds
                                  20
                                  (fn [_ _ _]
                                    (swap! how-many
                                           (constantly 0))
                                    nil))
              :chaser-tag-some
              (lib/every-n-seconds
               1
               (fn [chaser s _]
                 (swap! hidden-circs (constantly (into #{} (take 5 (drop 10 @circ-cycle)))))
                 {:updated-state
                  (hide-some s @hidden-circs)}))}))
         :transform (lib/->transform pos 20 20 1)})]
      (into circs [chaser]))))

(defn ->chaser-variation-spider
  [pos]
  (fn [n-circle circle-opts]
    (let
        [circs (lib/->clock-circles pos 250 22
                                    (merge circle-opts {:kinetic-energy 0.02 :particle? true}))
         ;; (lib/->clock-circles pos 250 22 circle-opts)
         hide-some
         (fn [s circs]
           (reduce
            (fn [s eid] (assoc-in s [:eid->entity eid :hidden?] true))
            s
            circs))
         chaser
         (merge
          (lib/->entity :circle)
          {:color [0 0 0]
           :on-update-map
           (let [how-many (atom 1)]
             (let [circs (map :id circs)
                   circ-cycle (atom (cycle circs))
                   hidden-circs (atom #{})]
               {:chaser
                (lib/every-n-seconds
                 (/ 1 10)
                 (fn [e s _]
                   (let [next-inactive
                         (into #{} (do (swap! circ-cycle (fn [xs] (drop @how-many xs))) (take @how-many @circ-cycle)))
                         ]
                     ;; (do (swap! circ-cycle (fn [xs] (drop @how-many xs))) (take @how-many @circ-cycle))
                     {:updated-state
                      (reduce
                       (fn [s eid]
                         (assoc-in s
                                   [:eid->entity eid :hidden?]
                                   (or
                                    (@hidden-circs eid)
                                    (next-inactive eid))))
                       s
                       circs)})))
                :chaser-center-lines
                (lib/every-n-seconds
                 (/ 1 20)
                 (fn [chaser s _]
                   {:updated-state
                    (->
                     s
                     (lib/append-ents
                      (into
                       []
                       (comp
                        (filter :hidden?)
                        (map
                         (fn [e]
                           (merge
                            (lib/->connection-line
                             chaser
                             e)
                            {:lifetime 1}))))
                       (map (s :eid->entity)
                            circs))))}))
                :chaser-inc (lib/every-n-seconds
                             10
                             (fn [_ _ _]
                               (swap! how-many inc)
                               nil))
                :chaser-soft-reset (lib/every-n-seconds
                                    20
                                    (fn [_ _ _]
                                      (swap! how-many
                                             (constantly 0))
                                      nil))
                :chaser-tag-some
                (lib/every-n-seconds
                 1
                 (fn [chaser s _]
                   (swap!
                    hidden-circs
                    (constantly
                     (into #{} (take 4 (drop 10 @circ-cycle)))))
                   (swap!
                    hidden-circs
                    (fn [c]
                      (into c (take (lib/normal-distr 2 1) (shuffle circs)))))
                   {:updated-state
                    (hide-some s @hidden-circs)}))}))
           :transform (lib/->transform pos 20 20 1)})]
      (into circs [chaser]))))

(defn setup
  [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (q/background (lib/->hsb (-> controls
                               :background-color)))
  (let [state {:controls controls :on-update []}]
    (-> state
        (lib/append-ents
         (;; (->chaser-variation-spider (lib/mid-point))
          (->chaser (lib/mid-point))
            12
            {:color (:heliotrope controls/color-map)
             ;; (rand-nth (into [] (vals
             ;; controls/color-map)))
             :transform (lib/->transform [0 0] 40 40 1.5)}))

        ;; ((->chaser (lib/mid-point))
        ;;  12
        ;;  {:color (:heliotrope controls/color-map)
        ;;   ;; (rand-nth (into [] (vals
        ;;   controls/color-map)))
        ;;   :transform
        ;;   (lib/->transform [0 0] 40 40 1.5)})
    )))

(defn on-double-click
  [state id]
  (let [e ((lib/entities-by-id state) id)
        explosion (lib/->explosion {:color (:color e)
                                    :n 20
                                    :pos (lib/position e)
                                    :size 10
                                    :spread 10})]
    (-> state
        (assoc-in [:eid->entity id :lifetime] 0.6)
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
              :mouse-pressed mouse-pressed
              :mouse-released mouse-released
              :mouse-wheel mouse-wheel
              :frame-rate 30)))


(defonce restart-fn (atom nil))
(defmethod art/view "illusions"
  [{:as opts :keys [place version]}]
  (let [f (fn []
            (let [controls (merge (controls/default-versions "illusions")
                                  (get-in versions ["illusions" version])
                                  @user-controls/!app)]
              (sketch place opts controls)))]
    (reset! restart-fn f)
    (f)))

(defmethod user-controls/action-button ::restart
  [_]
  (some-> @restart-fn (apply nil)))
