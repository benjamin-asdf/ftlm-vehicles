(ns ftlm.vehicles.art.not-fire
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
  (q/color-mode :hsb)
  (if
      (and
       ((q/state) :background-fades?)
       ;; (< 1 (lib/normal-distr 1 1))
       )
      (do (lib/draw-entities state)
          (q/color-mode :rgb)
          (if (:background-white? (:controls (q/state)))
            (q/fill 256 256 256 5)
            (q/fill 0 0 0 5))
          (q/rect 0 0 (* 2 (q/width)) (* 2 (q/height))))
      (do
        (q/background (lib/->hsb (or (state :background-color)
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

(defn ball
  [pos]
  (lib/->entity :circle
                {:color (rand-nth [:black :white])
                 :ball? true
                 :kinetic-energy 1
                 :lifetime 10
                 :no-stroke? true
                 :particle? true
                 :transform (lib/->transform pos 20 20 1)}))

(defmethod lib/setup-version :not-fire-0
  [state]
  (-> state
      (assoc :background-fades? true)
      (lib/live
        (lib/every-now-and-then
          (/ 1
             (-> state
                 :controls
                 (:ball-freq 1)))
          (fn [s k]
            (lib/append-ents
              s
              [(-> (assoc (ball (lib/rand-on-canvas-gauss
                                  (-> s
                                      :controls
                                      :ball-spread)))
                            :lifetime
                          (lib/normal-distr 10 2) :kinetic-energy
                          2 :z-index
                          100 :color
                          (rand-nth [:cyan :hit-pink :heliotrope
                                     :mint]))
                   (assoc-in [:transform :scale]
                             ;; (lib/normal-distr 0.5 0.2)
                             (lib/normal-distr
                               (:ball-scale-base (lib/controls))
                               (:ball-scale-stdv (lib/controls))))
                   (lib/live (lib/every-now-and-then
                               2
                               (fn [e s k]
                                 (if (< (count (lib/entities s)) 1000)
                                   {:updated-state
                                      (lib/append-ents
                                        s
                                        [(-> (lib/clone-entity e)
                                             (update :lifetime * 1.02)
                                             ;; (update-in
                                             ;; [:transform
                                             ;; :scale] *
                                             ;; (rand-nth [0.6
                                             ;; 1.1]))
                                             (update
                                               :kinetic-energy
                                               *
                                               (:ball-spread-acc
                                                 (lib/controls))))])}
                                   e)))))]))))
      (lib/live
        (lib/every-n-seconds
          (fn []
            (lib/normal-distr
              (/ 1 (:line-freq (:controls (q/state)) 1))
              (q/sqrt (/ 1 (:line-freq (:controls (q/state)) 1)))))
          (fn [s k]
            (let [balls (shuffle (filter (comp #{:circle} :kind)
                                   (lib/entities s)))
                  b1 (first balls)
                  b2 (second balls)]
              (when (and b1 b2)
                (lib/append-ents s
                                 [(lib/->connection-line b1 b2)]))))))
      (lib/live
        (lib/every-now-and-then
          1
          (fn [s k]
            (let [balls (shuffle (filter (comp #{:circle} :kind)
                                   (lib/entities s)))
                  b1 (first balls)
                  b2 (second balls)]
              (when (and b1 b2)
                (-> s
                    (update-in [:eid->entity (:id b1) :kinetic-energy]
                               *
                               1.2)
                    (update-in [:eid->entity (:id b2) :kinetic-energy]
                               *
                               1.2)))))))))


(defmethod lib/setup-version :not-fire-1
  [state]
  (-> state
      (assoc :background-fades? true)
      (lib/live
        (lib/every-now-and-then
          (/ 1
             (-> state
                 :controls
                 (:ball-freq 1)))
          (fn [s k]
            (lib/append-ents
              s
              [(-> (assoc (ball (lib/rand-on-canvas-gauss
                                  (-> s
                                      :controls
                                      :ball-spread)))
                            :lifetime
                          (lib/normal-distr 10 2) :kinetic-energy
                          2 :z-index
                          100 :color
                          (rand-nth [:cyan :hit-pink :heliotrope
                                     :mint]))
                   (assoc-in [:transform :scale]
                             ;; (lib/normal-distr 0.5 0.2)
                             (lib/normal-distr 1
                                               0.2
                                               ;; (:ball-scale-base
                                               ;; (lib/controls))
                                               ;; (:ball-scale-stdv
                                               ;; (lib/controls))
                             ))
                   (lib/live
                     (lib/every-now-and-then
                       2
                       (fn [e s k]
                         (if (< (count (lib/entities s)) 1000)
                           {:updated-state
                              (lib/append-ents
                                s
                                [(-> (lib/clone-entity e)
                                     (update :lifetime * 1.02)
                                     (update-in [:transform :scale]
                                                *
                                                (rand-nth [0.6 1.1]))
                                     (update :kinetic-energy
                                             *
                                             (:ball-spread-acc
                                               (lib/controls))))])}
                           e)))))]))))
      (lib/live
        (lib/every-n-seconds
          (fn []
            (lib/normal-distr
              (/ 1 (:line-freq (:controls (q/state)) 1))
              (q/sqrt (/ 1 (:line-freq (:controls (q/state)) 1)))))
          (fn [s k]
            (let [balls (shuffle (filter (comp #{:circle} :kind)
                                   (lib/entities s)))
                  b1 (first balls)
                  b2 (second balls)]
              (when (and b1 b2)
                (lib/append-ents s
                                 [(lib/->connection-line b1 b2)]))))))
      (lib/live
        (lib/every-now-and-then
          1
          (fn [s k]
            (let [balls (shuffle (filter (comp #{:circle} :kind)
                                   (lib/entities s)))
                  b1 (first balls)
                  b2 (second balls)]
              (when (and b1 b2)
                (-> s
                    (update-in [:eid->entity (:id b1) :kinetic-energy]
                               *
                               1.2)
                    (update-in [:eid->entity (:id b2) :kinetic-energy]
                               *
                               1.2)))))))))

(defmethod lib/setup-version :not-fire-2
  [state]
  (-> state
      (assoc :background-fades? true)
      (lib/live
       (lib/every-now-and-then
        (/ 1
           (-> state
               :controls
               (:ball-freq 1)))
        (fn [s k]
          (lib/append-ents
           s
           [(let [orig-col

                  (rand-nth [:cyan :hit-pink :heliotrope :mint
                             :black
                             :black
                             :black
                             :black
                             :black
                             :black])]
              (-> (assoc
                   (ball
                    (lib/rand-on-canvas-gauss
                     (-> s
                         :controls
                         :ball-spread)))
                   :immune? (rand-nth [false false false false false false true])
                   :orig-color orig-col
                   :lifetime
                   (lib/normal-distr 10 2) :kinetic-energy
                   2 :z-index
                   100
                   :color orig-col)
                  (assoc-in [:transform :scale]
                            ;; (lib/normal-distr 0.5 0.2)
                            (lib/normal-distr 1
                                              0.2
                                              ;; (:ball-scale-base
                                              ;; (lib/controls))
                                              ;; (:ball-scale-stdv
                                              ;; (lib/controls))
                                              ))
                  (lib/live
                   (lib/every-now-and-then
                    2
                    (fn [e s k]
                      (if (< (count (lib/entities s)) 1000)
                        {:updated-state
                         (lib/append-ents
                          s
                          [(-> (lib/clone-entity e)
                               (update :lifetime * 1.02)
                               (update-in [:transform :scale]
                                          *
                                          (rand-nth [0.6 1.1]))
                               (update :kinetic-energy
                                       *
                                       (:ball-spread-acc
                                        (lib/controls))))])}
                        e))))))]))))
      (lib/live
       (lib/every-n-seconds
        (fn []
          (lib/normal-distr
           (/ 1 (:line-freq (:controls (q/state)) 1))
           (q/sqrt (/ 1 (:line-freq (:controls (q/state)) 1)))))
        (fn [s k]
          (let [balls (shuffle (filter (comp #{:circle} :kind)
                                       (lib/entities s)))
                b1 (first balls)
                b2 (second balls)]
            (when (and b1 b2)
              (lib/append-ents s
                               [(lib/->connection-line b1 b2)]))))))
      (lib/live
       (lib/every-now-and-then
        1
        (fn [s k]
          (let [balls (shuffle (filter (comp #{:circle} :kind)
                                       (lib/entities s)))
                b1 (first balls)
                b2 (second balls)]
            (when (and b1 b2)
              (-> s
                  (update-in [:eid->entity (:id b1) :kinetic-energy]
                             *
                             1.2)
                  (update-in [:eid->entity (:id b2) :kinetic-energy]
                             *
                             1.2)))))))



      #_(lib/live
       (lib/every-n-seconds
        0.2
        (fn [s k]
          (lib/update-ents
           s
           (fn [e]
             (update
              e
              :color
              #(get {:black :white :white :black} % :black)))))))


      (lib/live
       (lib/every-now-and-then
        0.1
        ;; (fn [s k]
        ;;   (let [col (rand-nth [:black :white])]
        ;;     (lib/update-ents
        ;;      s
        ;;      (fn [e]
        ;;        (assoc e :color col)))))
        (fn [s k]
          (let [col (rand-nth [:black :white])]
            (lib/update-ents
             s
             (fn [e]
               (update
                e
                :color
                #(get {:white (:orig-color e)} % :white))))))))))


(defmethod lib/setup-version :not-fire-3
  [state]
  (-> state
      (assoc :background-fades? true)
      (lib/live
       (lib/every-now-and-then
        (/ 1
           (-> state
               :controls
               (:ball-freq 1)))
        (fn [s k]
          (lib/append-ents
           s
           [(let [orig-col

                  (rand-nth [:cyan :hit-pink :heliotrope :mint
                             :black
                             :black
                             :black
                             :black
                             :black
                             :black])]
              (-> (assoc
                   (ball
                    (lib/rand-on-canvas-gauss
                     (-> s
                         :controls
                         :ball-spread)))
                   :immune? (rand-nth [false false false false false false true])
                   :orig-color orig-col
                   :lifetime
                   (lib/normal-distr 10 2) :kinetic-energy
                   2 :z-index
                   100
                   :color orig-col)
                  (assoc-in [:transform :scale]
                            ;; (lib/normal-distr 0.5 0.2)
                            (lib/normal-distr 1
                                              0.2
                                              ;; (:ball-scale-base
                                              ;; (lib/controls))
                                              ;; (:ball-scale-stdv
                                              ;; (lib/controls))
                                              ))
                  (lib/live
                   (lib/every-now-and-then
                    2
                    (fn [e s k]
                      (if (< (count (lib/entities s)) 1000)
                        {:updated-state
                         (lib/append-ents
                          s
                          [(-> (lib/clone-entity e)
                               (update :lifetime * 1.02)
                               (update-in [:transform :scale]
                                          *
                                          (rand-nth [0.6 1.1]))
                               (update :kinetic-energy
                                       *
                                       (:ball-spread-acc
                                        (lib/controls))))])}
                        e))))))]))))
      (lib/live
       (lib/every-n-seconds
        (fn []
          (lib/normal-distr
           (/ 1 (:line-freq (:controls (q/state)) 1))
           (q/sqrt (/ 1 (:line-freq (:controls (q/state)) 1)))))
        (fn [s k]
          (let [balls (shuffle (filter (comp #{:circle} :kind)
                                       (lib/entities s)))
                b1 (first balls)
                b2 (second balls)]
            (when (and b1 b2)
              (lib/append-ents s
                               [(lib/->connection-line b1 b2)]))))))
      (lib/live
       (lib/every-now-and-then
        1
        (fn [s k]
          (let [balls (shuffle (filter (comp #{:circle} :kind)
                                       (lib/entities s)))
                b1 (first balls)
                b2 (second balls)]
            (when (and b1 b2)
              (-> s
                  (update-in [:eid->entity (:id b1) :kinetic-energy]
                             *
                             1.2)
                  (update-in [:eid->entity (:id b2) :kinetic-energy]
                             *
                             1.2)))))))



      #_(lib/live
         (lib/every-n-seconds
          0.2
          (fn [s k]
            (lib/update-ents
             s
             (fn [e]
               (update
                e
                :color
                #(get {:black :white :white :black} % :black)))))))


      (lib/live
       (lib/every-now-and-then
        0.1
        ;; (fn [s k]
        ;;   (let [col (rand-nth [:black :white])]
        ;;     (lib/update-ents
        ;;      s
        ;;      (fn [e]
        ;;        (assoc e :color col)))))
        (fn [s k]
          (let [col (rand-nth [:black :white])]
            (lib/update-ents
             s
             (fn [e]
               (update
                e
                :color
                #(get {:white (rand-nth [(:orig-color e) :black])} % :white))))))))))


#_(defmethod lib/setup-version :not-fire-4
  [state]
  (->
    state
    (assoc :background-fades? true)
    (lib/live
      (lib/every-now-and-then
        (/ 1
           (-> state
               :controls
               (:ball-freq 1)))
        (fn [s k]
          (lib/append-ents
            s
            [(let [orig-col (rand-nth [:cyan :hit-pink :heliotrope
                                       :mint :black :black :black
                                       :black :black :black])]
               (-> (assoc (ball (lib/rand-on-canvas-gauss
                                  (-> s
                                      :controls
                                      :ball-spread)))
                            :immune?
                          (rand-nth [false false false false false
                                     false true])
                            :orig-color
                          orig-col :lifetime
                          (lib/normal-distr 10 2) :kinetic-energy
                          2 :z-index
                          100 :color
                          orig-col)
                   (assoc-in [:transform :scale]
                             ;; (lib/normal-distr 0.5 0.2)
                             (lib/normal-distr 1
                                               0.2
                                               ;; (:ball-scale-base
                                               ;; (lib/controls))
                                               ;; (:ball-scale-stdv
                                               ;; (lib/controls))
                             ))
                   (lib/live
                     (lib/every-now-and-then
                       2
                       (fn [e s k]
                         (if (< (count (lib/entities s)) 1000)
                           {:updated-state
                              (lib/append-ents
                                s
                                [(-> (lib/clone-entity e)
                                     (update :lifetime * 1.02)
                                     (update-in [:transform :scale]
                                                *
                                                (rand-nth [0.6 1.1]))
                                     (update :kinetic-energy
                                             *
                                             (:ball-spread-acc
                                               (lib/controls))))])}
                           e))))))]))))
    (lib/live
      (lib/every-n-seconds
        (fn []
          (lib/normal-distr
            (/ 1 (:line-freq (:controls (q/state)) 1))
            (q/sqrt (/ 1 (:line-freq (:controls (q/state)) 1)))))
        (fn [s k]
          (let [balls (shuffle (filter :ball? (lib/entities s)))
                b1 (first balls)
                b2 (second balls)]
            (when (and b1 b2)
              (lib/append-ents s [(lib/->connection-line b1 b2)]))))))
    (lib/live
      (lib/every-now-and-then
        1
        (fn [s k]
          (let [balls (shuffle (filter :ball? (lib/entities s)))
                b1 (first balls)
                b2 (second balls)]
            (when (and b1 b2)
              (-> s
                  (update-in [:eid->entity (:id b1) :kinetic-energy]
                             *
                             1.2)
                  (update-in [:eid->entity (:id b2) :kinetic-energy]
                             *
                             1.2)))))))
    (lib/live
      (lib/every-n-seconds
        (fn [] (rand-nth [0.1 0.1 0.1 0.2 0.2 0.2 0.5 0.8]))
        ;; (fn [s k]
        ;;   (let [col (rand-nth [:black :white])]
        ;;     (lib/update-ents
        ;;      s
        ;;      (fn [e]
        ;;        (assoc e :color col)))))
        (fn [s k]
          (let [col (rand-nth [:black :white])
                scale (rand-nth
                       ;; [1.1 0.9]
                       [1.5 0.9])
                s (lib/update-ents
                   s
                   (fn [e]
                     (if-not (:ball? e)
                       e
                       (-> e
                           (update :color
                                   #(get {:white
                                          (rand-nth
                                           [(:orig-color
                                             e) :black])}
                                         %
                                         :white))
                           (update-in [:transform :scale]
                                      *
                                      scale)))))

                explode-them (filter (comp #(< 2 %) lib/scale)
                                 (lib/entities s))]
            (reduce (fn [s e]
                      (-> s
                          (lib/append-ents
                           (lib/->explosion
                            {:color :white
                             :n 10
                             :pos (lib/position e)
                             :size 3
                             :spread 5}))
                          (assoc-in [:eid->entity (:id e)
                                     :lifetime]
                                    0.2)))
                    s
                    explode-them)))))))


(defmethod lib/setup-version :not-fire-4
  [state]
  (->
   state
   (assoc :background-fades? true)
   (lib/live
    (lib/every-now-and-then
     (/ 1
        (-> state
            :controls
            (:ball-freq 1)))
     (fn [s k]
       (lib/append-ents
        s
        [(let [orig-col (rand-nth [:cyan :hit-pink :heliotrope
                                   :mint :black :black :black
                                   :black :black :black])]
           (-> (assoc (ball (lib/rand-on-canvas-gauss
                             (-> s
                                 :controls
                                 :ball-spread)))
                      :immune?
                      (rand-nth [false false false false false
                                 false true])
                      :orig-color
                      orig-col :lifetime
                      (lib/normal-distr 10 2) :kinetic-energy
                      2 :z-index
                      100 :color
                      orig-col)
               (assoc-in [:transform :scale]
                         ;; (lib/normal-distr 0.5 0.2)
                         (lib/normal-distr 1
                                           0.2
                                           ;; (:ball-scale-base
                                           ;; (lib/controls))
                                           ;; (:ball-scale-stdv
                                           ;; (lib/controls))
                                           ))
               (lib/live
                (lib/every-now-and-then
                 2
                 (fn [e s k]
                   (if (< (count (lib/entities s)) 1000)
                     {:updated-state
                      (lib/append-ents
                       s
                       [(-> (lib/clone-entity e)
                            (update :lifetime * 1.02)
                            (update-in [:transform :scale]
                                       *
                                       (rand-nth [0.6 1.1]))
                            (update :kinetic-energy
                                    *
                                    (:ball-spread-acc
                                     (lib/controls))))])}
                     e))))))]))))
   (lib/live
    (lib/every-n-seconds
     (fn []
       (lib/normal-distr
        (/ 1 (:line-freq (:controls (q/state)) 1))
        (q/sqrt (/ 1 (:line-freq (:controls (q/state)) 1)))))
     (fn [s k]
       (let [balls (shuffle (filter :ball? (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when (and b1 b2)
           (lib/append-ents s [(lib/->connection-line b1 b2)]))))))
   (lib/live
    (lib/every-now-and-then
     1
     (fn [s k]
       (let [balls (shuffle (filter :ball? (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when (and b1 b2)
           (-> s
               (update-in [:eid->entity (:id b1) :kinetic-energy]
                          *
                          1.2)
               (update-in [:eid->entity (:id b2) :kinetic-energy]
                          *
                          1.2)))))))
   (lib/live
    (lib/every-n-seconds
     (fn [] (rand-nth [0.1 0.1 0.1 0.2 0.2 0.2 0.5 0.8]))
     (fn [s k]
       (let [col (rand-nth [:black :white])
             scale (rand-nth
                    ;; [1.1 0.9]
                    [1.1 0.9])]
         (lib/update-ents
          s
          (fn [e]
            (let [make-circle
                  (fn [ent]
                    (if ((comp #(< 2 %) lib/scale) ent)
                      (-> ent
                          (assoc :no-stroke? false)
                          (assoc :stroke-weight 2)
                          (assoc :color (lib/with-alpha
                                          (lib/->hsb :black)
                                          0))
                          (assoc :stroke (:orig-color ent)))
                      ent))]
              (if-not (:ball? e)
                e
                (->
                 e
                 (update :color
                         #(get {:white (rand-nth [(:orig-color e)
                                                  :black])}
                               %
                               :white))
                 (update :stroke #(get {:white (rand-nth [(:orig-color e) :black])} % :white))
                 (update-in [:transform :scale] * scale)

                 make-circle)))))))))))




(defmethod lib/setup-version :not-fire-5
  [state]
  (->
   state
   (assoc :background-fades? true)
   (lib/live
    (lib/every-now-and-then
     (/ 1
        (-> state
            :controls
            (:ball-freq 1)))
     (fn [s k]
       (lib/append-ents
        s
        [(let [orig-col (rand-nth [:cyan :hit-pink :heliotrope
                                   :mint :black :black :black
                                   :black :black :black])]
           (-> (assoc (ball (lib/rand-on-canvas-gauss
                             (-> s
                                 :controls
                                 :ball-spread)))
                      :immune?
                      (rand-nth [false false false false false
                                 false true])
                      :orig-color
                      orig-col :lifetime
                      (lib/normal-distr 10 2) :kinetic-energy
                      2 :z-index
                      100 :color
                      orig-col)
               (assoc-in [:transform :scale]
                         ;; (lib/normal-distr 0.5 0.2)
                         (lib/normal-distr 1
                                           0.2
                                           ;; (:ball-scale-base
                                           ;; (lib/controls))
                                           ;; (:ball-scale-stdv
                                           ;; (lib/controls))
                                           ))
               (lib/live
                (lib/every-now-and-then
                 2
                 (fn [e s k]
                   (if (< (count (lib/entities s)) 1000)
                     {:updated-state
                      (lib/append-ents
                       s
                       [(-> (lib/clone-entity e)
                            (update :lifetime * 1.02)
                            (update-in [:transform :scale]
                                       *
                                       (rand-nth [0.6 1.1]))
                            (update :kinetic-energy
                                    *
                                    (:ball-spread-acc
                                     (lib/controls))))])}
                     e))))))]))))
   (lib/live
    (lib/every-n-seconds
     (fn []
       (lib/normal-distr
        (/ 1 (:line-freq (:controls (q/state)) 1))
        (q/sqrt (/ 1 (:line-freq (:controls (q/state)) 1)))))
     (fn [s k]
       (let [balls (shuffle (filter :ball? (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when (and b1 b2)
           (lib/append-ents s [(assoc
                                (lib/->connection-line b1 b2)
                                :stroke-weight 1)]))))))
   (lib/live
    (lib/every-now-and-then
     1
     (fn [s k]
       (let [balls (shuffle (filter :ball? (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when (and b1 b2)
           (-> s
               (update-in [:eid->entity (:id b1) :kinetic-energy]
                          *
                          1.2)
               (update-in [:eid->entity (:id b2) :kinetic-energy]
                          *
                          1.2)))))))
   (lib/live
    (lib/every-n-seconds
     (fn [] (rand-nth [0.1 0.1 0.1 0.2 0.2 0.2 0.5 0.8]))
     (let [n (atom 0)]
       (fn [s k]
         (swap! n inc)
         (let [col (rand-nth [:black :white])
               scale (rand-nth
                      [(if (even? @n) 1.5 0.5) 1.1])
               acc (rand-nth [1.5 0.5])]
           (lib/update-ents
            s
            (fn [e]
              (let [make-circle
                    (fn [ent]
                      (if ((comp #(< 2.2 %) lib/scale) ent)
                        (-> ent
                            (assoc :no-stroke? false)
                            (assoc :stroke-weight 2)
                            (assoc :z-index -1000)
                            (assoc :color (lib/with-alpha
                                            (lib/->hsb :black)
                                            0))
                            (assoc :stroke
                                   (rand-nth [:cyan :heliotrope])
                                   ;; (:orig-color ent)
                                   )
                            (assoc :kinetic-energy 0.01))
                        ent))]
                (if-not (:ball? e)
                  e
                  (->
                   e
                   (update :color #(get {:white (rand-nth [(:orig-color e) :black])} % :white))
                   (update :stroke #(get {:white (rand-nth [(:orig-color e) :black])} % :white))
                   (update-in [:transform :scale] * scale)
                   (lib/orient-towards (lib/mid-point))
                   (update :acceleration * acc)
                   make-circle))))))))))


   (lib/live
    (lib/every-n-seconds
     1
     (fn [s k]
       (update-in s
                  [:time-speed]
                  (fn [ts]
                    (let [ts (* ts (rand-nth [2 0.5]))
                          ts (min (max 1 ts) 10)]
                      ts))))))))


(defmethod lib/setup-version :not-fire-6
  [state]
  (->
   state
   (assoc :background-fades? true)
   (lib/live
    (lib/every-now-and-then
     (/ 1
        (-> state
            :controls
            (:ball-freq 1)))
     (fn [s k]
       (lib/append-ents
        s
        [(let [orig-col (rand-nth [:hit-pink :heliotrope
                                   :mint :black :black :black
                                   ])]
           (-> (assoc (ball (lib/rand-on-canvas-gauss
                             (-> s
                                 :controls
                                 :ball-spread)))
                      :immune?
                      (rand-nth [false false false false false
                                 false true])
                      :orig-color
                      orig-col :lifetime
                      (lib/normal-distr 10 2) :kinetic-energy
                      2 :z-index
                      100 :color
                      orig-col)
               (assoc-in [:transform :scale]
                         ;; (lib/normal-distr 0.5 0.2)
                         (lib/normal-distr

                          (:ball-scale-base
                           (lib/controls))
                          (:ball-scale-stdv
                           (lib/controls))
                          ))
               (lib/live
                (lib/every-now-and-then
                 2
                 (fn [e s k]
                   (if (< (count (lib/entities s)) 1000)
                     {:updated-state
                      (lib/append-ents
                       s
                       [(-> (lib/clone-entity e)
                            (update :lifetime * 1.02)
                            (update-in [:transform :scale]
                                       *
                                       (rand-nth [0.6 1.1]))
                            (update :kinetic-energy
                                    *
                                    (:ball-spread-acc
                                     (lib/controls))))])}
                     e))))))]))))
   (lib/live
    (lib/every-n-seconds
     (fn []
       (lib/normal-distr
        (/ 1 (:line-freq (:controls (q/state)) 1))
        (q/sqrt (/ 1 (:line-freq (:controls (q/state)) 1)))))
     (fn [s k]
       (let [balls (shuffle (filter :ball? (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when (and b1 b2)
           (lib/append-ents s
                            [(assoc (lib/->connection-line b1 b2)
                                    :stroke-weight 1)]))))))
   (lib/live
    (lib/every-now-and-then
     1
     (fn [s k]
       (let [balls (shuffle (filter :ball? (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when (and b1 b2)
           (-> s
               (update-in [:eid->entity (:id b1) :kinetic-energy]
                          *
                          1.2)
               (update-in [:eid->entity (:id b2) :kinetic-energy]
                          *
                          1.2)))))))
   (lib/live
    (lib/every-n-seconds
     (fn [] (rand-nth [0.1 0.1 0.1 0.2 0.2 0.2 0.5 0.8]))
     (let [n (atom 0)]
       (fn [s k]
         (swap! n inc)
         (let [col (rand-nth [:black :white])
               scale (rand-nth [(if (even? @n) 1.5 0.5) 1.1])
               acc (rand-nth [1.5 0.5])]
           (lib/update-ents
            s
            (fn [e]
              (let [make-circle
                    (fn [ent]
                      (if ((comp #(< 2.2 %) lib/scale) ent)
                        (let [flips? (rand-nth [false true])]
                          (-> ent
                              (assoc :no-stroke? false)
                              (assoc :stroke-weight 2)
                              (assoc :z-index -1000)
                              (update :lifetime
                                      #(if flips?
                                         (rand-nth [(* 1 %) 0.2
                                                    0.2
                                                    0.2])
                                         %))
                              (update-in [:transform :scale]
                                         *
                                         (if flips? 1.5 1))
                              (assoc :color (lib/with-alpha
                                              (lib/->hsb :black)
                                              0))
                              (assoc :stroke
                                     (if flips?
                                       :white
                                       (rand-nth
                                        [:cyan :heliotrope
                                         :mint :hit-pink])))
                              (assoc :kinetic-energy 0.01)))
                        ent))]
                (if-not (:ball? e)
                  e
                  (-> e
                      (update :color
                              #(get {:cyan (rand-nth
                                            [(:orig-color e)
                                             :black])}
                                    %
                                    :cyan))
                      (update :stroke
                              #(get {:cyan (rand-nth [(:orig-color e) :black])}
                                    %
                                    :cyan))
                      (update-in [:transform :scale] * scale)
                      (lib/orient-towards (lib/mid-point))
                      (update :acceleration * acc)
                      make-circle))))))))))
   (lib/live (lib/every-n-seconds
              0.2
              (fn [s k]
                (update-in s
                           [:time-speed]
                           (fn [ts]
                             (let [ts (* ts (rand-nth [2 1.1 1.2 0.5]))
                                   ts (min (max 1 ts) 10)]
                               ts))))))))





(defmethod lib/setup-version :not-fire-7
  [state]
  (->
   state
   (assoc :background-fades? true)
   (lib/live
    (lib/every-now-and-then
     (/ 1
        (-> state
            :controls
            (:ball-freq 1)))
     (fn [s k]
       (lib/append-ents
        s
        [(let [orig-col (rand-nth [:hit-pink :heliotrope :cyan])]
           (-> (assoc (ball (lib/rand-on-canvas-gauss
                             (-> s
                                 :controls
                                 :ball-spread)))
                      :immune?
                      (rand-nth [false true])
                      :orig-color
                      orig-col :lifetime
                      (lib/normal-distr 20 10)
                      :kinetic-energy
                      2 :z-index
                      100 :color
                      orig-col)
               (assoc-in [:transform :scale]
                         ;; (lib/normal-distr 0.5 0.2)
                         (lib/normal-distr
                          (:ball-scale-base
                           (lib/controls))
                          (:ball-scale-stdv
                           (lib/controls))))
               (lib/live
                (lib/every-now-and-then
                 1
                 (fn [e s k]
                   (if (< (count (lib/entities s)) 1000)
                     {:updated-state
                      (lib/append-ents
                       s
                       [(-> (lib/clone-entity e)
                            (update :lifetime * 1.02)

                            (update-in [:transform :scale] * (rand-nth [0.6 1.1]))
                            (update :kinetic-energy * 1.1))])}
                     e))))))]))))
   (lib/live
    (lib/every-n-seconds
     (fn []
       (lib/normal-distr
        (/ 1 (:line-freq (:controls (q/state)) 1))
        (q/sqrt (/ 1 (:line-freq (:controls (q/state)) 1)))))
     (fn [s k]
       (let [balls (shuffle (filter :ball? (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when (and b1 b2)
           (lib/append-ents s
                            [(assoc (lib/->connection-line b1 b2)
                                    :stroke-weight 1)]))))))
   #_(lib/live
    (lib/every-n-seconds
     (fn [] (rand-nth [0.1 0.1 0.1 0.2 0.2 0.2 0.5 0.8]))
     (let [n (atom 0)]
       (fn [s k]
         (swap! n inc)
         (let [col (rand-nth [:black :white])
               scale (rand-nth [(if (even? @n) 1.5 0.5) 1.1])
               acc (rand-nth [1.5 0.5])]
           (lib/update-ents
            s
            (fn [e]
              (let [make-circle
                    (fn [ent]
                      (if ((comp #(< 2.2 %) lib/scale) ent)
                        (let [flips? (rand-nth [false true])]
                          (-> ent
                              (assoc :no-stroke? false)
                              (assoc :stroke-weight 2)
                              (assoc :z-index -1000)
                              (update :lifetime
                                      #(if flips?
                                         (rand-nth [(* 1 %) 0.2
                                                    0.2
                                                    0.2])
                                         %))
                              (update-in [:transform :scale]
                                         *
                                         (if flips? 1.5 1))
                              (assoc :color (lib/with-alpha
                                              (lib/->hsb :black)
                                              0))
                              (assoc :stroke
                                     (rand-nth
                                      [:cyan
                                       :heliotrope
                                       :mint
                                       :hit-pink]))
                              (assoc :kinetic-energy 0.01)))
                        ent))]
                (if-not (or (:ball? e) (:immune? e))
                  e
                  (-> e
                      (assoc :color (rand-nth [:cyan :mint :heliotrope :hit-pink]))
                      (assoc :stroke (rand-nth [:cyan :mint :heliotrope :hit-pink]))
                      (update-in [:transform :scale] * scale)
                      (lib/orient-towards (lib/mid-point))
                      ;; (update :acceleration * acc)
                      make-circle))))))))))
   (lib/live (lib/every-n-seconds
              0.2
              (fn [s k]
                (update-in s
                           [:time-speed]
                           (fn [ts]
                             (let [ts (* ts (rand-nth [1.2 1.1 1.2 0.5]))
                                   ts (min (max 1 ts) 10)]
                               ts))))))))

(defmethod lib/setup-version :not-fire-8
  [state]
  (->
   state
   (assoc :background-fades? true)
   (lib/live
    (lib/every-now-and-then
     (/ 1
        (-> state
            :controls
            (:ball-freq 1)))
     (fn [s k]
       (lib/append-ents
        s
        [(let [orig-col (rand-nth [:hit-pink :heliotrope :cyan])]
           (-> (assoc (ball (lib/rand-on-canvas-gauss
                             (-> s
                                 :controls
                                 :ball-spread)))
                      :immune? (rand-nth [false true
                                          true
                                          true
                                          true])
                      :orig-color
                      orig-col :lifetime
                      (lib/normal-distr 20 10)
                      :kinetic-energy
                      2 :z-index
                      100 :color
                      orig-col)
               (assoc-in [:transform :scale]
                         ;; (lib/normal-distr 0.5 0.2)
                         (lib/normal-distr
                          (:ball-scale-base
                           (lib/controls))
                          (:ball-scale-stdv
                           (lib/controls))))
               (lib/live
                (lib/every-now-and-then
                 1
                 (fn [e s k]
                   (if
                       (and
                        (< (lib/scale e) 1)
                        (< (count (lib/entities s)) 1000))
                       {:updated-state
                        (lib/append-ents
                         s
                         [(-> (lib/clone-entity e)
                              (update :lifetime * 1.02)
                              (assoc :immune? true)

                              (assoc :kinetic-energy (rand-nth [1.1 0.9]))
                              (update :kinetic-energy (fn [ke] (max (min 3 ke) 0)))

                              (update-in [:transform :scale] * (rand-nth [1.1 1.1 1.05]))


                              ;; (update :kinetic-energy * 1.05)
                              )])}
                       e))))))]))))
   (lib/live
    (lib/every-n-seconds
     (fn []
       (lib/normal-distr
        (/ 1 (:line-freq (:controls (q/state)) 1))
        (q/sqrt (/ 1 (:line-freq (:controls (q/state)) 1)))))
     (fn [s k]
       (let [balls (shuffle (filter :ball? (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when (and b1 b2)
           (lib/append-ents s
                            [(assoc (lib/->connection-line b1 b2)
                                    :stroke-weight 1)]))))))

   (lib/live
    (lib/every-n-seconds
     (fn [] (rand-nth [0.1 0.1 0.1 0.2 0.2 0.2 0.5 0.8]))
     (let [n (atom 0)]
       (fn [s k]
         (swap! n inc)
         (let [col (rand-nth [:black :white])
               scale (rand-nth [1.2 (/ 1 1.2)])
               acc (rand-nth [1.5 0.5])]
           (lib/update-ents
            s
            (fn [e]
              (let [make-circle
                    (fn [ent]
                      (if ((comp #(< 2.2 %) lib/scale) ent)
                        (let [flips? (rand-nth [false true
                                                true
                                                true])]
                          (-> ent
                              (assoc :no-stroke? false)
                              (assoc :stroke-weight 2)
                              (assoc :z-index -1000)
                              (update :lifetime
                                      #(if flips?
                                         (rand-nth [(* 1 %) 0.2
                                                    0.2
                                                    0.2])
                                         %))
                              (update-in [:transform :scale]
                                         *
                                         (if flips? 1.5 1))
                              (assoc :color (lib/with-alpha
                                              (lib/->hsb :black)
                                              0))
                              (assoc :stroke
                                     (rand-nth
                                      [:cyan
                                       :heliotrope
                                       :mint
                                       :hit-pink]))
                              (assoc :kinetic-energy 0.1)
                              ))
                        ent))]
                (if-not (or (:ball? e) (:immune? e))
                  e
                  (-> e
                      (assoc :color (rand-nth [(:orig-color e)
                                               (:orig-color e)
                                               (:orig-color e)
                                               :white]))
                      (assoc :stroke (rand-nth [(:orig-color e)
                                               (:orig-color e)
                                               (:orig-color e)
                                               :white]))
                      (update-in [:transform :scale] * scale)
                      (lib/orient-towards (lib/mid-point))
                      ;; (update :acceleration * acc)
                      make-circle))))))))))

   (lib/live (lib/every-n-seconds
              0.2
              (fn [s k]
                (update-in s
                           [:time-speed]
                           (fn [ts]
                             (let [ts (* ts (rand-nth [1.2 1.1 1.2 0.5]))
                                   ts (min (max 1 ts) 5)]
                               ts))))))))






















(defmethod lib/setup-version :not-fire-9
  [state]
  (->
   state
   (assoc :background-fades? true)
   (lib/live
    (lib/every-now-and-then
     (/ 1
        (-> state
            :controls
            (:ball-freq 1)))
     (fn [s k]
       (lib/append-ents
        s
        [(let [orig-col (rand-nth [:hit-pink :heliotrope :cyan])]
           (-> (assoc (ball (lib/rand-on-canvas-gauss
                             (-> s
                                 :controls
                                 :ball-spread)))
                      :immune? (rand-nth [false true
                                          true
                                          true
                                          true])
                      :orig-color
                      orig-col :lifetime
                      (lib/normal-distr 5 2)
                      :kinetic-energy
                      2 :z-index
                      100 :color
                      orig-col)
               (assoc-in [:transform :scale]
                         ;; (lib/normal-distr 0.5 0.2)
                         (lib/normal-distr
                          (:ball-scale-base
                           (lib/controls))
                          (:ball-scale-stdv
                           (lib/controls))))
               (lib/live
                (lib/every-now-and-then
                 1
                 (fn [e s k]
                   (if
                       (and
                        (< (lib/scale e) 1)
                        (< (count (lib/entities s)) 1000))
                       {:updated-state
                        (lib/append-ents
                         s
                         [(-> (lib/clone-entity e)
                              (update :lifetime * 1.02)
                              (assoc :immune? true)

                              (assoc :kinetic-energy (rand-nth [1.1 0.9]))
                              (update :kinetic-energy (fn [ke] (max (min 3 ke) 0)))

                              (update-in [:transform :scale] * (rand-nth [1.1 1.1 1.05]))


                              ;; (update :kinetic-energy * 1.05)
                              )])}
                       e))))))]))))
   (lib/live
    (lib/every-n-seconds
     (fn []
       (lib/normal-distr
        (/ 1 (:line-freq (:controls (q/state)) 1))
        (q/sqrt (/ 1 (:line-freq (:controls (q/state)) 1)))))
     (fn [s k]
       (let [balls (shuffle (filter :ball? (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when (and b1 b2)
           (lib/append-ents s
                            [(assoc (lib/->connection-line b1 b2)
                                    :stroke-weight 1)]))))))

   (lib/live
    (lib/every-n-seconds
     (fn [] (rand-nth [0.1 0.1 0.1 0.2 0.2 0.2 0.5 0.8]))
     (let [n (atom 0)]
       (fn [s k]
         (swap! n inc)
         (let [col (rand-nth [:black :white])
               scale (rand-nth [1.2 (/ 1 1.2)])
               acc (rand-nth [1.5 0.5])]
           (lib/update-ents
            s
            (fn [e]
              (let [make-circle
                    (fn [ent]
                      (if ((comp #(< 2.2 %) lib/scale) ent)
                        (let [flips? (rand-nth [false true
                                                true
                                                true])]
                          (-> ent
                              (assoc :no-stroke? false)
                              (assoc :stroke-weight 2)
                              (assoc :z-index -1000)
                              (update :lifetime
                                      #(if flips?
                                         (rand-nth [(* 1 %) 0.2
                                                    0.2
                                                    0.2])
                                         %))
                              (update-in [:transform :scale]
                                         *
                                         (if flips? 1.5 1))
                              (assoc :color (lib/with-alpha
                                              (lib/->hsb :black)
                                              0))
                              (assoc :stroke
                                     (rand-nth
                                      [:cyan
                                       :heliotrope
                                       :mint
                                       :hit-pink]))
                              (assoc :kinetic-energy 0.1)
                              ))
                        ent))]
                (if-not (or (:ball? e) (:immune? e))
                  e
                  (-> e
                      (assoc :color (rand-nth [(:orig-color e)
                                               (:orig-color e)
                                               (:orig-color e)
                                               :white]))
                      (assoc :stroke (rand-nth [(:orig-color e)
                                                (:orig-color e)
                                                (:orig-color e)
                                                :white]))
                      (update-in [:transform :scale] * scale)
                      (lib/orient-towards (lib/mid-point))
                      ;; (update :acceleration * acc)
                      make-circle))))))))))

   (lib/live (lib/every-n-seconds
              0.2
              (fn [s k]
                (update-in s
                           [:time-speed]
                           (fn [ts]
                             (let [ts (* ts (rand-nth [1.2 1.1 1.2 0.5]))
                                   ts (min (max 1 ts) 5)]
                               ts))))))))


(defmethod lib/setup-version :not-fire-10
  [state]
  (->
    state
    (assoc :background-fades? true)
    (lib/live
      (lib/every-now-and-then
        (/ 1
           (-> state
               :controls
               (:ball-freq 1)))
        (fn [s k]
          (lib/append-ents
            s
            [(let [orig-col (rand-nth [:hit-pink :heliotrope :cyan
                                       :mint])]
               (->
                 (assoc (ball (lib/rand-on-canvas-gauss
                                (-> s
                                    :controls
                                    :ball-spread)))
                          :orig-color
                        orig-col :lifetime
                        (lib/normal-distr 5 2) :kinetic-energy
                        2 :z-index
                        100 :color
                        orig-col)
                 (assoc-in [:transform :scale]
                           ;; (lib/normal-distr 0.5 0.2)
                           (lib/normal-distr
                             (:ball-scale-base (lib/controls))
                             (:ball-scale-stdv (lib/controls))))
                 (lib/live
                   (lib/every-now-and-then
                     2
                     (fn [e s k]
                       (if (and
                             ;; (< (lib/scale e) 1)
                             (< (count (lib/entities s)) 1000))
                         {:updated-state
                            (lib/append-ents
                              s
                              [(->
                                 (lib/clone-entity e)
                                 (update :lifetime * 1.02)
                                 (assoc :immune? true)
                                 ;; (update-in [:transform :pos]
                                 ;;            +
                                 ;;            [(rand-nth [-10
                                 ;;            10])
                                 ;;             (rand-nth [-10
                                 ;;             10])])
                                 (assoc :kinetic-energy (rand-nth
                                                          [1.1 0.9]))
                                 (update :kinetic-energy
                                         (fn [ke] (max (min 3 ke) 0)))
                                 (update-in [:transform :scale]
                                            *
                                            (rand-nth [1.1 1.05 0.9]))
                                 ;; (update :kinetic-energy *
                                 ;; 1.05)
                               )])}
                         e))))))]))))
    (lib/live
      (lib/every-n-seconds
        (fn []
          (lib/normal-distr
            (/ 1 (:line-freq (:controls (q/state)) 1))
            (q/sqrt (/ 1 (:line-freq (:controls (q/state)) 1)))))
        (fn [s k]
          (let [balls (shuffle (filter :ball? (lib/entities s)))
                b1 (first balls)
                b2 (second balls)]
            (when (and b1 b2)
              (lib/append-ents s
                               [(assoc (lib/->connection-line b1 b2)
                                  :stroke-weight 1)]))))))
    (lib/live
      (lib/every-n-seconds
        (fn [] (rand-nth [2]))
        (let [n (atom 0)]
          (fn [s k]
            (swap! n inc)
            (let [col (rand-nth [:black :white])
                  scale (rand-nth [1.2 (/ 1 1.2)])
                  acc (rand-nth [1.5 0.5])]
              (lib/update-ents
                s
                (fn [e]
                  (let [make-circle
                          (fn [ent]
                            (if ((comp #(< 2.2 %) lib/scale) ent)
                              (let [flips? (rand-nth [false true true
                                                      true])]
                                (-> ent
                                    (assoc :no-stroke? false)
                                    (assoc :stroke-weight 2)
                                    (assoc :z-index -1000)
                                    (update :lifetime
                                            #(if flips?
                                               (rand-nth [(* 1 %) 0.2
                                                          0.2 0.2])
                                               %))
                                    (update-in [:transform :scale]
                                               *
                                               (if flips? 1.5 1))
                                    (assoc :color (lib/with-alpha
                                                    (lib/->hsb :black)
                                                    0))
                                    (assoc :stroke
                                             (rand-nth
                                               [:cyan :heliotrope
                                                :mint :hit-pink]))
                                    (assoc :kinetic-energy 0.1)))
                              ent))]
                    (if-not (or (:ball? e) (:immune? e))
                      e
                      (-> e
                          ;; (assoc :color (rand-nth
                          ;; [(:orig-color e)
                          ;;                          col]))
                          ;; (assoc :stroke (rand-nth
                          ;; [(:orig-color e)
                          ;;                           col]))
                          (update-in [:transform :scale] * scale)
                          (lib/orient-towards (lib/mid-point))
                          ;; (update :acceleration * acc)
                          make-circle))))))))))
    (lib/live (lib/every-n-seconds
                2
                (fn [s k]
                  (update-in s
                             [:time-speed]
                             (fn [ts]
                               ;; (- ts (* 2 (q/sin (q/millis))))
                               (let [ts (* ts
                                           (rand-nth [1.2 1.5 2 0.6]))
                                     ts (min (max 1 ts) 5)]
                                 ts))))))))





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
              :draw #'draw-state
              :features [:keep-on-top]
              :middleware [m/fun-mode]
              :frame-rate 30)))

;; --------------------
;; I need to update 'lib/the-state' to have multiple of these in the gallery
;; - or put iframes in the gallery !
;; --------------------

(defonce restart-fn (atom (constantly nil)))
(defmethod art/view "not-fire"
  [{:as opts :keys [place version]}]
  (let [controls (merge
                  (controls/default-versions "not-fire")
                  (get-in versions ["not-fire" version])
                  @user-controls/!app)]
    (sketch place opts controls)))
