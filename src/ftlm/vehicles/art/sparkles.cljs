(ns ftlm.vehicles.art.sparkles
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
  (if ((q/state) :background-fades?)
    (do (lib/draw-entities state)
        (q/color-mode :rgb)
        ;; (q/fill 256 256 256 10)
        (q/fill 0 0 0 5)
        (q/rect 0 0 (* 2 (q/width)) (* 2 (q/height)))
        )
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
                 :kinetic-energy 1
                 :lifetime 10
                 :no-stroke? true
                 :particle? true
                 :transform (lib/->transform pos 20 20 1)}))

(defmethod lib/setup-version :sparkles-1
  [state]
  (-> state
      ;; (assoc :background-color (lib/->hsb :black)
      ;;        :background-value 1)
      ;; (lib/live
      ;;   (fn [s k]
      ;;     (-> s
      ;;         (update :background-value + (* *dt* 20))
      ;;         (update-in
      ;;           [:background-color]
      ;;           (fn [c]
      ;;             (q/lerp-color
      ;;               (lib/->hsb :black)
      ;;               (lib/->hsb :white)
      ;;               (+ 0.5 (* 0.5 (q/sin (:background-value
      ;;               s))))))))))
      (lib/live
        (lib/every-now-and-then
          0.2
          (fn [s k]
            (lib/append-ents
              s
              [(-> (ball (lib/rand-on-canvas-gauss 0.2))
                   (lib/live (lib/every-now-and-then
                               0.2
                               (fn [e s k]
                                 (assoc-in e
                                   [:color]
                                   (rand-nth [:black :white])))))
                   (lib/live (lib/every-now-and-then
                               1
                               (fn [e s k]
                                 (-> e
                                     (update-in [:transform :scale]
                                                *
                                                (rand-nth
                                                  [0.9 0.9 1.1 1.1 1.1
                                                   2])))))))]))))
      (lib/live
        (lib/every-now-and-then
          0.2
          (fn [s k]
            (let [balls (shuffle (filter (comp #{:circle} :kind)
                                   (lib/entities s)))
                  b1 (first balls)
                  b2 (second balls)]
              (when (and b1 b2)
                (lib/append-ents
                  s
                  [(assoc (lib/->connection-line b1 b2)
                     :color (rand-nth [:white :black]))]))))))))


(defmethod lib/setup-version :sparkles-2
  [state]
  (-> state
      (lib/live (lib/every-now-and-then
                  0.2
                  (fn [s k]
                    (lib/append-ents s
                                     [(ball (lib/rand-on-canvas-gauss
                                              0.2))]))))
      (lib/live (lib/every-now-and-then
                  0.2
                  (fn [s k]
                    (let [balls (shuffle (filter (comp #{:circle}
                                                       :kind)
                                           (lib/entities s)))
                          b1 (first balls)
                          b2 (second balls)]
                      (when (and b1 b2)
                        (lib/append-ents
                          s
                          [(assoc (lib/->connection-line b1 b2)
                             :color (rand-nth [:white :black]))]))))))
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
                               5)
                    (update-in [:eid->entity (:id b2) :kinetic-energy]
                               *
                               5)))))))))


(defmethod lib/setup-version :sparkles-3
  [state]
  (->
   state
   (lib/live
    (lib/every-now-and-then
     0.2
     (fn [s k]
       (assoc
        s
        :background-color
        (rand-nth
         [controls/quite-green
          ;; controls/olive-lime-green
          ;; (:horizon controls/color-map)
          (:cyan controls/color-map)
          (:red controls/color-map)
          ])))))

   (lib/live
    (lib/every-now-and-then
     0.2
     (fn [s k]
       (lib/append-ents s

                        [(assoc
                          (ball (lib/rand-on-canvas-gauss 0.2))
                          :color
                          (rand-nth
                           [controls/quite-green
                            controls/olive-lime-green
                            (:horizon controls/color-map)
                            (:cyan controls/color-map)])
                          )]))))

   (lib/live
    (lib/every-now-and-then
     0.2
     (fn [s k]
       (let [balls (shuffle (filter (comp #{:circle} :kind)
                                    (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when (and b1 b2)
           (lib/append-ents
            s
            [(lib/->connection-line b1 b2)]))))))

   (lib/live
    (lib/every-now-and-then
     1
     (fn [s k]
       (let [balls (shuffle (filter (comp #{:circle} :kind)
                                    (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when
             (and b1 b2)
             (->
              s
              (update-in [:eid->entity (:id b1) :kinetic-energy] * 5)
              (update-in [:eid->entity (:id b2) :kinetic-energy] * 5)))))))))


(defmethod lib/setup-version :sparkles-4
  [state]
  (->
   state
   (lib/live
    (lib/every-now-and-then
     2
     (fn [s k]
       (assoc
        s
        :background-color
        (rand-nth
         [controls/quite-green
          ;; controls/olive-lime-green
          ;; (:horizon controls/color-map)
          (:cyan controls/color-map)
          (:red controls/color-map)
          ])))))

   (lib/live
    (lib/every-now-and-then
     1
     (fn [s k]
       (update
        s
        :background-fades?
        not
        ))))

   (lib/live
    (lib/every-now-and-then
     0.2
     (fn [s k]
       (lib/append-ents
        s
        [(assoc
          (ball (lib/rand-on-canvas-gauss 0.2))
          :color
          (rand-nth
           [controls/quite-green
            controls/olive-lime-green
            (:horizon controls/color-map)
            (:cyan controls/color-map)]))]))))

   (lib/live
    (lib/every-now-and-then
     0.2
     (fn [s k]
       (let [balls (shuffle (filter (comp #{:circle} :kind)
                                    (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when (and b1 b2)
           (lib/append-ents
            s
            [(lib/->connection-line b1 b2)]))))))


   (lib/live
    (lib/every-now-and-then
     0.2
     (fn [s k]
       (let [balls (shuffle (filter (comp #{:circle} :kind)
                                    (lib/entities s)))
             b1 (first balls)]
         (when b1
           (-> s
               (assoc-in
                [:eid->entity (:id b1) :color]
                {:h 0 :s 255 :v 100})
               (assoc-in [:eid->entity (:id b1) :transform :scale] 2)))))))

   (lib/live
    (lib/every-now-and-then
     1
     (fn [s k]
       (let [balls (shuffle (filter (comp #{:circle} :kind)
                                    (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when
             (and b1 b2)
             (->
              s
              (update-in [:eid->entity (:id b1) :kinetic-energy] * 5)
              (update-in [:eid->entity (:id b2) :kinetic-energy] * 5)))))))))


#_(defmethod lib/setup-version :sparkles-5
  [state]
  (->
   state


   (lib/live
    (lib/every-now-and-then
     0.2
     (fn [s k]
       (assoc
        s
        :background-color
        (rand-nth
         [controls/quite-green
          (:red controls/color-map)])))))

   (lib/live
    (lib/every-now-and-then
     5
     (fn [s k]
       (assoc
        s
        :background-color
        (rand-nth
         [:white :black]
         )))))

   (lib/live
    (lib/every-now-and-then
     0.2
     (fn [s k]
       (assoc s :background-fades? true)
       ;; (update s :background-fades? not)
       )))


   (lib/live
    (lib/every-now-and-then
     0.2
     (fn [s k]
       (lib/append-ents
        s
        [(assoc
          (ball (lib/rand-on-canvas-gauss 0.1))
          :kinetic-energy 2
          :color
          (rand-nth
           [:black :white
            (:cyan controls/color-map)
            controls/olive-lime-green
            controls/quite-green
            ]))]))))

   (lib/live
    (lib/every-now-and-then
     0.2
     (fn [s k]
       (let [balls (shuffle (filter (comp #{:circle} :kind)
                                    (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when (and b1 b2)
           (lib/append-ents
            s
            [(lib/->connection-line b1 b2)]))))))


   (lib/live
    (lib/every-now-and-then
     0.2
     (fn [s k]
       (let [balls (shuffle (filter (comp #{:circle} :kind)
                                    (lib/entities s)))
             b1 (first balls)]
         (when b1
           (-> s
               (assoc-in
                [:eid->entity (:id b1) :color]
                (rand-nth (into (vals controls/color-map))))
               (update-in [:eid->entity (:id b1) :transform :scale] * 1.1)))))))

   (lib/live
    (lib/every-now-and-then
     1
     (fn [s k]
       (let [balls (shuffle (filter (comp #{:circle} :kind)
                                    (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when
             (and b1 b2)
             (->
              s
              (update-in [:eid->entity (:id b1) :kinetic-energy] * 5)
              (update-in [:eid->entity (:id b2) :kinetic-energy] * 5)))))))))






#_(defmethod lib/setup-version :sparkles-5
  [state]
  (->
   state


   (lib/live
    (lib/every-now-and-then
     0.2
     (fn [s k]
       (assoc
        s
        :background-color
        (lib/with-alpha
          (rand-nth [controls/quite-green (:red controls/color-map)])
          (rand-nth [0.1 1]))))))

   (lib/live
    (lib/every-now-and-then
     0.2
     (fn [s k]
       (lib/append-ents
        s
        [

         (->

          (assoc
           (ball (lib/rand-on-canvas-gauss 0.1))
           :kinetic-energy 2



           :color
           (rand-nth
            [:black :white
             (:cyan controls/color-map)
             controls/olive-lime-green
             controls/quite-green
             ]))


          (lib/live
           (lib/every-now-and-then 1
                                   (fn [e s k]
                                     (->
                                      e
                                      (assoc-in [:transform :scale] (rand-nth [0.1 0.5 1 1.5]))
                                      (assoc-in [:color] (rand-nth [:black :white])))
                                     )
                                   )
           )


          )]))))

   (lib/live
    (lib/every-now-and-then
     0.2
     (fn [s k]
       (let [balls (shuffle (filter (comp #{:circle} :kind)
                                    (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when (and b1 b2)
           (lib/append-ents
            s
            [(lib/->connection-line b1 b2)]))))))


   (lib/live
    (lib/every-now-and-then
     0.2
     (fn [s k]
       (let [balls (shuffle (filter (comp #{:circle} :kind)
                                    (lib/entities s)))
             b1 (first balls)]
         (when b1
           (-> s
               (assoc-in
                [:eid->entity (:id b1) :color]
                (rand-nth (into (vals controls/color-map))))
               (update-in [:eid->entity (:id b1) :transform :scale] * 1.1)))))))

   (lib/live
    (lib/every-now-and-then
     1
     (fn [s k]
       (let [balls (shuffle (filter (comp #{:circle} :kind)
                                    (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when
             (and b1 b2)
             (->
              s
              (update-in [:eid->entity (:id b1) :kinetic-energy] * 5)
              (update-in [:eid->entity (:id b2) :kinetic-energy] * 5)))))))))



(defmethod lib/setup-version :sparkles-5
  [state]
  (->
   state

   ;; (lib/live
   ;;  (lib/every-now-and-then
   ;;   0.2
   ;;   (fn [s k]
   ;;     (assoc
   ;;      s
   ;;      :background-color
   ;;      (lib/with-alpha
   ;;        (rand-nth [:white :black :black :black :black])
   ;;        (rand-nth [0.1 1]))))))

   (lib/live
    (lib/every-now-and-then
     0.1
     (fn [s k]
       (lib/append-ents
        s
        [
         (->
          (assoc
           (ball (lib/rand-on-canvas-gauss 0.1))
           :kinetic-energy 2
           :color
           (rand-nth [:black :white :cyan]))
          ;; (lib/live (lib/every-now-and-then
          ;;            1
          ;;            (fn [e s k]
          ;;              (-> e
          ;;                  (update-in [:transform :scale] * 0.5 1.5 2)
          ;;                  (assoc-in
          ;;                   [:color]
          ;;                   (lib/with-alpha
          ;;                     (lib/->hsb
          ;;                      (rand-nth [:black :white]))
          ;;                     0.9))))))
          )]))))


   #_(lib/live
      (lib/every-now-and-then
       0.05
       (fn [s k]
         (let [cnt (count (filter (comp #{:triangle} :kind) (lib/entities s)))]
           (lib/append-ents
            s
            [(->
              (lib/->entity
               :triangle
               {:transform
                (lib/->transform

                 ;; idx: cnt

                 [
                  (+ (* (mod cnt 5) 100))
                  (+ (* (/ cnt 5) 100))
                  ]
                 100 100
                 1)
                :color :orange
                ;; :lifetime
                ;; (* 20 (rand-nth [1 2 5]))
                })
              (lib/live
               (lib/every-now-and-then
                10
                (fn [e s k]
                  (assoc e :particle? true :kinetic-energy 1))))
              (lib/live
               (lib/every-now-and-then
                2
                (fn [e s k]
                  (update
                   e
                   :color
                   {:orange :white
                    :white :black
                    :black :orange})))))])))))

   (lib/live
    (lib/every-now-and-then
     0.2
     (fn [s k]
       (let [balls (shuffle (filter (comp #{:circle} :kind)
                                    (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when (and b1 b2)
           (lib/append-ents
            s
            [(lib/->connection-line b1 b2)]))))))

   (lib/live
    (lib/every-now-and-then
     1
     (fn [s k]
       (let [balls (shuffle (filter (comp #{:circle} :kind)
                                    (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when
             (and b1 b2)
             (->
              s
              (update-in [:eid->entity (:id b1) :kinetic-energy] * 5)
              (update-in [:eid->entity (:id b2) :kinetic-energy] * 5)))))))))


(defmethod lib/setup-version :sparkles-6
  [state]
  (->
   state
   (assoc :background-fades? true)
   (lib/live
    (lib/every-now-and-then
     1
     (fn [s k]
       (lib/append-ents
        s
        [(->
          (assoc
           (ball (lib/rand-on-canvas-gauss 0.1))

           :kinetic-energy 2
           :z-index 100
           :color (rand-nth [:white :cyan :hit-pink
                             :heliotrope]))
          (assoc-in [:transform :scale] (lib/normal-distr 1 0.5))
          (lib/live
           (lib/every-now-and-then
            2
            (fn [e s k]
              (when
                  (< (count (lib/entities s)) 500)
                  {:updated-state
                   (lib/append-ents
                    s
                    [(->
                      (lib/clone-entity e)
                      (update :lifetime * 1.02)
                      (assoc-in
                       [:transform :pos]
                       (lib/rand-on-canvas)))])})))))]))))

   (lib/live
    (lib/every-now-and-then
     0.2
     (fn [s k]
       (let [balls (shuffle (filter (comp #{:circle} :kind)
                                    (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when (and b1 b2)
           (lib/append-ents s [(lib/->connection-line b1 b2)]))))))

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

(defmethod lib/setup-version :sparkles-7
  [state]
  (->
   state
   (assoc :background-fades? true)
   (lib/live
    (lib/every-now-and-then
     1
     (fn [s k]
       (lib/append-ents
        s
        [(->
          (assoc (ball (lib/rand-on-canvas-gauss 0.1)) :lifetime
                 10
                 ;; (lib/normal-distr 5 5)
                 :kinetic-energy
                 2 :z-index
                 100 :color
                 (rand-nth [:cyan :hit-pink :heliotrope :mint]))
          (assoc-in [:transform :scale]
                    (lib/normal-distr 0.7 0.3))
          (lib/live (lib/every-now-and-then
                     2
                     (fn [e s k]
                       (when (< (count (lib/entities s)) 500)
                         {:updated-state
                          (lib/append-ents
                           s
                           [(-> (lib/clone-entity e)
                                (update :lifetime * 1.02)
                                (update :kinetic-energy
                                        *
                                        2))])})))))]))))
   (lib/live
    (lib/every-now-and-then
     2
     (fn [s k]
       (let [balls (shuffle (filter (comp #{:circle} :kind)
                                    (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when (and b1 b2)
           (lib/append-ents s [(lib/->connection-line b1 b2)]))))))
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






(defn update-intensity-osc
  [e s _]
  (let [speed 1
        cycle-duration 20000
        e (update
           e
           :intensity-factor
           (fn [x]
             (let [fade-factor (-> (* (/ (q/millis)
                                         cycle-duration)
                                      q/TWO-PI)
                                   (Math/sin)
                                   (Math/abs))
                   wave-value (* fade-factor
                                 (+ x (* lib/*dt* speed)))]
               wave-value)))]
    (assoc e :intensity (+ 10 (* 30 (:intensity-factor e))))))

(defn ->ray-source
  ([] (->ray-source (lib/mid-point)))
  ([pos]
   (let [e (lib/->entity :circle
                         {:activation-shine-colors
                            {:high (:heliotrope controls/color-map)
                             :low controls/white}
                          :color (:white controls/color-map)
                          :intensity 20
                          :intensity-factor 1
                          :kinetic-energy 0.02
                          :makes-circular-shines? true
                          :on-drag-start-map
                            {:survive (fn [e s k]
                                        (dissoc e :lifetime))}
                          :particle? true
                          :pos pos
                          :ray-source? true
                          :scale 0.75
                          :shinyness nil
                          :transform (lib/->transform pos 20 20 1)
                          :unique-id :the-ray-source})]
     (-> e
         (lib/live (lib/every-now-and-then
                     10
                     (fn [e s k]
                       (-> e
                           (assoc-in [:transform :scale] 1)
                           (assoc-in [:intensity] 20)
                           (assoc-in [:intensity-factor] 1)))))
         (lib/live [:shine
                    (fn [e s k]
                      (lib/flash-shine-1 e
                                         (/ (:intensity e) 20)
                                         {:high (:heliotrope
                                                  controls/color-map)
                                          :low controls/white}))])
         (lib/live
           [:circular-shine-radio
            (lib/every-n-seconds
              (fn [] (lib/normal-distr (/ 1.5 3) (/ 1.5 3)))
              (fn [ray s k]
                {:updated-state
                   (lib/append-ents
                     s
                     [(let [e (lib/->circular-shine-1 ray)]
                        (->
                         e
                         (assoc :z-index -1)
                         (assoc :color (lib/with-alpha
                                         (:yellow controls/color-map)
                                         0))
                         (assoc :stroke-weight 3)
                         (assoc :stroke (:color ray))
                         (assoc :on-update
                                [(lib/->grow
                                  (* 2
                                     (+ 1
                                        (:intensity-factor ray
                                                           0))))])
                         (assoc :lifetime (* 5
                                             (lib/normal-distr
                                              3
                                              (Math/sqrt
                                               3))))))])}))])
         (lib/live [:intensity-osc update-intensity-osc])))))

(defmethod lib/setup-version :sparkles-8
  [state]
  (let [rs (->ray-source)]
    (-> state
        (assoc :background-fades? true)
        (lib/append-ents [rs])

        (lib/live
         (lib/every-now-and-then
          0.2
          (fn [s k]
            (let [balls (shuffle (filter (comp #{:circle} :kind)
                                         (lib/entities s)))
                  b1 (first balls)
                  b2 rs]
              (when (and b1 b2)
                (lib/append-ents s
                                 [(lib/->connection-line b1 b2)]))))))


        (lib/live
         (lib/every-n-seconds
          (fn []
            ;; (println (q/millis))
            (if (< 20000 (q/millis)) 0.2 20))
          (fn [s k]
            (lib/append-ents
             s
             [(->
               (assoc (ball (lib/rand-on-canvas-gauss 5))
                      :lifetime 10
                      :kinetic-energy
                      2
                      :z-index
                      100
                      :color
                      (rand-nth
                       [:white :yellow :hit-pink]
                       ;; [:cyan :hit-pink :heliotrope :mint]
                       ))
               (assoc-in [:transform :scale]
                         1
                         )

               #_
               (lib/live
                (lib/every-now-and-then
                 2
                 (fn [e s k]
                   (when (< (count (lib/entities s)) 500)
                     {:updated-state
                      (lib/append-ents
                       s
                       [(-> (lib/clone-entity e)
                            (update :lifetime * 1.02)
                            (update :kinetic-energy
                                    *
                                    2))])})))))]))))



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
                               1.2))))))))))


(defmethod lib/setup-version :sparkles-9
  [state]
  (->
   state
   (assoc :background-fades? true)
   (lib/live
    (lib/every-now-and-then
     1
     (fn [s k]
       (lib/append-ents
        s
        [(->
          (assoc (ball (lib/rand-on-canvas-gauss 0.2)) :lifetime
                 10
                 ;; (lib/normal-distr 5 5)
                 :kinetic-energy
                 2 :z-index
                 100 :color
                 (rand-nth [:cyan :hit-pink :heliotrope :mint]))
          (assoc-in [:transform :scale]
                    (lib/normal-distr 0.5 0.2))
          (lib/live (lib/every-now-and-then
                     2
                     (fn [e s k]
                       (when (< (count (lib/entities s)) 500)
                         {:updated-state
                          (lib/append-ents
                           s
                           [(-> (lib/clone-entity e)
                                (update :lifetime * 1.02)
                                (update :kinetic-energy
                                        *
                                        2))])})))))]))))
   (lib/live
    (lib/every-n-seconds
     (fn []
       (lib/normal-distr
        (/ 1 (:line-freq (:controls (q/state))))
        (q/sqrt (/ 1 (:line-freq (:controls (q/state)))))))
     (fn [s k]
       (let [balls (shuffle (filter (comp #{:circle} :kind)
                                    (lib/entities s)))
             b1 (first balls)
             b2 (second balls)]
         (when (and b1 b2)
           (lib/append-ents s [(lib/->connection-line b1 b2)]))))))
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
(defmethod art/view "sparkles"
  [{:as opts :keys [place version]}]
  (let [controls (merge
                  (controls/default-versions "sparkles")
                  (get-in versions ["sparkles" version])
                  @user-controls/!app)]
    (sketch place opts controls)))
