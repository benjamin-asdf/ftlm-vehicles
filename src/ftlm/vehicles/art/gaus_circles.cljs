(ns ftlm.vehicles.art.gaus-circles
  (:require
   [ftlm.vehicles.art :as art]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]))

(enable-console-print!)

(defonce controls
  (atom
   {:spread 1.5
    :base-color 7.790258368269614
    :speed 2
    :spread-spead 1
    :brownian-factor 0.2
    ;; (/ 1 5)
    }))

(defn ->entity [kind]
  {:kind kind})
;; in ms

(defn age [entity] (- (q/millis) (:spawn-time entity)))
;; assume hsb

(defn shine [entity _dt]
  (update entity
          :color
          (fn [c]
            (mod (+ c (/ (* 1 (@controls :speed)) 2)) 255))))


(defn signum [x]
  (cond
    (< x 0) -1
    (> x 0) 1
    :else 0))

(defn sine-wave [frequency time-in-millis]
  (* (Math/sin (* 2 Math/PI (/ time-in-millis 1000) frequency))))

(defn generate-palette [base num-colors]
  ;; (into [] (map #(mod (+ base (* % (/ 360 num-colors))) 360) (range num-colors)))
  (into [] (map (fn [_] (mod (+ base (rand 50)) 360)) (range num-colors))))

(comment
  (shine {:color 1} 1))

(defn move [{[xv yv] :velocity :as entity}]
  (update-in entity [:transform :pos] (fn [[x y]] [(+ x xv) (+ y yv)])))

(defn friction [{:keys [velocity friction] fr :friction :as entity}]
  (if (or (not velocity) (not friction))
    entity
    (update entity :velocity
            (fn [[xv yv]]
              [(* (signum xv) (max (abs (- (abs xv) fr)) 0))
               (* (signum yv) (max (abs (- (abs yv) fr)) 0))]))))

(defn wobble [{:keys [wobble spawn-time] :as entity}]
  (if-not wobble
    entity
    (update-in
     entity
     [:transform :scale]
     (fn [s]
       (+
        s
        (* wobble (/ (q/sin (* q/TWO-PI (/ (mod (- (q/millis) spawn-time) 1000) 1000))) 30)))))))


(defn brownian-motion [entity]
  (let [brownian-factor (:brownian-factor @controls)]
    (update entity :velocity (fnil (fn [[x y]] [(+ x
                                                   (* brownian-factor (q/random-gaussian)))
                                                (+ y
                                                   (* brownian-factor (q/random-gaussian)))])
                                   [0 0]))))

(defn update-circ [entity]
  (-> entity
      ;; (shine dt)
      move
      wobble
      friction
      brownian-motion))

(comment
  (update {:foo []} :foo conj :b :c)
  (update-circ {:color 0}))


(defn ->transform [pos width height scale]
  {:pos pos :width width :height height :scale scale})

(defn rand-circle [{:keys [color-palatte]}]
  (let [{:keys [spread]} @controls
        cx (/ (q/width) 2)
        cy (/ (q/height) 2)
        ;; angle (/ (mod (/ (q/millis) 200) 360) 1)
        radius 20

        px (+ cx (* 50 (* (q/random-gaussian) spread)))
        py (+ cy (* 50 (* (q/random-gaussian) spread)))

        ;; px (+ cx (* radius (q/cos angle)))
        ;; py (+ cy (* radius (q/sin angle)))

        dx (- px cx)
        dy (- py cy)

        dist (/ (Math/sqrt (+ (* dx dx) (* dy dy))) 1.3)
        ux (/ dx dist)
        uy (/ dy dist)
        spread-speed (:spread-spead @controls)]
    {:color (rand-nth color-palatte)
     :spawn-time (q/millis)
     ;; :wobble 1
     :transform
     (->transform
      [px py]
      radius
      radius
      1.5)
     ;; destination
     ;;
     :friction (/ 1 (- 40 (rand 10)))
     :velocity [(* ux spread-speed) (* uy spread-speed)]
     :lifetime (+ 100 (rand 20))}))

(defn random-cirlces [state n]
  (update state :circles (fn [circles] (concat circles (repeatedly n #(rand-circle state))))))

(defn update-lifetime [state]
  (update
   state
   :circles
   (fn [circles]
     (into
      []
      (comp
       (map #(update % :lifetime dec))
       (remove (comp #(< % 1) :lifetime)))
      circles))))

(defn update-the-circles [state]
  (->  state
       (update :circles #(map update-circ %))))

(defn update-state [state]
  (-> state
      update-lifetime
      (update-the-circles)
      (random-cirlces (* 4 (max (sine-wave 2 (q/millis)) 0)))
      (update :t inc)))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  {:color-palatte (generate-palette (-> @controls :base-color) 8)}
  ;; (random-cirlces {:t 0} )
  )

(defn draw-state [state]
  (q/background 255)
  ;; (q/specular 0 0 0)
  (doseq [{:keys [color transform]} (:circles state)]
    (q/fill color 255 255)
    (let [[x y] (:pos transform)
          {:keys [width height scale]} transform]
      (q/ellipse x y (* scale width) (* scale height)))))

(defn sketch [host]
  (q/sketch
   :host host
   :size [600 600]
   :setup setup
   :update update-state
   :draw draw-state
   :features [:keep-on-top]
   :middleware [m/fun-mode]))

(defmethod art/view "foo"
  [opts]
  (sketch (:place opts)))
(comment
  (swap! controls assoc :spread 1)
  (swap! controls assoc :brownian-factor 0.2)
  (swap! controls assoc :spread-spead 1)
  (swap! controls assoc :base-color (rand 360)))
