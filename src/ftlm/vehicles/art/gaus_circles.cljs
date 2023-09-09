(ns ftlm.vehicles.art.gaus-circles
  (:require
   [ftlm.vehicles.art :as art]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]))

(enable-console-print!)

(defonce controls (atom {:spread 1.5 :speed 2}))

(defn ->entity [kind]
  {:kind kind})

;; assume hsb
(defn shine [entity _dt]
  (update entity
          :color
          (fn [c]
            (mod
             (+
              c
              ;; (/ (q/sin t) 10)
              (* 1 (@controls :speed)))
             255))))

(comment
  (shine {:color 1} 1)
  )

(defn move [{[xv yv] :velocity :as entity}]
  (update-in entity [:transform :pos] (fn [[x y]] [(+ x xv) (+ y yv)])))

(defn wobble [{:keys [wobble] :as entity}]
  (if-not wobble
    entity
    (update-in
     entity
     [:transform :scale]
     (fn [s]
       (+
        s
        (* wobble (abs (q/sin (q/millis)))))))))

(defn update-circ [entity dt]
  (-> entity
      (shine dt)
      move
      wobble))

(comment
  (update {:foo []} :foo conj :b :c)
  (update-circ {:color 0} 1))

(defn ->transform [pos width height scale]
  {:pos pos :width width :height height :scale scale})

(defn rand-circle []
  (let [{:keys [spread]} @controls
        cx (/ (q/width) 2)
        cy (/ (q/height) 2)
        angle (/ (mod (/ (q/millis) 200) 360) -1)
        radius 20

        ;; px (+ cx (* 50 (* (q/random-gaussian) spread)))
        ;; py (+ cy (* 50 (* (q/random-gaussian) spread)))

        px (+ cx (* radius (q/cos angle))) ; calculate the x position using polar coordinates
        py (+ cy (* radius (q/sin angle))) ; calculate the y position using polar coordinates

        dx (- px cx)
        dy (- py cy)

        dist (Math/sqrt (+ (* dx dx) (* dy dy)))
        ux (/ dx dist)
        uy (/ dy dist)
        spread-speed 1]
    {:color (rand 256)
     :wobble 3
     :transform
     (->transform
      [px py]
      radius
      radius
      1)
     :velocity [(* ux spread-speed) (* uy spread-speed)]
     :lifetime (+ 100 (rand 100))}))


(defn random-cirlces [state n]
  (update state :circles (fn [circles] (concat circles (repeatedly n rand-circle)))))

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

(defn update-the-circles [{:keys [dt] :as state}]
  (->  state
       (update :circles #(map (fn [c] (update-circ c dt)) %))
       (update :circles #(map (fn [c] (update-circ c dt)) %))))

(defn update-state [state]
  (-> state
      update-lifetime
      (update-the-circles)
      (random-cirlces (rand 1))
      (update :t inc)))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (random-cirlces {:t 0} (rand 20)))

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
  (swap! controls assoc :spread 2)
  )
