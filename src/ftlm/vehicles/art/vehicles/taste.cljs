(ns ftlm.vehicles.art.vehicles.taste
  (:require [clojure.walk :as walk]
            [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
            [ftlm.vehicles.art :as art]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [ftlm.vehicles.art.controls :as controls :refer [versions]]
            [ftlm.vehicles.art.user-controls :as user-controls]
            [goog.style]))



;; === Vehicle 3c.  ===
;; multi sensory
;; 1. make a simple olfaction impl
;; 2. make areas of higher whatever things that smell
;; 3. temperature bubbles
;; 4. random vehicle 3.
;; so that it can love temp or be aggressive towards light etc.

;; === Interlude 1: === (maybe)
;; assemble connections

;; === Interlude 2: ===
;; - interneurons -> visible in inspection window
;; - another small kind of effect (another actuator): change the color of the vehicle
;; - Now we make interneurons that change the weight of what
;; each of your sensors are contributing
;; - We can do this randomly just for proof of concept (they would be engineered by evolution, vehicle 6)
;; - Call these 'mood' neurons
;; - Wire them also to the color of the vehicle
;; - Now, depending on mood the color changes,
;; and the vehicle might now love light and be red
;; and then fear light and be green etc.
;;
;; -> we have created dynamism where before there were static versions
;; -> it is like taking a stack of paper of possible vehicle configs.
;; Then stacking this paper on top of each other in a thick block.
;; The dynamism here allows the system to move in a new dimension along the thickness
;; of this stack of paper. (I like how there are new dimensions of behaviour available to the system now)
;;
;; -> consider genetic regulation of cells, a dynamic version of different versions of cells
;; -> we are tackling here a little bit both the hierachical and the dyanmic part.
;;
;; The colors are just so we have a more striking effect.
;; In reality, mood is signaled via many (subtle..) body actuators
;; - smell (sweat), pupil size, body posture, blood in the skin (visual), etc.
;;

;; === vehicle 4 ===
;; non-linear activation functions (transduction functions)
;; and abrupt ones -> `will`

;; === Interlude 3: ===
;; The Breath
;; Question how to hook up your mood neurons to something so you can
;; abruptly decide to do something else?
;; The inputs, yes.
;; Consider the day night cycle, a periodic sensory input.
;; Good enough to hook in parts of your cognition,
;; and you are complected to time!
;;
;; - lets take vehicle 4, with the mood interneurons, and add a breath
;; - The breath is something like a sine wave
;; - Your mood is allowed to subtly change with in and out breath
;; -> Your cognition has a little bit of time in it!
;;
;; -> compare to deep learning positional encoding
;; -> The Soul is also called the breath. Maybe there is something deep about this.
;; The rythm of life that connects you to the real world.
;; If time is a concept to you, resource constraintnes is allowed to be a concept to you.
;; -> On of the fundamental aspects of cybernetics and intelligence I think.
;;

(defn rand-temperature-bubble
  ([_]
   (rand-temperature-bubble (rand-nth [:hot :cold])))
  ([controls hot-or-cold]
   (let [max-temp 10]
     (merge (hot-or-cold controls)
            {:hot-or-cold hot-or-cold}
            {:d (lib/normal-distr 150 20)
             :max-temp max-temp
             :pos (lib/rand-on-canvas-gauss 0.7)
             :temp (rand-int (inc max-temp))}))))

(defn env [state]
  {:ray-sources
   (into [] (filter :ray-source?) (lib/entities state))
   :odor-sources
   (into [] (filter :odor-source?) (lib/entities state))
   :temperature-bubbles
   (into [] (filter :temperature-bubble?) (lib/entities state))})

(defn ->rand-sensor-pair-plans
  [motor-left motor-right]
  (let [modality (rand-nth [:rays :smell :temperature])
        sensor-left-opts {:anchor :top-left
                          :modality modality
                          :shuffle-anchor? (#{:smell}
                                            modality)}
        sensor-left-opts
          (merge
            sensor-left-opts
            (when (= modality :smell)
              {:activation-shine-colors
                 {:high (:misty-rose controls/color-map)
                  :low (:heliotrope controls/color-map)}
               :fragrance (rand-nth [:oxygen
                                     :organic-matter])})
            (when (= modality :temperature)
              {:hot-or-cold (rand-nth [:hot :cold])}))
        sensor-right-opts (assoc sensor-left-opts
                            :anchor :top-right)
        decussates? (rand-nth [true false])
        sensor-left-id (random-uuid)
        sensor-right-id (random-uuid)
        transduction-fn (rand-nth [:excite :inhibit])]
    (case modality
      :temperature
        [[:cart/sensor sensor-left-id
          (assoc sensor-left-opts
            :anchor :middle-middle
            :activation-shine-colors
              ({:cold {:high {:h 196 :s 26 :v 100}
                       :low controls/white}
                :hot {:high (:hit-pink controls/color-map)
                      :low controls/white}}
               (:hot-or-cold sensor-left-opts)))]
         [:brain/connection :_
          {:bezier-line (lib/rand-bezier 5)
           :destination [:ref motor-left]
           :f transduction-fn
           :source [:ref sensor-left-id]}]
         [:brain/connection :_
          {:bezier-line (lib/rand-bezier 5)
           :destination [:ref motor-right]
           :f transduction-fn
           :source [:ref sensor-left-id]}]]
      [[:cart/sensor sensor-left-id sensor-left-opts]
       [:cart/sensor sensor-right-id sensor-right-opts]
       [:brain/connection :_
        {:bezier-line (lib/rand-bezier 5)
         :destination [:ref motor-left]
         :f transduction-fn
         :source [:ref
                  (if decussates?
                    sensor-right-id
                    sensor-left-id)]}]
       [:brain/connection :_
        {:bezier-line (lib/rand-bezier 5)
         :destination [:ref motor-right]
         :f transduction-fn
         :source [:ref
                  (if decussates?
                    sensor-left-id
                    sensor-right-id)]}]])))

(defn ->love-wires
  [motor-left motor-right sensor-opts]
  (let [sensor-left-opts (merge sensor-opts {:anchor :top-left})
        sensor-right-opts (assoc sensor-left-opts :anchor :top-right)
        sensor-left-id (random-uuid)
        sensor-right-id (random-uuid)
        decussates? false]
    [[:cart/sensor sensor-left-id sensor-left-opts]
     [:cart/sensor sensor-right-id sensor-right-opts]
     [:brain/connection :_
      {:destination [:ref motor-left]
       :f :inhibit
       :source [:ref (if decussates? sensor-right-id sensor-left-id)]}]
     [:brain/connection :_
      {:destination [:ref motor-right]
       :f :inhibit
       :source [:ref (if decussates? sensor-left-id sensor-right-id)]}]]))

(defn random-multi-sensory
  [sensor-pair-count]
  (fn [{:as opts :keys [baseline-arousal]}]
    {:body (merge opts
                  {:color-of-the-mind
                     (rand-nth [:cyan :hit-pink
                                :navajo-white :sweet-pink
                                :woodsmoke :mint
                                :midnight-purple])})
     :components
       (into [[:cart/motor :motor-left
               {:activation-shine-colors
                  {:high (:misty-rose controls/color-map)}
                :activation-shine-speed 0.5
                :anchor :bottom-left
                :corner-r 5
                :on-update [(lib/->cap-activation)]
                :rotational-power 0.02}]
              [:cart/motor :motor-right
               {:activation-shine-colors
                  {:high (:misty-rose controls/color-map)}
                :activation-shine-speed 0.5
                :anchor :bottom-right
                :corner-r 5
                :on-update [(lib/->cap-activation)]
                :rotational-power 0.02}]
              [:brain/neuron :arousal
               {:activation-shine true
                :activation-shine-colors
                  {:high (:red controls/color-map)}
                :nucleus :arousal
                :on-update [(lib/->baseline-arousal
                              (or baseline-arousal 0.8))]}]
              [:brain/connection :_
               {:destination [:ref :motor-left]
                :f rand
                :hidden? true
                :source [:ref :arousal]}]
              [:brain/connection :_
               {:destination [:ref :motor-right]
                :f rand
                :hidden? true
                :source [:ref :arousal]}]]
             ;; (->love-wires :motor-left :motor-right
             ;; {:modality :smell
             ;; :fragrance :oxygen})
             (mapcat identity
               (repeatedly sensor-pair-count
                           (fn []
                             (->rand-sensor-pair-plans
                               :motor-right
                               :motor-left)))))}))

(def body-plans
  {:multi-sensory (random-multi-sensory 6)})

(defn shuffle-anchor [{:keys [shuffle-anchor?] :as e}]
  (if-not shuffle-anchor?
    e
    (let [[x y] (lib/anchor->trans-matrix (:anchor e))
          anch-pos
          [(lib/normal-distr x 0.2)
           (lib/normal-distr y 0.12)]]
      (assoc e :anchor-position anch-pos))))

(def builders
  {:brain/connection (comp lib/->connection
                           #(walk/prewalk-replace
                              {:excite lib/excite
                               :inhibit lib/inhibit}
                              %))
   :brain/neuron lib/->neuron
   :cart/body
     (fn [opts]
       (lib/->body
         (merge
           {:color (:sweet-pink controls/color-map)
            :corner-r 10
            :darts? true
            :draggable? true
            :on-update-map
              {:indicator
                 (lib/every-n-seconds
                   1
                   (fn [e s _]
                     (if (= (:id e) (:id (:selection s)))
                       (assoc e
                         :stroke-weight 4
                         :stroke (:amethyst-smoke
                                   controls/color-map))
                       (dissoc e :stroke-weight :stroke))))}
            :pos (lib/rand-on-canvas-gauss 0.3)
            :rot (* (rand) q/TWO-PI)
            :scale 1}
           opts)))
   :cart/motor lib/->motor
   :cart/sensor (comp shuffle-anchor lib/->sensor)})

(defmulti build-entity first)

(defmethod build-entity :default [[kind opts]] ((builders kind) opts))

(defn ref? [v] (and (sequential? v) (= (first v) :ref)))

;; only have maps 1 deep right now

(defn resolve-refs
  [temp-id->ent form]
  (update-vals form
               (fn [v]
                 (if (ref? v)
                   (or (temp-id->ent (second v))
                       (throw (js/Error. (str (second v) " is not resolved"))))
                   v))))

(defn ->cart
  [{:keys [body components]}]
  (let [body (build-entity [:cart/body body])
        {:keys [comps]}
        (reduce (fn [{:keys [comps temp-id->ent]} [kind temp-id opts]]
                  (let [entity (build-entity [kind
                                              (resolve-refs temp-id->ent opts)])]
                    {:comps (into comps
                                  (if (map? entity)
                                    [entity]
                                    entity))
                     :temp-id->ent (if (= temp-id :_)
                                     temp-id->ent
                                     (assoc temp-id->ent temp-id entity))}))
                {:comps []
                 :temp-id->ent {}}
                components)]
    (into [(assoc body :components (into [] (map :id) comps))]
          comps)))

(defn ->ray-source [opts]
  (lib/->ray-source
   (assoc opts
          :shinyness false
          :color controls/white)))

(defmethod lib/event! ::spawn
  [{:keys [what]} {:as state :keys [controls]}]
  (lib/append-ents state
                   (->cart ((body-plans what) (controls :what)))))

(defn draw-state
  [state]
  (q/background (lib/->hsb (-> state :controls :background-color)))
  (q/stroke-weight 1)
  (q/stroke 0.3)
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

(def the-state (atom {}))

(defn update-state
  [state]
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
              (lib/update-ents #(update-entity % state (env state)))
              lib/transduce-signals
              lib/track-components
              lib/track-conn-lines
              lib/ray-source-collision-burst
              lib/kill-entities))]
    (reset! the-state state)
    state))

(defn some-rand-environment-things
  [controls n]
  (let [stuff (repeatedly n
                          #(rand-nth [:temp-cold :temp-hot
                                      :organic-matter
                                      :oxygen]))
        ->make
          {:organic-matter
             (fn []
               (lib/->organic-matter
                 {:odor {:decay-rate 2 :intensity 40}
                  :pos (lib/rand-on-canvas-gauss 0.5)}))
           :oxygen (fn []
                     (lib/->oxygen
                       {:odor {:decay-rate 2 :intensity 40}
                        :pos (lib/rand-on-canvas-gauss
                               0.2)}))
           :temp-cold (fn []
                        (lib/->temperature-bubble-1
                          (rand-temperature-bubble controls
                                                   :cold)))
           :temp-hot (fn []
                       (lib/->temperature-bubble-1
                         (rand-temperature-bubble controls
                                                  :hot)))}]
    (mapcat (fn [op] (op)) (map ->make stuff))))

(defn setup
  [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (q/background (lib/->hsb (-> controls
                               :background-color)))
  (let [state {:controls controls
               :on-update
               [(lib/every-n-seconds
                 1
                 (fn [state]
                   (let [sources (filter :ray-source?
                                         (lib/entities state))]
                     (if (< (count sources) 2)
                       (lib/append-ents
                        state
                        (->ray-source
                         {:intensity (+ 5 (rand 30))
                          :pos
                          (lib/rand-on-canvas-gauss
                           (controls
                            :ray-source-spread))
                          :scale (controls
                                  :ray-source-scale)
                          :z-index 10}))
                       state))))]
               :nuclei
               {:arousal {:pos [500 300]}}}
        state
        (-> state
            (lib/append-ents
             (->> [:multi-sensory
                   ;; :multi-sensory
                   ;; :multi-sensory
                   ]
                  (sequence
                   (comp
                    (map (juxt identity controls))
                    (mapcat
                     (fn [[kind
                           {:as opts :keys [amount]}]]
                       (repeatedly amount
                                   #((body-plans kind)
                                     opts))))
                    (map ->cart)
                    cat))))
            (lib/append-ents
             (some-rand-environment-things controls 6)))]
    (assoc state
           :selection {:id (first (map :id
                                       (filter :body?
                                               (lib/entities state))))
                       :time (q/millis)})))


(defn sketch
  [host {:keys [width height]} controls]
  (let [[screen-width screen-height] (lib/window-dimensions)
        width (cond (= width "max") screen-width width width :else screen-width)
        height (cond (= height "max") screen-height height height :else screen-height)
        width 1000
        height 800]
    (q/sketch :host host
              :size [width height]
              :setup (partial setup controls)
              :update update-state
              :draw draw-state
              :features [:keep-on-top]
              :middleware [m/fun-mode]
              :mouse-pressed lib/mouse-pressed
              :mouse-released lib/mouse-released
              :mouse-wheel lib/mouse-wheel
              :frame-rate 30)))

(defn from-left [amount]
  (- (q/width) amount))

(defn from-bottom [amount]
  (- (q/height) amount))

(defn draw-inspect
  [state]
  (let [selection (:selection state)
        e ((lib/entities-by-id state) (:id selection))]
    (if (:body? e)
      (do
        (q/background
          (lib/->hsb
            (or (controls/color-map (e :color-of-the-mind))
                (lib/->hsb (-> state
                               :controls
                               :background-color)))))
        ;; draw all the sensors of the thing on the
        ;; top left in a grid
        (let [components (map (lib/entities-by-id state)
                           (:components
                             ((lib/entities-by-id state)
                               (:id selection))))]
          (let [sensors (filter :sensor? components)]
            (lib/draw-entities-1
              (for [[row sensor-row]
                      (map-indexed vector
                                   (partition-all 3
                                                  sensors))
                    [col sensor] (map-indexed vector
                                              sensor-row)]
                (assoc-in (merge sensor
                                 {:kind :rect
                                  :size [40 40]
                                  :stroke
                                    (:very-blue
                                      controls/color-map)
                                  :stroke-weight 2})
                  [:transform :pos]
                  [(+ 40 (* col 25)) (+ 40 (* row 25))]))))
          (let [actuators (filter :actuator? components)]
            (lib/draw-entities-1
              (for [[row actuators-row]
                      (map-indexed
                        vector
                        (partition-all 3 actuators))
                    [col actuator]
                      (map-indexed vector actuators-row)]
                (assoc-in (merge actuator
                                 {:corner-r 0
                                  :kind :rect
                                  :stroke
                                    (:red
                                      controls/color-map)
                                  :stroke-weight 2})
                  [:transform :pos]
                  [(from-left (+ 200 (* col 25)))
                   (from-bottom (+ 100 (* row 25)))]))))
          (when (:nuclei state)
            (let [nuclei (group-by :nucleus
                                   (filter :nucleus
                                     components))]
              (lib/draw-entities-1
                (for [[nucleus neurons] nuclei
                      neuron neurons]
                  (let [nucleus-pos (get-in state
                                            [:nuclei nucleus
                                             :pos])]
                    (-> (dissoc neuron :hidden?)
                        (merge {:kind :rect
                                :stroke controls/white})
                        (assoc :transform (lib/->transform
                                            nucleus-pos
                                            20
                                            20
                                            1))))))))))
      (q/background (lib/->hsb (-> state
                                   :controls
                                   :background-color))))))


(defn update-inspect [state]
  @the-state)

(defn setup-inspect [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (q/background (lib/->hsb (-> controls :background-color))))

(defn sketch-inspect
  [host controls]
  (q/sketch :host host
            :size [1000 400]
            :setup (partial setup controls)
            :update #'update-inspect
            :draw #'draw-inspect
            :features [:keep-on-top]
            :middleware [m/fun-mode]
            ;; :mouse-pressed mouse-pressed
            ;; :mouse-released mouse-released
            ;; :mouse-wheel mouse-wheel
            :frame-rate 10))

(defonce restart-fn (atom nil))
(defmethod art/view "taste"
  [{:as opts :keys [place version]}]
  (let [f (fn []
            (let [controls (merge (controls/default-versions "taste")
                                  (get-in versions ["taste" version])
                                  @user-controls/!app)]
              (sketch place opts controls)
              (sketch-inspect
                (let [e (js/document.getElementById "art-place-2")]
                  (goog.style/setStyle e (clj->js {:margin-top "16px"}))
                  e)
                controls)))]
    (reset! restart-fn f)
    (f)))

(defmethod user-controls/action-button ::restart
  [_]
  (some-> @restart-fn (apply nil)))

(defmethod user-controls/action-button ::spawn [_ what]
  (swap! lib/event-queue conj {:kind ::spawn :what what}))
