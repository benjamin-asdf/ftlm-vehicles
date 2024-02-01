(ns ftlm.vehicles.art.vehicles.taste
  (:require [clojure.walk :as walk]
            [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
            [ftlm.vehicles.art :as art]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [ftlm.vehicles.art.controls :as controls :refer [versions]]
            [ftlm.vehicles.art.user-controls :as user-controls]
            [goog.style]))

;; ===
;; I am after toy models of cognition (for starters)
;; -----------------------------------------------------------------------
;; 1. Everything looks as if cognitioon is what the cortex is doing
;; 2. Thalamus has 10^8 neurons, upper limit for cortical inputs
;; 3. Cortex has 10^10 inputs. This means that at least 10 or maybe 100x more
;; inputs to the cortex are from the cortex. (Braitenberg 1986)
;; 4. The stuff that cognition is is mostly stuff _about the mind itself_ (reflexive)
;; No wonder hallucinations are a thing.
;;
;;
;; I. "Bacteria brain" (vehicle 1)
;; sensor -> motor
;;
;; II. "Insect brain" (vehicle 2...5, 6, ... )
;;
;; sensor -> [ a few interneurons ] -> motor
;;
;; III. "Frog brain" (vehicle 8, ... 13/14, ... )
;;
;;
;;   +   +                      +-+
;;   +-|-+                      | |
;;     |                        +++
;;     | sensors                 | actuators
;;     |                         |
;;     |                         |
;;   realistic brain          dreamer brain (vehicle 14)
;;     |                         |
;; +---+------ tectum? -----------+
;; |                              |
;; |  predictor-comparator(s)     | vehicle 13
;; |                              | association learners (vehicle 7), regularity learners (vehicle 11)
;; +---+-------------------^------+ values (vehicle 6)
;;     |                   |        surprise (vehicle 13)
;;     |                   |        distant futures (vehicle 14, vehicle 12)
;;     |                   |
;;     |                   |
;;     +-------------------+
;;       prediction loops etc.
;;
;;
;; IV. Vehicle 15
;;
;; predictor-compartor with confabulation (future blog post)
;;
;; Key idea: predictors are allowed to modify the short term memory too,
;; in order to get more `harmony`.
;; We get confabulation.
;; -> mechanism for magic tricks
;; -> mechanism for multiple drafts and tale-tale brain
;;
;; .. and then some stuff... vehicle 16, 17, 18... (?), future blog posts.
;;
;; Like a magic trick we look back and we think, where did cognition come from?
;; It somehow sneaked up on us and suddenly we have this rich system that is constantly about itself.
;; Analyzing the situations in ever more finegrained details.
;;
;;


;; ===
;; The mind is...
;; hierachical, dynamic, self-assembled
;; This is similar to "complex adaptive system" but different emphasis.
;;
;; `hierachical`: because abstraction is key.
;; A map can be better than the territory.
;; Building blocks are useful.
;; If you can get more things done in the same amount of steps,
;; that is power.
;;
;; -> Plans and multiple drafts model, a recipie can be power for
;; it can be revised.
;;
;; `dynamic`:
;; 1. navigate ever more complex environments
;; Consider cells, then cells with gene regulation
;; -> stable across more enviroments, because more dynamic.
;; 2. Allow to acrete your content.
;; (plans and intermediate representations again).
;; Cognition is the content of the mind.
;; The way lisp code is the content of a lisp program.
;; 3. Allow to become specialized.
;; Allocate resources, have attention mechanisms etc.
;;
;; `self-assembled`:
;; It needs to bootsrap from nothing.
;; - Its fundamental algorithms need to be simple enough so they can
;; be computed by reality.
;; - It grows without a loss function or fitness function specified from outside
;; - Makes we wonder about local rules that allow useful 'brain' to grow.
;; - And later, the mind is a programmer, discovering the resources of the computer
;; it is running on; And building its resourcefulness.
;; - When thinking about Cognition you are allowed to go in loops
;; - We are allowed to do this, if we have a path from something simple to something more complex.
;;
;; ===


;;
;;
;; ===                             ---+
;;                                    | vehicles 1-5
;; -- the world of the neurons --     |
;;    |    |       |        |         |
;;    v    v       v        v         | vehicle 8
;; +---------------------------+      |
;; | toolbox of cognition      |------+---------------- vehicle 7, vehicle 11, 8
;; +---------------------------+      |
;;                                    | vehilce 11
;; -- the world of cognition --    ---+ vehicle 14
;;                                 ---+
;; perception                         | vehicle 8,11,...
;; action                             |
;; imagination                        | vehicle 15,16,17,...
;;                                    | Forsesight
;; -- the world of intelligence --    |
;;                                    | Vague Programmer
;;                                 ---+
;;
;;
;; The spirit of vehicle 7 (Concepts, Mnemotrix) and 8 (Space, Things and Movements)
;; and 11 (Rules and Regularities, Ergotrix) is to observe what do neurons do,
;; and then use reasoning from the realm of psychology, from top-down, what does the system need,
;; what could be things that the neurons are providing?
;;
;; I less interested in scaling vehicle 5 (Artificial neurons) into cognition.
;; I am interested in thinking about what is the stuff that you need to make cognition.
;; To find a toolbox of cognition that is higher level than the neurons.
;; This way we come up with Mnemotrix (m-lines), Ergotrix (e-lines), vague and concrete states, maps,
;; predictor-comparators (vehicle 13).
;;
;; I have a vague idea of a predictor-comparator-box, that uses m-lines and e-lines to reason,
;; that is allowed to dynamically query the rest of the system for states (modeling A-Stream pyramidal cell inputs).
;;
;;


;; ===

;; === Vehicle 3c.  ===
;; multi sensory
;; 1. make a simple olfaction impl
;; 2. make areas of higher whatever things that smell
;; 3. temperature bubbles
;; 4. random vehicle 3.
;; so that it can love temp or be aggressive towards light etc.


;; === Interlude 1: ===
;; assemble connections

;; === Interlude 2: ===
;; - interneurons -> visible in inspection window
;; - another small kind of effect: change the color of the vehicle
;; - Now we make interneurons that change the weight of what
;; each of your sensors are contributing
;; - We can do this randomly just for proof of concept
;; - Call these 'mood' neurons
;; - Wire them also to the color of the vehicle
;; - Now, depending on mood the color changes,
;; and the vehicle might now love light and be red
;; and then fear light and be green etc.
;; -> we have created dynamism where before there where versions
;;
;; -> it is like taking a stack of paper of possible vehicle configs
;; and dyanmically moving in the third dimension of this stack of paper
;;
;; -> consider genetic regulation of cells, a dynamic version of different versions of cells
;; -> we are tackling here a little bit both the hierachical and the dyanmic part.


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





(defn rand-temperature-bubble [controls]
  (let [hot-or-cold (rand-nth [:hot :cold])
        max-temp 10]
    (merge
     (hot-or-cold controls)
     {:hot-or-cold hot-or-cold}
     {:d (lib/normal-distr 150 20)
      :max-temp max-temp
      :pos (lib/rand-on-canvas-gauss 0.7)
      :temp (rand-int (inc max-temp))})))




(defn env [state]
  {:ray-sources
   (into [] (filter :ray-source?) (lib/entities state))
   :odor-sources
   (into [] (filter :odor-source?) (lib/entities state))
   :temperature-bubbles
   (into [] (filter :temperature-bubble?) (lib/entities state))})

;; --- make rand body plan?

(defn ->rand-sensor-pair-plans
  [motor-left motor-right]
  (let [modality
        (rand-nth [:rays :smell :temperature])
        sensor-left-opts {:anchor :top-left
                          :modality modality
                          :shuffle-anchor? (#{:smell} modality)}
        sensor-left-opts (merge sensor-left-opts
                                (when (= modality :smell)
                                  {:fragrance
                                   (rand-nth [:oxygen
                                              :organic-matter])
                                   })
                                (when (= modality :temperature)
                                  {:hot-or-cold (rand-nth [:hot :cold])}))
        sensor-right-opts (assoc sensor-left-opts :anchor :top-right)
        decussates? (rand-nth [true false])
        sensor-left-id (random-uuid)
        sensor-right-id (random-uuid)
        transduction-fn
        (rand-nth [:excite :inhibit])
        ]
    (case modality
      :temperature [[:cart/sensor sensor-left-id
                     (assoc sensor-left-opts :anchor :middle-middle)]
                    [:brain/connection :_
                     {:destination [:ref motor-left]
                      :f transduction-fn
                      :source [:ref sensor-left-id]}]
                    [:brain/connection :_
                     {:destination [:ref motor-right]
                      :f transduction-fn
                      :source [:ref sensor-left-id]}]]
      [[:cart/sensor sensor-left-id sensor-left-opts]
       [:cart/sensor sensor-right-id sensor-right-opts]
       [:brain/connection :_
        {:destination [:ref motor-left]
         :f transduction-fn
         :source [:ref (if decussates? sensor-right-id sensor-left-id)]}]
       [:brain/connection :_
        {:destination [:ref motor-right]
         :f transduction-fn
         :source [:ref (if decussates? sensor-left-id sensor-right-id)]}]])))

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
    {:body opts
     :components
     (into [[:cart/motor :motor-left
             {:anchor :bottom-left
              :corner-r 5
              :on-update [(lib/->cap-activation)]
              :rotational-power 0.02}]
            [:cart/motor :motor-right
             {:anchor :bottom-right
              :corner-r 5
              :on-update [(lib/->cap-activation)]
              :rotational-power 0.02}]
            [:brain/neuron :arousal
             {:on-update [(lib/->baseline-arousal (or baseline-arousal
                                                      0.8))]}]
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
           ;; (->love-wires :motor-left :motor-right {:modality :smell :fragrance :oxygen})
           (mapcat identity
                   (repeatedly
                    sensor-pair-count
                    (fn []
                      (->rand-sensor-pair-plans :motor-right :motor-left)))))}))

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
  {:brain/connection
   (comp lib/->connection
         #(walk/prewalk-replace {:excite lib/excite :inhibit lib/inhibit} %))
   :brain/neuron
   lib/->neuron
   :cart/body (fn [opts]
                (lib/->body (merge {:color (:sweet-pink controls/color-map)
                                    :corner-r 10
                                    :draggable? true
                                    :darts? true
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
   (assoc opts :shinyness false)))

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

(defn update-entity [entity state]
  (let [env (env state)]
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

(defn setup
  [controls]
  (q/rect-mode :center)
  (q/color-mode :hsb)
  (q/background (lib/->hsb (-> controls :background-color)))
  (let [state {:controls controls
               :on-update
               [(lib/every-n-seconds
                 1
                 (fn [state]
                   (let [sources (filter :ray-source? (lib/entities state))]
                     (if (< (count sources) 3)
                       (lib/append-ents
                        state
                        (->ray-source {:intensity (+ 5 (rand 30))
                                       :pos (lib/rand-on-canvas-gauss
                                             (controls :ray-source-spread))
                                       :scale (controls :ray-source-scale)
                                       :z-index 10}))
                       state))))]}]
    (-> state
        (lib/append-ents
         (->> ;; [:multi-sensory :multi-sensory :multi-sensory]
          [:multi-sensory]
          (sequence
           (comp (map (juxt identity controls))
                 (mapcat (fn [[kind {:as opts :keys [amount]}]]
                           (repeatedly amount #((body-plans kind) opts))))
                 (map ->cart)
                 cat))))
        (lib/append-ents (lib/->organic-matter
                          {:odor {:decay-rate 2 :intensity 40}
                           :pos (lib/rand-on-canvas-gauss 0.5)}))
        (lib/append-ents (lib/->oxygen {:odor {:decay-rate 2 :intensity 40}
                                        :pos (lib/rand-on-canvas-gauss 0.2)}))
        (lib/append-ents (lib/->oxygen {:odor {:decay-rate 2 :intensity 40}
                                        :pos (lib/rand-on-canvas-gauss 0.3)}))
        (lib/append-ents (->ray-source {:intensity 20
                                        :pos (lib/rand-on-canvas-gauss 0.4)
                                        :z-index 10}))
        (lib/append-ents (lib/->temperature-bubble-1 (rand-temperature-bubble
                                                      controls)))
        (lib/append-ents (lib/->temperature-bubble-1 (rand-temperature-bubble
                                                      controls)))
        (lib/append-ents (lib/->temperature-bubble-1 (rand-temperature-bubble
                                                      controls))))))

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
              :mouse-pressed mouse-pressed
              :mouse-released mouse-released
              :mouse-wheel mouse-wheel
              :frame-rate 30)))

(defn draw-inspect
  [state]

  (q/background (lib/->hsb (-> state :controls :background-color)))
  (def state state)
  (when-let [selection (:selection state)]
    (def selection selection)
    (lib/draw-entities-1 [(update-in ((lib/entities-by-id state)
                                       (:id selection))
                                     [:transform :pos]
                                     (constantly [500 200]))])))

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
