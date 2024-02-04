(ns ftlm.vehicles.art.vehicles.taste
  (:require [clojure.walk :as walk]
            [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
            [ftlm.vehicles.art :as art]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [ftlm.vehicles.art.controls :as controls :refer [versions]]
            [ftlm.vehicles.art.user-controls :as user-controls]
            [goog.style]))

;;--------------------------------------------------------------------------------
;; THE HIGHER VEHICLES
;;--------------------------------------------------------------------------------

;; ===
;; I am after toy models of cognition (for starters)
;; -----------------------------------------------------------------------
;;
;; I. "Bacteria brain", "tiny, tiny brain" (vehicle 1)
;; sensor -> motor
;;
;; II. "Nematode brain", "very small brain" (vehicle 2...5, 6, ... )
;;
;; sensor -> [ a few interneurons ] -> motor
;;
;; III. Insect -> frog brain, "small brain" (vehicle 8, ... 13/14, ... )
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
;;     |                   |        distant futures (vehicle 14, vehicle 12) (maybe minimally for a frog)
;;     |                   |        thought pumps (vehicle 12)
;;     |                   |
;;     +-------------------+
;;       prediction loops etc. ?
;;
;; There is probably not much cognition going on in a frog brain, considering there is no cortex.
;; The number of neurons in the tectum is roughly the number of fibers entering the tectum (Braitenberg 1986).
;; So there is probably not much reflexivity happening.
;;
;; IV. Cortex
;;
;; I am in the camp of people assuming that the interesting part of cognition happens in the cortex.
;; In other words, it looks like modeling the function of the cortex will give me a model of cognition.
;;
;; Givens:
;;
;; 0. Injuring cortex makes a person lose specific capabilities.
;; 1. The cortex is the thing that explosively blew up in human evolution.
;;    1a. What ever is special about us is almost certainly special because of the cortex.
;; 2. Thalamus has 10^8 neurons; this is an upper limit for cortical inputs.
;; 3. Cortex neurons have 10^10 inputs.
;;    -> this means that 10x maybe event 100x more connections to the cortex are /from/ the cortex. (Braitenberg 1986)
;;    The stuff that cognition is is mostly stuff _about the mind itself_ (reflexive).
;;    No wonder hallucinations are a thing.
;; 5. The cortex is more generic than other parts of the brain, it looks as if evolution
;;    found some basic building block (cortical columns?) which when duplicated,
;;    made more useful intelligence in the animal.
;;    -> both the given that the cortex is generic/uniform and the given that areas are different is interesting.
;; 6. To explain cognition means to also explain altered states, out of body experiences, dreaming,
;;    mental pathologies, etc. (Metzinger).
;; 7. Everything with a brain also sleeps afaik. Roughly half of a good model of what the brain does
;;    concerns itself with stuff that happens during sleep.
;;    (we will see that this move makes us able to happily to move this or that part of a cognition mechanism
;;    into a dream phase).
;; 8. The mind is self-assembled, without an external loss function.
;; 9. The mind is online and has resource constraints.
;;    Unlike a computer, there is generally not a pause when you ask a person something.
;;    The mind cannot stop reality in order to process some input.
;;    (But the existence of attentional blink shows us that a tradeoff in this space is being made).
;; 10. Whatever the basic building block of the cortex, it grows in in the first 2 dimensions but not the height.
;;    Otherwise we would have evolved more cortex volume, not surface area (Braitenberg ...)
;;
;; --------------------
;; Reasonings / Intuitions:
;;
;; The cortext is a information mixing machine - Braitenberg
;;
;; What I call explanation juggle balls somebody might call the 'puzzle pieces' of a problem.
;; A explanation juggle ball can be pulled out, and considered, together with another such ball.
;; Or balls, if one is already juggleling some, and capable of putting multiple at once in their head.
;;
;; - When making explanations about the mind and brain, you cannot know the whole picture all
;;    at once.
;; - You have to put single juggle balls in your mind, and only juggle 1 or 2 at the time
;;    See Rich Hickey Hammock Driven Development.
;; - When thinking about the cortex:
;;   a. We are allowed to use the rest of the system as explanation. The cortex is about itself.
;;      -> The explanation is allowed to go in a loop.
;;   b. We are allowed to use an abstraction barrier, and explain a part (a juggle ball) in terms of stuff
;;      to be figured out somewhere else. (this too is just a programming insight.)
;;   For this reason I will every now and then come up with this or that magic substance,
;;   'Imagine we had the magic substance xyz...'
;;    Braitenberg did this when he cam up with Mnemotrix and Ergotrix.
;; - The cortex is like an ocean. This is part of the point.
;;   One does not go an and explain the ocean by looking at all the drops of water.
;;   The better way to understand the ocean is to consider a drop of water, while not forgetting
;;   the ocean.
;;
;;
;; ==================================== CORTEX ==============================================
;; +----------------------------------------------------------------------------------------+
;; |                                                                                        |
;; |             Cortex                                                                  <--+-----+
;; |                                                                                        |     |
;; |                   ---------------------------------------------------------------------+     | mostly about itself
;; |                                                                                        |     |
;; |                                                                                        |     |
;; |                                                                                        |     |
;; |                                                                                        |     |
;; |   -------------------------------------------------------------------------------      |     |
;; |                                                                                        |     |
;; |    inputs                                       |                                   ---+-----+
;; |    ^   ^                                        | motor output                         |
;; |    |   |                                        |                                      |
;; +====|===|========================================|=======================================+----+
;;  |   |   |          |                             |                                       |    |-
;;  +------------------+                             v                                       +----+
;;                  rest of the brain                 actuators                                 eyes
;;
;; Why is there a big fat cake of stuff that is about itself on top of the brain?
;; And it seems to have to have to do with what we call cognition and intelligence.
;;
;; To explain the cortex is to explain an ocean.
;; The most beatiful way to explain an ocean is to consider a drop of water, without loosing sight of the ocean.
;; -> Hence the vehicles and the small toy ideas about how things _could_ work.
;; Once we see a substance that makes sense to grow for a vehilce, if this substance looks like you can grow
;; more of it and have it turn out nice, just from its wiring.
;;
;; ---------------------------------------------------------------------------------------------
;; 1. Excitatory activation pyramidal cells looks like the main biological function of the cortex
;; 2. Short term memory looks like cell activation going back and forth.
;; 3. Everything fits with the idea that cells in hippocampus go back and forth for longer,
;;    producing mid-term memory (future blog posts)
;; 4. Vague ideas / mixed ideas / ideas with a little bit of concreteness and else vaguenss,
;;    I feel like this is a nice building material out of which to make cognition out of.
;;    -----------------------------------------------------------------
;;    The concept of confabulation and 'filling in the blanks':
;;    -----------------------------------------------------------------
;;    This is very important for the kind of mechanisms I want to build.
;;
;;    (also similarly called hallucination hypothesis, predictive coding: https://en.wikipedia.org/wiki/Predictive_coding)
;;    Similar: Dennetts multiple drafts.
;;    Many people had such intutions before me.
;;    What I bring to the table is being able to program, and doing so sensually, with a sense of aesthetics.
;;    If you think about it, this would be really great aspect to emphasize for the creators of machine intelligence.
;;
;;
;; V. Vehicle 15
;; CONFABULATION
;;
;; predictor-compartor with confabulation (future blog post)
;;
;; 15a.
;; From the comparator, we get surprise.
;; If you are in surprise, you do a double take, you make your short term memory
;; be about the real past.
;;
;; Key idea: predictors are allowed to modify the short term memory too, in order to get more `harmony`.
;;
;; a predictor harmony loop
;; re-arrange, confabulate, modify predictions, until a stop condition is met.
;; The stop condition could come from a comparator
;;
;; (defn predictor-loop [predictor stop? input-mental-states]
;;   (loop [mental-states input-mental-states]
;;     (or
;;      (stop? mental-states)
;;      (recur (predictor mental-states)))))
;;
;; (defn predictor-comparator-harmony-loop [comparator predictor input-mental-states]
;;   (let [trusted-states (trusted input-mental-states)]
;;     (predictor-loop
;;      predictor
;;      (fn [prediction-mental-states]
;;        (cond
;;          (enough-harmony?
;;           (comparator trusted-states prediction-mental-states))
;;          prediction-mental-states
;;          (give-up? prediction-mental-states)
;;          [:unresolved input-mental-states prediction-mental-states]
;;          :else nil)))))
;;
;; enough-harmony? Could be guided by a hyperparameter. (dopamine?)
;;
;; Lets say there are some real states comming from short term memory.
;; The predictor is adding some prediction states at the end.
;; This then might be a little bit of a self-contained story snippet.
;; I call this a story-line henceforth.
;;
;;
;;     +-----------------+-----------------------+
;;     |                 |                       | story line
;;     +-----------------+-----------------------+
;;      real states        prediction states
;;                         thought / dream
;;      (purple)           (blue)
;;
;;
;; Since we have 2 kinds of mental states in this system, lets give them colors.
;; Tint everything touched by the realistic brain a bit more purple. And everything by the
;; predictor / perception brain as blue.
;;
;; These colors come from the colors I chose to draw this on the whiteboard.
;;
;; 15b:
;; If we are still surprised after a double take (our system was not able to make sense of
;; the short term memory) - we mark the state as unresolved.
;; Imagining for a moment we already have mid-term memory.
;; Alternatively, we can reverbarate those unresolved states very frequently in the system,
;; to make sure we don't forget them.
;; -> sounds a bit like a default mode network for this vehicle to me.
;; This vehicle might be somewhat neurotic, going back to some memory it can't make sense of over and over.
;;
;; In a sleep phase, we pick some unresolved, and try different predictors. Maybe some novel,
;; 'fresh perspective' will allow the vehicle to understand the situation it was facing.
;;
;; VI. Vehicle 16
;;
;; 16a: dimensions of input, event streams, higher order predictors
;;
;; We can start to swap our language away from "prediction states" to "perception" for the blue snippets.
;; Higher order story lines are now driving, in part, the predictions of the lower level ones.
;;
;; We might even start tinting some of the furthest derived resources of this growing layer of cake green,
;; for we are firmly crossing the threshold into imagination and day dreaming now.
;; Not merely blue anymore, not merely perception.
;;
;; This vehicle starts having the capacity for a LCD trip, to be in awe about its input;
;; (double takes).
;; It dreams up potential interpretations and sees its perceptions from different angles.
;;
;;
;; 16b: The mental arranger
;;
;; We already got the same thing from the higher order predictors.
;;
;; Lets imagine a resource with a bigger picture of the ongoings in this system in the vehicle.
;; One might imagine flying across the vast city of predictors we have created.
;; We can draw each predictor, or each dimension (not sure yet) on the abscissa of a coordinate system
;; on the ordinate there is something that is roughly time, but not actually time.
;; I call this the event-flow. It is the ordering of the stories in something like time,
;; but since it is up to the vehicle to make up the mental stories, this is more like some rough best guess attempt.
;;
;; The resulting picture I call mind mosaik, because you see these color full snippets being arranged there now.
;; And they are mostly rectangles, spreading over different amounts of event flow.
;;
;;
;;                               "synchronization barrier"
;;
;;                               |  - say you see a ball falling
;;                               |  - ball on floor event
;;                               |  - you expect to not hear the ball falling afterwards
;;                               |  - the ball-hit-floor event is a sync barrier
;;                               |
;;  p-streams                    |
;;                               |
;;          +--------------------+-----------------------+
;;          | +----------+       |                       |                --+
;; ---->    | | story1   |       |        +--------------+----+             | Situations
;;          +-+----------+-------+--------+--------------+    |             | (areas in the mind mosaik)
;;          |      +-----------+ |        |              |    |             |
;; ---->    |      | story2    | |        |              |    |             |
;;          +------+-----------+-+--------+--------------+    |             |
;;          |                    |        |              |    |             |
;; ---->    |                    |        |              |    |             |
;;          +--------------------+--------+--------------+    |             |
;;          |                    |        +--------------+----+             |
;;          |                    |                       |               ---+
;;          +---X-----X-----X----+-----------------------+
;;          |  / \   / \   / \   |                       |
;; ---->    | /   \ /   \ /   \  |                       |  ? time givers ?  -> see interlude 3: The breath
;;          +/-----X-----X-----\-+-----------------------+
;;                               +                          - a rhythmic event stream for instance
;;                           ^   |           ^              - heartbeat, breathing, walking movements
;;                           |               |              - day cycle, hormone cycles (cortisol), ...
;;                           |               |              - time givers allow the system to order events
;;            event flow     |               |              - You can also generate an ever changing random event stream,
;;                           |               |              - as anchors for the rest of the system to locate in time
;;                                                          - this is similar to positional encoding in machine learning
;;                           e1              e2             - in fact, putting in a positional encoder into our vehicles
;;                                                            sounds useful
;;
;;
;;   Such a vehicle can see a ball falling and produce a vague story line:
;;
;;  +-----------+-----------------+
;;  |           |                 |
;;  +-----------+-----------------+
;;   e1                          e2
;; ball-started-falling , random-noise + ball-falling + ongoing... , ball-on-floor
;;
;; The cool thing is that the mind arranger can bully the auditory system into hearing a ball-on-floor event.
;; In other words the auditory sytem can be biased towawrds hearing such a sound.
;; The ball-on-floor-event is a moment in time for which some ongoings are allowed to before, and some after, and some at the same time.
;; A synchronization barrier in programming is a construct that /ensures that some ongoings are done at a particular moment in the timelines/.
;; We need this only if there is more than 1 timeline. If there is only 1, then everything happens in order (called synchronously).
;; If things are allowed to happen at the same time, called concurrently, that is called having mutliple timelines.
;; -> One way to coordinate timelines is with sync barries. So certain ongoings are guaranteed to be over on both timelines.
;;
;; If our vehicle has an auditory system with some story timelines and a visual system with some story timeliness, then a synchronization barrier
;; might be useful, after hearing the ball fall, I don't expect it to see falling still. If I would perceive these things out of order, I
;; would think something is seriously wrong.
;; (are drugs and pathologies where ordering is off?)
;;
;;
;; Maybe here is a reason why some (cheaper?) magic tricks incoorparate loud bangs.
;; Maybe here is a reason why hypnotists incoorparate snappping.
;;
;;
;; A magician can make a sound roughly like ball hitting floor and the user will observe, literally see perhaps a ball hitting the floor.
;;
;;
;; One can now wonder about different shapes of stories and events.
;; Steven Pinker (2007), points some of this out to us: (see reading)
;;

;; Vehicle 17:
;; Red cognition
;; actuation
;; The predictor - actuator loop of power:
;; - I predict I have an interesting effect the world
;; - I percieve my effects in the world
;; - If I can have interesting effects, that is power, and worth developing.
;;
;; I sort of see these 2 arms of cognition then bootstrapping this thing into higher
;; and higher cognition.
;; The blue arm (perception) driving ever more sophisticated predictions, including the action of the vehicle itself,
;; The red arm (actuation) driving the ever more sophisticated plans and models and capabilities  - the reach.
;; And then the 'self', the thing the vehicle starts percieving as the doer, both in the world and its internal world.
;;
;; The world comes into the mind of the vehicle,
;; and the vehicle becomes a part of the world
;;
;; 17b: internal actuators:
;; -> imagination is a action of a kind
;;
;; - throwing one out there: If all that fits, then maybe the neuronal layer in the cortext that has motor output
;; also has outputs to hippocampus for 'remember something'
;; also has outputs to parietal and temporal cortex 'imagine something'
;;
;; These things would be red of a kind.
;;
;; 17c: Just some ideas on the functionings of the hippocampus
;;
;;
;; Vehicle 18:
;;
;; A vehicle with a story to tell
;;
;;
;; ...
;;
;; Like a magic trick we look back and we think, where did cognition come from?
;; It somehow sneaked up on us and suddenly we have this rich system that is constantly about itself.
;; Analyzing the situations in ever more finegrained details. Wondering about itself in the world,
;; Dreaming, being subject to pathologies and capable of substance abuse and psychodelic trips.
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
;; -> Plans and multiple drafts model, a recipie can be powerful, for
;; it can be revised.
;;
;;
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
;; 4. Consider the Von Neuman Machine, the program is dynamic in the computer
;; The existence of dynamic content is power.
;; 5. Maybe Ideas? Memes? are the dynamic content out of which the mind
;; is made out of.
;; Dynamism is what makes ideas a great building material.
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
;; +---------------------------+      |                 rhythm, thought pumps (12)
;;                                    | vehilce 11
;; -- the world of cognition --    ---+ vehicle 14        <- what is the cortex doing?
;;                                 ---+
;; perception                         | vehicle 8,11,15,...
;; action, reach                      | vehicle 17
;; imagination                        | vehicle 15,16,17,...
;; fine grained communication         | vehicle 16, 18?          vehicles that have stories to tell
;; the self                           | vehicle 17
;; internal actuators                 |
;; the mind                           | vehicle 16,
;; higher goals                       | (what kind of vehicle do I want to be?)
;; orchestration                      |
;;                                    |
;;                                    | Forsesight      (candidate machine intelligence mechanisms)
;; -- the world of intelligence --    |
;; specialization                     |
;;                                    |
;; Hofstadter essences finding?       |
;; explanation perfusers?             |
;;                                    |
;;                                    | Harmony
;;                                    | Vague Programmer
;;                                    | Cakemaker
;;                                 ---+ Soul
;;
;; -- the world of civilization --      technology?
;;
;;
;; The spirit of vehicle 7 (Concepts, Mnemotrix) and 8 (Space, Things and Movements)
;; and 11 (Rules and Regularities, Ergotrix) is to observe what do neurons do,
;; and then use reasoning from the realm of psychology, from top-down, what does the system need,
;; what could be things that the neurons are providing?
;; Once we know some things the neurons are doing, we can make higher order abstractions that
;; represent such things, I call it the toolbox of cognition.
;; Nowhere it is written down that artificial neurons are the best level of abstraction to make
;; cognition.
;;
;;
;; I less interested in scaling vehicle 5 (Artificial neurons) into cognition.
;; I am interested in thinking about what is the stuff that you need to make cognition.
;; To find a toolbox of cognition that is higher level than the neurons.
;; This way we come up with Mnemotrix (m-lines), Ergotrix (e-lines), vague and concrete states (vehicle 15), maps,
;; predictor-comparators (vehicle 13).
;; Thought pumps (vehicle 12), event flow assemblers (vehicle 16?).
;;
;;
;; Vehicle 15:
;; My idea for vehicle 15 is that deriving from vehicle 14, we allow the predictors to modify the short term memory states.
;;
;;
;; 15a: The predictor is allowed to modify the short term memory, and thereby make the system go in harmony
;; With this move, we enable confabulation.
;;
;; 15b: Allow the comparator to be relaxed about which predictor states fit.
;;
;; There is a kind of hyperparameter in the system, the comparator now has a choice, how important are realistic states?
;; In the limit we get the plain vehicle 14, it is strict about the predictor states fitting the realistic states,
;; but the perception of this vehicle is less perfused with meaning.
;; In the other extreme, you would not pay attention to reality at all anymore, you would simply percieve everything
;; the way you dream it up.
;; My hunch is that dopamine in part serves the role of setting this balance in humans.
;; From this mechanistic model, it looks as if a cocain trip could be described such:
;; /Everything clicks into place/.
;; /Everything is they way I imagine it to be/.
;;
;; ---
;; Do magic tricks work better on high dopmaine people?
;; Do autists have a different perception of the blind spot? Maybe there is some meassure of how hard it is
;; to reveal the blind spot that would be some meassure of this balance in the system.
;;
;;
;;       how much you trust your imagination (confubulation without reality check)
;;
;;        <------------------------------------------>
;;
;;          autistic                    cocain trip
;;
;; <-- vehicle 14 is way over to this side.
;;
;;
;; ---
;; This vagueness move is very important in my thinking. I think the possibility of vague ideas is key for cognition.
;; Where the rest of the system is allowed to fill in the blanks.
;; Also: multiple drafts model (Dennett), tale-tale brain, the existence of magic tricks, the way the mind fill in
;; the blind spot, the story confabulations of split brain persons, many things that point to the same underlying way
;; of how it works, I think.
;;
;; Confabulation seems to be intertwined with the nature cognition.
;; One cannot separate cognition and perception with solid lines. The way perception is self-assembled in the first place
;; is via these vague prediciton / blank filling mechanisms.
;;
;; No wonder that..
;; Psychosis exists, rather than being evolved away, you cannot evolve away some fundamental functioning of the mind.
;;
;; I discuss my pet theory candidate of psychosis in vehicle 17(? future).
;; (although there are many possible etiologies of psychosis in such a system).
;; There is something that needs to detect  /x happenend because of me/ - and that is in overdrive for instance.
;; The rest of the mind will fill in the blanks - Even if doing so means creating absurd believes.
;;
;; ---
;;
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
  (let [modality (rand-nth [:rays :smell :temperature])
        sensor-left-opts {:anchor :top-left
                          :modality modality
                          :shuffle-anchor? (#{:smell} modality)}
        sensor-left-opts (merge sensor-left-opts
                                (when (= modality :smell)
                                  {:fragrance (rand-nth [:oxygen
                                                         :organic-matter])})
                                (when (= modality :temperature)
                                  {:hot-or-cold (rand-nth [:hot :cold])}))
        sensor-right-opts (assoc sensor-left-opts :anchor :top-right)
        decussates? (rand-nth [true false])
        sensor-left-id (random-uuid)
        sensor-right-id (random-uuid)
        transduction-fn (rand-nth [:excite :inhibit])]
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
       (into
         [[:cart/motor :motor-left
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
           {:on-update [(lib/->baseline-arousal (or baseline-arousal 0.8))]}]
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
         ;; (->love-wires :motor-left :motor-right {:modality :smell
         ;; :fragrance :oxygen})
         (mapcat identity
           (repeatedly
             sensor-pair-count
             (fn [] (->rand-sensor-pair-plans :motor-right :motor-left)))))}))

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
         (->>
          [:multi-sensory :multi-sensory :multi-sensory]
          ;; [:multi-sensory]
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
