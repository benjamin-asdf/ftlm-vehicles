;;--------------------------------------------------------------------------------
;; THE HIGHER VEHICLES
;;--------------------------------------------------------------------------------
;;
;; Braitenberg stopped at vehicle 14. Vehicle 14 is impressive in many ways.
;; The most impressive thing is how we can go forth and imagine further.
;; More cake, more capabilities and hey - it looks awefully like a mind!
;; Solving all the philosophical conundrums with common sense and /toys/.
;;
;;
;;-------------------------------------------------------------------------
;; I am after TOY MODELS OF COGNITION (for starters)
;; -----------------------------------------------------------------------
;;
;; A complete model of cognition will make our minds look trivial.
;; Including hitherto ellusive concepts like dejavu, tip-of-tounge, System-1 vs System-2 thinking,
;; the nature of feeling and intuition, how children start speaking, etc.
;;
;;
;;
;; To be precise on the relationship between the brain and models of cognition.
;; There are 2 concerns: brain science and cybernetic psychology.
;; The former is concerned with the actual detailed functioning of the (human) brain.
;; While the latter is concerned with the space of possible machines that implement cognition,
;; and elegant programatic descriptions thereof.
;;
;; Presumably it is possible to implement such programs on current, classical computers,
;; using ordinary programming languages.
;; Such a computer program would be a machine intelligence.
;;
;; These 2 intellectual concerns then can be seen as 2 perspectives on the same coin - a model of cognition.
;; It is interesting to observe, that the cybernetician computer programmer (sometimes called a hacker) has the freedom
;; to disregard, or even openly contradict, biological theories of brain function.
;;
;; On the other hand, it is the immense malleability and fluidity of brain intelligence (sometimes called common sense or simply /reasoning/),
;; together with its groundedness in the world, that is the final frontier in AI.
;;
;; For this reason the project of looking at the brain, formulating coherent, deep, imaginative models of the nature of its computations,
;; the mechanisms that the biology of the brain is exploiting to produce the focused yet divergent, imaginative yet grounded mental content
;; we call cognition.
;;
;; In other words all the god stuff is from the 40's-70's. This together with modern brain science clues of what the brain is doing will help us
;; understand the deepest and most ellusive open questions around what the mind is.
;;
;; There is much to think in the land from where the artifical neuron (McCulloch-Pitts 1940-something) came from.
;;
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
;;
;; IV. Cortex
;;
;; I am in the camp of people assuming that the interesting part of cognition happens in the cortex.
;; In other words, it looks like modeling the function of the cortex is a path toowards a model of cognition.
;;
;;
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
;; ------------------------------------
;; The primitives of cognition
;; ----------------------------------
;; Alternative names for a story like like this:
;; - narrative unit
;; - or why not: meme
;; - or perhaps meme stack
;; - perception stack
;; - cognition seq
;;
;; -> either way it is a nice move to take these stacks/seqs of cognition states and take them
;;    as the fundamental unit of the next level of our explanation of cogntion.
;; -> we can think in terms of resources that produce, manipulate, replicate, analyse these narrative units
;; -> some properties of narrative units:
;;    - content is allowed to be vague
;;    - content is not bound to a time line
;;    - You can express 'timespans' compressed:
;;      [ a, then-some-stuff, b ]
;;      You don't need to represent all the drops falling from a sink to express
;;      'drops falling from the sink for a while'
;;    - narrative units can include internal actuations, consider:
;;      [ vague:i-think-of-food, vague:i-remember-where-food-is,  vague:i-go-to-food, concrete:food ]
;;      Such a story line is allowed to be part of the mechanism of thought and memory. The rest of the brain can now do its best to fill in the vague states.
;;
;;    - narrative units can arbitrarily abstract in hierachies and temporal hierachies:
;;      [ situation-a (goes on for half a day), situation-b (goes on for 2 hours), ... ]
;;
;;    - They can also overlap and layer, things are allowed to go on at the same time
;;     [ abstract-situation-a,               abstract-situation-a,                keeps-going-on, ...  ]
;;     [ fine-grainend-abstract-situation-1, fine-grainend-abstract-situation-2  ] <- meaning can totally depend on the larger situation
;;
;;
;; Braitenberg has similar ideas, the primitive he was thinking of he called 'cell assemblies' (Hebb).
;; These cell assemblies will come in handy. They are 'controled activation'.
;; I firmly put them on the substance layer, not the cognition layer.
;; I.e. when we implement cognition we are allowed to program a program that uses cell assemblies as a datastructure.
;; We are allowed to be somewhat inspired by the kinds of things the datastructure can do, but in principle we adhere to the abstraction barrier.
;; Perhaps one of the first things you would do is define an amorphous narrative unit as above.
;; You have to move up the layers somehow.
;;
;;
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
;; Maybe there is a reason why some (cheaper?) magic tricks incoorparate loud bangs.
;; Maybe there is a reason why hypnotists incoorparate snappping.
;;
;; A magician can make a sound roughly like ball hitting floor and the user will observe, literally see, perhaps a ball hitting the floor.
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
;; and the vehicle becomes a part of the world.
;;
;; 17b: internal actuators:
;; -> imagination is a action of a kind
;;
;; - throwing one out there: If all that fits, then maybe the neuronal layer in the cortext that has motor output
;; also has outputs to hippocampus for 'remember something'
;; also has outputs to parietal and temporal cortex 'imagine something'
;; (maybe those go through the Thalamus, as a relay).
;; It is very useful to the system to make certain internal perceptions. This way it can communicate to itself via
;; perception states.
;; If you accidentally mark those as 'real' states, that would be a mechanism for hallucination.
;; Hallucination is imagination going wrong.
;; The halluzinations of an LCD trip are slightly different variety, I believe.
;; If LCD is pumping the blue arm (so that one is associated with serotonin?) then you would say 'the self is gone'.
;; Because your mind is only about the perception and imagination stuff. And all the red cognition is gone.
;; (The stuff that puts you as an agent with capabilities in the world).
;;
;; 17c: Just some ideas on the functionings of the hippocampus
;;
;;
;; ----
;;
;; tip of the tongue idea:
;;
;; 1. Narrative unit saying "I remember some stuff"
;;    i.e. the system goes and predicts itself / fills in the blanks of remembering something
;; 2. Usually your memory system would deliver and fill in the blanks, which comes to you as 'memory', used for speech.
;; 3. If memory fails for this or that reason to fill in the blanks, the system tries harder by spending more
;;    time to sharpen pieces of the ongoing narrative unit.
;; 4. Part of the narrative is that you produce speech from the remembered states,
;;    sharpening and predicting yourself more intensely to use the tongue muscles to produce the remembered states.
;; 5. Perhaps the system tries to compensate for the missing memory by making the whole mind more about the entire narrative unit.
;;    This doesn't help if the memory resources are failing.
;;    But it will make you feel like you are totally predicting yourself and your tounge to produce the speech any moment.
;;
;;
;;
;;    Prediction stack / narrative unit / a story line about a memory event
;;
;;     +---------------+-----------------------------+
;;     |               |                             |
;;     |               |                             |
;;     +---------------+-----------------------------+
;;      "I remember something"           "I speak the remembered stuff"
;;     - vague state                   - initially vague,
;;     - usually is filled in by       - a prediction about what we do next
;;       memory resources              - tounge muscles are implicated
;;
;;
;;
;;
;; -> From considering this I think
;; a) internal predictions and internal actuations as a cenceptual framework seem to carry us really far up in cognition machinery
;; b) The exisitence of tip-of-the-tounge seems to tell us that step 3 is probably not stupid.
;;    It looks more like 3 is the normal way memory works, and it is just in special circumstances where something else
;;    fails that we feel ourselves predicting ourselves wrongly.
;;
;;
;;
;; Vehicle 18:
;;
;; A vehicle with a story to tell
;;
;; Vehicle 19:
;;
;; Something something Broca
;;
;; Something something Wernicke
;;
;;
;; Vehicle 20:
;;
;; Vehicles that need to do coorparation to achieve things.
;;
;; --------------------------------------------------------------------------------------------------------------
;; Like a magic trick we look back and we think, where did cognition come from?
;; It somehow sneaked up on us the whole time. And suddenly we have this rich system that is constantly about itself.
;; Analyzing the situations in ever more finegrained details. Wondering about itself in the world,
;; Dreaming, being subject to pathologies and capable of substance abuse and psychodelic trips.
;; It is superstitious, it is surprised, it enjoys magic tricks, it has friends.
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
;; | building materials of cognition  |------+---------------- vehicle 7, vehicle 11, 8
;; +---------------------------+      |                     rhythm, thought pumps (12),
;;                                    |                     cell assemblies, hypervectors, datastructures, artifial neurons
;;                                    | vehilce 11
;; -- the world of cognition --    ---+ vehicle 14        <- what is the cortex doing?
;;                                 ---+
;; perception                         | vehicle 8,11,15,...
;; action, reach                      | vehicle 17
;; imagination                        | vehicle 15,16,17,...
;; fine grained communication         | vehicle 16, 18?     vehicles that have stories to tell
;; the self                           | vehicle 17
;; internal actuators                 |
;; the mind                           | vehicle 16,
;; higher goals                       | (what kind of vehicle do I want to be?)
;; orchestration                      |
;; social cognition                   |
;;                                    |
;;                                    |
;; +------------------------------+   |
;; |                         ???  |   | <--- toolbox of intelligence?
;; +------------------------------+   |
;;                                    | Forsesight      (candidate machine intelligence mechanisms)
;; -- the world of intelligence --    |
;; specialization                     |
;; resource allocation                |                 <- how does a childs mind learn to use the brain?
;; curiosity                          |                 <- what happens during sleep?
;; technology                         |
;;                                    |
;;                                    |
;;                                    |
;; explanation perfusers?             |
;;                                    |
;;                                    |
;;                                    | Harmony
;;                                    | Vague Programmer
;;                                    | Cakemaker
;;                                 ---+ Soul (alternative names: Curious Hacker, Slowly waking up Computer)
;;
;; -- the world of civilization --      technology?
;;
;;
;; The spirit of vehicle 7 (Concepts, Mnemotrix) and 8 (Space, Things and Movements)
;; and 11 (Rules and Regularities, Ergotrix) is to observe what do neurons do,
;; and then use reasoning from the realm of psychology, from top-down, what does the system need,
;; what could be things that the neurons are providing?
;; Once we know some things the neurons are doing, we can make higher order abstractions that
;; represent such things, I call it the building materials of cognition (formely the toolbox of cognition)
;; Nowhere it is written down that artificial neurons are the best level of abstraction to make
;; cognition.
;;
;;
;; I am less interested in scaling vehicle 5 (Artificial neurons) into cognition.
;; I am interested in thinking about what is the stuff that you need to make cognition.
;; To find a toolbox of cognition that is higher level than the neurons.
;; This way we come up with Mnemotrix (m-lines), Ergotrix (e-lines), vague and concrete states (vehicle 15), maps,
;; predictor-comparators (vehicle 13).
;; Thought pumps (vehicle 12), event flow assemblers (vehicle 16?).
;;
;; Note: Thought pumps, also called putting inhibition as a mechanism into your model instead of hoping it evolves from
;; neuronal net, is one of the main pieces of Santosh Vempala's "cell assemblies".
;; https://www.youtube.com/watch?v=mSX9CCKdBDA
;; I am a big fan of the cell assemblies.
;; I discovered them after writing most of this doc. They go firmly into exactly the spot I call
;; "cognition building material" here.
;; The properties of cell assemblies are very similar to hypervectors.
;; But maybe the path between brain science and them is clearer since its neurons so you can map the entities 1:1.
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
