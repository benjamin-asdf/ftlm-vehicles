;; =========================================
;; What do frequencies do in the brain?
;; =========================================

;;

;; Vehicle 1:
;; There is only 1 sensor, so no chance for sync activity
;; I think here, the frequency then doesn't matter much
;;


;; Vehicle 2:
;; When both sensors are active there is a new meaning - go forward for instance.

;; Cortex Vehicles / Cell assemblies:
;; There is use for the system to have 2 separate operations:
;; 1. juxtoposition of ideas, analogous to 'mere' association, while keeping the ideas separate
;; 2. binding of ideas, expressing a derived information state
;;


;; From considering the cell assemblies and hebbian learning, it is obvious that
;; synchronous activation is in principle sufficient to represent a bind in this way.
;; Consider a neuronal unit can be tuned such that it listens to 2 signals ariving at the same time.
;; Which is allowed to be a separate idea than the mere juxtaposition of 2 ideas.
;; (a derived signal, a bound signal).
;;

;; Observations:

;; === alpha ===
;; every (/ 1 10) seconds, 100ms, 10hz (in a range)
;;
;; - happens in occipital lobe
;; - roughly the frequency I feel like I can still 'parse' the input
;; - drowsiness and early sleep

;; === beta ===
;; 2x, 3x, .. of alpha (in a range)
;;
;; - 'normal' cogntion
;; - dreaming
;;

;; === gamma ===
;; 2x, 3x, .. of beta (in a range)
;; fastest
;;

;; === theta ===
;; slow
;;
;; - hippocampus activity does that
;;

;; === delta ===
;; very slow
;;
;; - deep sleep
;; - hippocampus <-> cortex during stage 3 sleep
;; [https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3327976/#:~:text=Hippocampal%20activity%20seems%20to%20specifically,the%20pre-existing%20cortical%20networks.]
;; - presumably some evolutionary driver said slower is better, since the same thing happens for rodents in theta freq.
;;

;; Considerations
;;
;; - There are many potential reasons, but it is striking that axonal transmission speed is being maximized evolutionarily
;; - i.e. there is some strong driver to make axon transmission fast.
;; - one reason to do this is if synchronous activation is a fundamental aspect of the circuitry
;; (meaning its part of how this thing gets its job done low level).


;; Considering the brain as an activation managing device (activation that is representing information),
;; it is obvious that there is use for very, very slow activity in parts of the system;
;; Imagine you have neuronal units all representing some information states, with short term memory
;; comming out of the logic of the units activating each other.
;; Now consider some early animal (or vehicle) where we have a population of neurons that have a 2x,3x,4x slower activity.

;; --- Evolutionary driver for mid-term memory ---
;; - first, activity goes back and forth in your growing midterm memory module.
;; - you have a lot of neurons with a lot of activity (that persists...) going back and forth in your midterm memory module.
;; - separate your cortex from this neuron population, to manage the activation, else you have an epilepsy problem.
;; - Also, you have an evolutionary driver to make them slower, since the primary function of this activity is a storage mechanism.

;; -> you end up with a (cortical) module that has a lot of neuronal units, a lot of activity, a slow rhythm, that is relatively isolated
;; from the rest of the cortex (because of all the acivity).
;; Maybe it is sort of connected to everything else because everything else makes contributions to what is useful for midterm memory.


;; A key insight here is that once we have figured out for what the system might be using such slow activations,
;; we can go and implement the essential interfaces, cutting our compute by factors of whatever.
;;
;; For instance, if I want to have mid-term memory, *I do not need to simulate the neurons having activity going back and forth*
;;
;; Consider the essential interface of such a module to the cortex.
;;
;; We can imagine the mid term memory module as a pearl maker mechanism, each pearl is allowed to be a frozen information representation.
;; A perl-maker mechanism would look at the activity of the cortex, and decide which pearls to make next
;;  (probably has to do with something like comming up with a coherent story line and a strong concept of a 'situation' and 'place').
;; If the activity of the cortex looks like now would be a good time to supplant with some stored memory, you take one or multiple pearls,
;; and re-instantiate some of the frozen information states.
;; (perhaps this comes out of the logic of association and plasticity already, if I have some half cognition states representing a vague idea of something
;; that is in midterm memory, the cell assemblies of the midterm memory can now go and fill in the rest of the information states, thereby implementing retrieval).
;;
;; (can all be implemented in terms of something like hypervectors, including mechanisms to find relevant information etc.)
;;
;; Concretely, one idea I have is to project between cell assemblies and hypervectors. Then only store the hypervectors in the memory module.
;;

;; returns a hyper vector
(->hyper cell-assembly)

;; this operation would re-ignite some cell assemblies in the system,

(ignite state hdv)

;; Retrieval etc. you can implement in terms of hypervectors or some mix.
;;
;;

;; Considerations on delta
;; - It looks like some kind of re-normalization of the plasticity,
;;   together with maybe some cell physiological stuff going on that is neccessary for the health of the neurons
;; - consider this: Having a nights sleep over something is taking the sharp edges off the memories.
;;                  Kinda like the opposite of intense rumination or something.
;;                  (the opposite of mental re-hearsal of a piece of info, over and over)
;; - It is interesting that this is disturbed in shizophrenia. (delta waves are abnormal, less delta in pre-frontal).
;; - A potential persepctive: If you would not have the ability to have a goods night sleep, to 'sleep over it'.
;;   If every pain would persist in your psyche. If every knive would keep being sharp and never dull down.
;;   It would then be obvious how this is a serious condition, driving you crazy.
;;
;;

;; Considerations on gamma

;; If one thinks about cell assembly algorithms, it is clear that every now and then, I simply want to
;; make x amount of cycles. Where the amount of time spend is allowed to be *as fast as possible*.
;;
;; We have then use and an evolutionary driver, to implement a mode of neuronal activity that is simply the fastest possible.
;;
;; It is not clear, however, if all gamma activity has this underlying rational.
;; It might be useful to have an operation available during normal cognition that allows you to go 2x from some other inforamtion state.
;;
;; So maybe there are at least 2 different reasons to have gamma waves, and one would find 2 different modes of functioning in the brain, too.
;; One that is 'merely' a continuation of the already observed trend, that input states are roughly 2x slower than cognition states (conjecturing on the role of alpha and beta here).
;; Meaning: if beta activation is computing information states based on alpha in a roughly 2x fashion, then maybe gamma waves are computing based on beta (and alpha..) in a 2x/4x fashion.
;;

;; Hunch: There is use for even multiples in the system
;; Something like I squeze 2 beta cycles of derived cognition in 1 alpha cycle of input comming in.
;; So there is a counterintuitive conclusion here: the system is quicker and more effective in scene-understanding by having a relatively slow input rhythm,
;; And perhaps it is the interplay betwee alpha and beta that is tuned to be optimized for cognitive load or something.
;;
;; Still, there is use for having a range of freq. available(?)
;;
;; Hunch 2:
;; Basically, the sync. activation opens a whole dimension for the system to put information together infinitely fine grained.
;; Consider that we can observe a scene of 100s of objects.
;; The hunch would be that if object recognition binding happens via sync activation, then you don't have the problem of overalapping
;; too many meanings. (Because the chance for 2 random sync activations to overlap randomly would be too small?).
;;
;; The hunch here is that simply synchronized activity represents a symbol (virtually a gensym), you bind meaning by making synchronous activiation with this symbol.
;; This would then be somehow be part of how the way we do object recognition (or 'scene parsing'), too.
;; During the course of scene parsing, we would allocate symbols of meaning.
;; One important thing to note here is what Dennett called 'immanence illusion'. This is a top down pretentious idea in a way, that we understand a scene immediately.
;; Turns out that this is completely not the case. So there is a representation of 'parsed scene' in the mind, without much information content that the system can draw from later.
;; ((See Nancy Kishwasher and collaborators work on this))
;;
;;
;;
;; Synesthesia -
;;             If bind is implemented in the brain with synchronous activation, and bind is also the operation that we use to say what color what object has,
;;             then I would submit we would see synchronous activation of say number representing activity with color representing activity in people with synesthesia.
;;             Possibly giving us clues about the puzzle of binding in cognitive machines and the brain's implementation of it.
;;             -> I would submit that we would find neurons that are labeling numbers with colors in the ventral stream.
;;             -> my main thinking right now is that this part of perception is allowed to be feed forward feature detectors (Rosenblatt)
;;
;;
;; Where's Waldo? - it would be interesting in the space of object recognition and binding to know the brain scan results of showing somebody Where's Waldo pictures.
;;                  Actually EEG is interesting, too. If my hunches are correct, you might see suddenly more spread out eeg signals - more symbols.
;;                  On the other hand, maybe there is so much different pieces of information flying around in the system that 50 little objects don't contribute much.
;;                  (maybe you have 50 objects in a normal scene all the time anyway).
;;
;;
;; Eye movements - Consider being blind for a moment during a sacade. Aftewards, new activation flows from thalamic input nuclei into the cortex.
;;                 Maybe this already is sufficient to allocate the synchronization symbols I am thinking of.
;;
;;                 Lets pretend for a moment that:
;;
;;                 1. The thalamic alpha frequency is consistently the same
;;                 2. During eye saccades, there is 0 input comming from the thalamus
;;
;;                 It would go like this: You look at one part of a scene, it comes in with alpha frequency.
;;                 Now run your cognition with this activity as input, the system cannot help but have that go in sync with the input (input activity is flow and driver)
;;                 This is semi automatic, you simply have to run your cognition in sync with the currently incomming alpha freq.
;;                 (In reality I suppose, you make a further trick by going in 2x with the input, so you get to more derived cognition states roughly double as fast.)
;;
;;                 So now you have some cognition states going in the input frequency of the object you are lookig at (~= short term memory).
;;
;;
;;    This fires every 100ms
;;    [ activity-representing-cognition-about-obj-1 ]  <-------+
;;     in sync                                                 |
;;                                                             |
;;                                                          alpha freq inputs
;;                                                 [ X, 0, 0, 0, X, 0, 0, 0, _, _, _, _, _, X, 0, 0, 0, ...] (fire, nothing, nothing,.. fire,) (every 100ms)
;;                                                                         <eye-saccade>         |           (note that the time spend during an eye saccade is allowed to be random)
;;                                                                                               v
;;                                                                                    [ activity-representing-cognition-about-obj-2 ]  (in alpha freq. for sake of argument)
;;                                                                               [ activity-representing-cognition-about-obj-1]     |  (in alpha freq. too)
;;                                                                                                                            |     |
;;                                                                                                                            |     |
;;                                                                                                                            +--+--+
;;                                                                                                                               |
;;                                                                                                              it is this offset that is a clue to the brain
;;                                                                                                              that these are 2 objects.
;;                 ---
;;                 Now an eye sacade, supressing thalamic inputs.
;;                 ---
;;                 Look at the next object at the scene and repeat; We are building now a second tower of derived cognition states, in the same frequency but out of sync with the first.
;;                 The phase offset corresponds to the modulo the durration of the eye saccade with the period of the alpha frequency.
;;                 Somebody can do the math, depending on how strict the system is about synchronization activation, in other words how close 2 inputs need to come in to count as sync.
;;                 How many symbols you can allocate this way without noise. Presumably this is a lot.
;;
;;                 If this is somewhat true, you might wonder if there are internal eye saccades, too. If there is something that supresses an otherwise orderly incomming 'internal input'
;;                 stream, its a candidate.
;;
;;
;;                 The interpretation most at hand is:
;;
;;                 - ventral stream is doing object recognition with the current input states of the point of sharpest vision
;;                 - a second system in parietal cortex is keeping track of a) where you are looking right now and b) an inventory of the already parsed objects.
;;                 - The eye saccade trick gives us non-overlapping sync. activation for each object we look at
;;                 - This opens the door to wonder what happens when the eyes move across different spots of the same object? Apparently the system is capable of mapping incoming input to existing
;;                   objects in this 'inventory', too.
;;                 - one fun move the parietal could be doing now is biasing the system heavily to make another eye saccade just in the right timing to sync up with an already seen object.
;;                   -> this would be joyfully low-tech
;;                 - Another idea is the same thing but circuit wise - parietal cotext -> thalamus,
;;                   inhibiting in just the right amount to sync with an already existing object
;;                 -> Are there illusions where you exidentally see 1 object as 2 objects? Or 2 objects as 1 object?
;;                 -> To the user (the mind), it seems like this parsing works so well.
;;                 -> one edge of the scene parsing failing is when you are surprised a person coming around a corner, there is a flip where the prediction states of the system suddenly all
;;                    need to update - presumably this surprise mechanism means activating input thalamus a lot so you get a lot of real-world states comming in.
;;
;;                ----------------------------
;;                 - (These ideas center around the main idea that thalamus input activity is driving the phase and frequency of the cortex, might or might not be the case).
;;                   Or it is the case to some degree but there is more circuits involved.
;;                 - it is tempting to speculate on what a derived thalamic nucleus like pulvinar migth be doing in this context, maybe part of the mechanisms that keep around symbols / identities for objects in a scene
;;                   go through this?
;;
;;                ---------------------------
;;                 - Or consider it like this: The inventory in the parietal is completely fine with keeping track of aspects of an object comming in different frequencies and phases.
;;                   (maybe I was too fixated on the sync. activation idea)
;;                 -> because with the eye saccade-phase-trick we have solved like half of the problem, maybe this is kinda good enough to solve the hardest aspect of object binding.
;;                 -> maybe parietal can now use ordinary assembly calculus (neuron cell assemblies firing) to represent objects and locations
;;                    And the information states are enough to solve the ghostly symbol binding problem
;;




;; Candidate bind mechanisms using cell assemblies
;; ------------------------------------------------

;; Consider you have a neuronal area with some thousands of neurons
;; Consider, too that you have tight inhibition regulating the number of active units
;; (what Braitenberg called a thought pump)
;;
;; --- Digressing on thought pumps and memetic substrates ---
;; Thought pumps could also be called 'inhibition models' but I think the term is so much more
;; beatifully speculative, dearing. And an intuition-pump itself (Dennett).
;; (note that intuition-pumps are not to be confused with thought-pumps).
;; Intuition-pumps are cognitive level. Roughly meaning: you know them yourself.
;; Thought-pumps are substrate level. Roughly: You have no idea how they work. (do you know how you remember somehting?).
;;
;; The uses for thought-pumps are many, and it would make sense to come in multiple flavors.
;; The most fundamental issue that a thought pump is solving is that of too much activity - epilepsy
;; Funnily enough, we can conceptualize the activity now as a meme substrate.
;; - assuming that the functioning of the brain is basically implemented in activity (of pyramidal cells), for which there are good reasons (see elsewhere).
;; The most primitive (like a point unit or something) meme is the one that simply says 'more activation'.
;; It is easy to see why the opposite meme would simply die out quickly.
;;
;; Thought pumps are a class of mechanism that create a competitive environemnt for memes.
;; If you constrain the kinds of memes that make your thoughts, you are thinking only the most useful thoughts.
;; The simplest thought pump would be one that simply shits on everybody evenly. Only clever activation will survive, actication that is useful to the system,
;; presumably because it is representing useful activation states.
;;
;; Santosh Vampala uses an extremely simple thought pump (inhibition model), a cap-k algorithm that simply takes the top (k) neurons of an area and silences the rest.

;; If we consider the brain to be an activation managing engine, we see that the concpet of inhibition is not some equivalent element to excitation or something.
;; (It can do that, too, but there is other use for inhibition, too).
;; It would be like saying gasoline and the engine are equivalent elements in a car engine.
;; The inhibition allows the system to shape and manage its activation, this is an essential problem space for such a machine.
;;

;; +----------+
;; | X        |
;; |  X       |
;; |    X   X |
;; | X        |
;; |        X |
;; +----------+
;;  neuronal area with some active units

;; [ meaning-condensor ]

;; A meaning condensor would be a mechanism that drives down the activation to some kind of minimal active units.
;; In a second step, we can increase the activation again, now with the units (more) in sync.
;; In terms of cap-k, we can simply decrease k a little each time step.
;; One could make the math or the experiment to see that some kind of kernel activity will form (I am almost certain).
;; I.e. you will get some remaining neurons that are especially well posed, for this brain at this time, to re-ignite the cell assembly.
;; Depending on some of the properties of this mechanism this would looks slightly different. Whether you go smooth or jerky etc.
;; In our theoretically prestine implementation, we can consider what happens in cap-k goes down to 1.
;; If the meaning of an assembly - or of multiple assemblies - is condensend down to a single neuron, transiently, then
;; increasing the activity back up again will be in sync with that 'kernel activity'.
;;
;; Here is a similar idea:
;;
;; Consider how you want some amount of noice toleration for sync. activity, because you will never achieve full synchronicity.
;; It is a variable in the system, how precisely 2 inputs have to be in sync in order to count sync.
;; If you are too lose with this, you lose the benefit. And everything becomes a blur of meaning (useless noise).
;; If you are too strict, the system cannot sucessfully do a binding operation.
;;
;; Since there are 2 opposing concerns here, it would be aesthetically pleasing if there are 2 different kinds of elements in the system,
;; emphasing each side of the coin.
;;
;;
;; Lets imagine we start evolving a special population of neurons which have more lose temporal constraints on what counts as synchronicity.
;; If I have cell assemblies with neurons inside this population, it will create overlapping assemblies, representing 2 or more of its inputs.
;; Once a overlappng assembly is formed, we can drive down the activity everywhere else but the sync-population, call it the sync-maker nuclues.
;; It would then be useful to separate the 2 anatomically (cortex and the sync-maker nucleus), so that the biological mechanism is allowed to evolve more forcefully.
;; Say, it could drive down the activity in the cortex (or an area) to extinguish the activity (assembly). Then the re-ignition by the sync-maker would
;; produce 2 or more cell assemblies, going in sync this time.
;; Effectively the overlapping assembly in the sync-maker then would represent the bound information, because of the second step in the mechanism.
;; Another way to see this is activity flowing like bottle across time, if I make a tiny hole through which the information (activation) flows through in time,
;; then on the other side of this hole the activity automatically is in sync, because it was ignited from a single unit.
;;
;; If synchronicity is an important computational mechanism in the system, then we expect strong evolutionary drivers for whatever mechanisms are producing synchronous activity.
;;
;; Here is another idea from another perspective:
;; Lets say we construct the system such that synchronous activity is the default.
;; We might be able to achieve this by carefully gating the inputs, to make them go in a specified rhythm (it is tempting to attribute such concerns to thalamus->primary-sensory-areas circuitry).
;; From this idea then, you might wonder what are the internal inputs that drive beta activity in the rest of the brain. It makes you wonder whether a beta rhythm is generated
;; from the anterior thalamus, then; But this is a small digression.
;;
;; If all the inputs are synchronous, so all activity initially is synchronous, then it is the job of the system to derive differentially synchronous activity from the input.
;;
;;

;; -------------------------------------------
;; What a cognitive machine can do with a bind operation
;; ------------------------------------------

;; If I have some neurons in my neuronal areas that represent 'this other guy' (this other part of the system), for instance by being neurons that
;; project (potentially indirectly) to the other guy.
;; Then I can bind some information with the idea of 'the other guy' in the system.
;; From one perspective you might say we are 'addressing' this part of the system, then.
;; (although the concept of addressing is already our interpretation again. The system simply binds 2 pieces of information).
;; To be clear, the activity in sync at this moment is all in the first area. It is that a sup population of neurons that mean nothing else but
;; being projections to the other area, is active together with the information representing neurons.

;; This way the rest of the system, if they care (meaning they have neurons that are efferents of area 1), can listen to the concept of area 1 having some
;; information bound to another guy.
;; In particular, that other guy would maybe be interested in this kind of information.
;;

;;
;; Lets be more concrete, imagine there is a nuclues with the meaning 'remember stuff'.
;; Lets call this nuclues the remember-place
;; Say we have some cell assemblies representing information piece A.
;;

(bind A remember-place)

;; If I have a bind operation available I can simply bind information A with remember-guy.
;; So everybody in the system, including remember-place know that this is my meaning.
;; This would be really useful now, if remember-place then goes and does something like remembering A.
;; Later, I can do the same thing with another nulcues, calling it retrieval-place.
;; The beauty of a datastructure like cell assemblies is that we can have incomplete info,

(bind random-noise retrieval-place)

(bind half-A retrieval-place)

;; Now retrieval-place place can look at the bound concept "half a and retrieval-place".
;;


;; just visuals:

;; What a challange it would be to craft some kind of (visual) input, that would work against
;; whatever is broken with migraines.
;; The existence of brain stem auras however, seems to suggest some deep (neurophysiological..) causes
;; Deep with the double meaning of 'below cortex' and 'somewhere fundamentally in the circuitry'.
;;
;;
;; - why do auras blink in alpha frequency?
;; - from considering the C shape of an aura around the boundary between where the periphery kinda starts...
;;   ... you might wonder if the broken activation somehow is in a ring across v1



(lib/append-ents
 mind-world
 [ ;; (merge
  ;;  (lib/->entity :circle)
  ;;  {:color (:cyan controls/color-map)
  ;;   :hidden? false
  ;;   :kinetic-energy 0.2
  ;;   :no-stroke? true
  ;;   :on-update-map
  ;;   {:fade-just-to-mess-with-people
  ;;    (lib/->fade-pulse
  ;;     (lib/normal-distr 0.5 1))}
  ;;   :particle? true :transform (lib/->transform
  ;;               (lib/rand-on-canvas-gauss
  ;;                0.2)
  ;;               30
  ;;               30
  ;;               1)
  ;;   :z-index 10})



  (merge (lib/->entity :circle)
         {:color (:cyan controls/color-map)
          :hidden? false
          ;; :kinetic-energy 0.2
          :no-stroke? true
          :on-update-map {:tick-tock
                          (lib/every-n-seconds
                           ;; (/ 1 20)
                           (/ 1 10)
                           (fn [e _ _]
                             (update e :hidden? not)))}
          ;; {:fade-just-to-mess-with-people
          ;;  (lib/->fade-pulse
          ;;   (/ 1 20))}
          ;; :particle? true
          :transform (lib/->transform
                      (lib/rand-on-canvas-gauss 0.2)
                      30
                      30
                      4)
          :z-index 10})])
