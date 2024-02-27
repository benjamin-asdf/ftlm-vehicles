;; ============================
;; Some things you can do with fine grainend inhibition
;; ============================






;; Thalamic reticular nucleus is a candidate auto thought-pump
;;
;; Thinking of TRN, the interpretation that is most at hand is that the TRN is sort of gating
;; the input from Thalamus to Cortex - but why, what is the use of this, do you need this in a model of cortical function, etc., etc.?
;;

;; Thought pump idea:
;; (A memetic device).
;; If you have syntactic units (each state sort of saying what is possibly active in the next state, implementable by a graph with hebbian plasticity).
;; We can now have a second part in the system, the 'inhibition system', narrowing down how many units are active.
;; Braitenberg thought pump, vehicle 12.
;; The algorithm is what he called trains-of-thought. You sort of only get the most interesting, 'focused'
;; activation states.
;; (of course activation states represent information, this is the leap that we get from neuroscience and McCulloch).
;;
;; The need for this is obvious, if one considers the memetic problem of too much acvtivity. The basic meme, is the meme that says 'activate everybody'.
;; If this meme is succesfull in a brain, that is an epileptic seizure.
;; We can easily observe that the opposite meme saying "don't acvitate anybody" is not a good meme, it will die quickly.
;; So we see that we need a 'memetic constraint device' in our brain-machine.
;;
;; The simplest model of this is a cap-k algorithm, simply set the count of active units in a given area to k.
;; So only the units that have the most associations to the currently active ones will be active in the next step.
;; There is some math that from this you get a 'datastructure' of self-activating, interconnected k + a few neuronal units called a cell assembly,
;; which represents the input states to the neuronal area.
;; ((see Santosh Vempala))
;;
;; Memetic devices and thought-pumps are a class of algorithm that constrain the space of possible memes in a system,
;; in other words they produce a more highly competetive, difficult environment for memes, allowing only the most succesfull ones to survive.
;;
;; Braitenberg called the memes 'cell assemblies' (similar/same idea, the term in both cases comes from Hebb) and stretched the idea of 'activity with a structure' quite far.
;; Note that cell assemblies are allowed to have temporal structure. So I kinda imagine these activity nets sort of amorphously waxing and waning etc.
;; One leading to the other sometimes, and others more frozen in time (where activity merely goes back and forth between some units).
;; I like to call the temporal structure of such a cell assembly the syntax of the cell assembly.
;; (Because it sort of says the ordering of what is following what).
;;
;; From thinking of stacks of information states, I have somewhere else the idea of a memetic datastructure I call a narrative unit.
;; Since this stuff is made from a substance like cell-assemblies, they are allowed to be vague, so somebody else can fill in the blanks.
;; The 1 primitive operation that such substrate can do is prediction. (Pretty sure you can show this theoretically, but I go and program stuff).
;; This overlaps with the cell assemblies of Braitenberg mostly.
;;


;;                                         +--------+
;;                             +--+        |        |
;;                             |  |        |  X  X  | high activity cortical region
;;         <-------------------+--+--------+-- X    | Assumption: activates some thalamic nuclei
;; +-----------+               |  |        | X  X   | 3. You go back and forth with thalamus, cortex
;; |        ---+---------------+-X+--------+--->    | 4. Since you activate the TRN in between, you inhibit your input
;; |           |               | X|        |        |
;; |           |               |  |        |        |
;; |           |               |  |        |        |
;; |           |               |  |        |        |
;; |      -----+---------------+--+--------+-->     |   1. thalamic nuclei driving cortex activation
;; |           |               | X|        |        |
;; |       |---+---------------+--|        |        |   2. TRN inhibiting back
;; |           |               |  |        |        |
;; +-----------+               |  |        |        |
;;  Thalamic input nuclei      |  |        |        |
;;                             |  |        |        |
;;                             |  |        |        |
;;                             +--+        |        |
;;                             TRN         |        |
;;                                         |        |
;;                                         +--------+
;;                                            Cortex

;; Assuming
;; 1. Thalamus input drives cortex activity
;; 2. There is a Cortex <-> Thalamus loop
;;    - so one cortex neuronal area to one thalamic area to same cortex neuronal area
;; 3. You have the TRN in inside that loop, too
;; (not saying that there aren't other circuits, too. But that this would be one).

;; In effect, the TRN is an inhibition map now.
;; What ever region in TRN is active corresponds to a inhibited region in cortex.

;;
;; TRN is now an auto thought-pump here, limiting the inputs that come to regions of activity.
;; In other words, focusing down the kinds of activations you have in that region.
;; If we assume further that we have a geometry in the cortex, where close regions are encoding associated
;; information.
;; You get something similar to a lateral inhibition mechanism from this circuit.
;; Where activating 1 set of cell assemblies in a region will focus down on the ones that most intensly survive in the
;; more comptetive environent.
;; (Still developing my thoughts here).
;; In cognitive neuroscience terms this is an attention mechanism
;; - maybe its the memes that survive without thalamic input that are interesting to the system?
;;   maybe those then represent more derived cognitive states?
;;
;;
;; Here is a little mechanistic story that is one perspective on tip-of-the-tounge:
;;
;; Suppose that you encode words in the cortex geometrically, with words or somehow the association lines leading to
;; some words next to each other in the geometry (same neuronal area).
;; Now the system goes like 'activate this area because that is where I find the word'.
;; The auto thought pump above is focusing down your train of thought. In other words, we focus into which attractor states
;; your information states are falling into.
;;
;;
;; If you activated the wrong units, you have inhibited this whole area, sharply focusing down on the wrong thoughts.
;; Now you have this problem, being 'locked in' too much in certain association lines that don't help you find the word you look for.
;;
;; You went down some wrong memeory lanes, and your train of thought is in a slightly wrong possition now.
;; It seems the only solution to the problem is to stop thinking hard, let the sytem return to non-focused state.
;; From there you can try again.
;;
;; It is sriking, that there is a moment in something like tip-of-the-tounge where you know it is hopeless to keep thinking.
;; It is when you realize that you are too far down in some wrong idea, that you cannot even have another idea right now.
;;
;; (this is not *the* explanation of tip-of-the-tounge, it is just one little puzzle piece where something low-level seems to explain
;; a piece of cognition a little bit).


;; ------------------------------ migraine auras? ------------------------
;;
;; Givens:
;; - migraines
;; - CSD travels at 1.5-9.5mm/min
;; - migraine without aura seems to look from mutliple angles like actually a different problem, (related mabye but different).
;;   (why is bilateral pain more common without aura? Why is migraine without aura more connected to hormonal changes?)
;; - migraine pain is usually one-sided
;;
;;
;; Considerations:
;; - Since it is mono-lateral whatever circuits, nuclei, mechanism etc. are at play probably are mono-lateral, too
;; - What moves as slow as CSD? Nothing in the space of neuronal activity at first sight it seems like.
;;
;;
;;
