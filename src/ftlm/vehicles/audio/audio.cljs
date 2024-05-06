(ns ftlm.vehicles.audio.audio)

(def binaural-beat-freq 40)
(defonce ctx (or (js/window.AudioContext.) (js/window.webkitAudioContext.)))
(def slider (js/document.getElementById "frequencyRange"))
(def display (js/document.getElementById "frequencyDisplay"))

(defn ->panner
  [dest pan]
  (let [panner (. ctx createStereoPanner)]
    (set! (.. panner -pan -value) pan)
    (.connect panner dest)
    panner))

(defn ->oscillator
  [dest]
  (let [o (.createOscillator ctx)]
    (set! (.. o -type) "sine")
    (. o (connect dest))
    o))

(defn ->gain
  [dest volume]
  (let [gain (.createGain ctx)
        _ (set! (.. gain -gain -value) volume)
        _ (.connect gain dest)]
    gain))

(defn set-freq [o hz] (set! (.. o -frequency -value) hz) o)

(defn beep!
  [{:keys [pan hz durr volume]}]
  (let [dest (.-destination ctx)
        o (-> (->gain dest volume)
              (->panner pan)
              (->oscillator)
              (set-freq hz))
        _ (. o start)]
    (js/Promise. (fn [resolve reject]
                   (js/setTimeout #(do (. o stop) (resolve))
                                  durr)))))

(comment
  (beep {:pan 0 :hz 150 :durr 500})
  (doseq
      [i (range 2)]
      (beep {:durr 500
             :hz (+ 100 (rand-int 350))
             :pan (- (rand 2) 1)}))
  (let [base 50]
    (doseq [i (take 3 (shuffle (range 10)))]
      (beep
       {:durr 200
        :hz (* 2 i base)
        :pan (- (rand 2) 1)}))))
