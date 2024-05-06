(ns ftlm.vehicles.audio
  (:require [ftlm.vehicles.audio.audio :as impl]))

(defn ->beep
  [{:keys [pan hz durr volume]}]
  {:durr (or durr 200)
   :hz (or hz 400)
   :pan (or pan 0)
   :volume (or volume 1)})

(def beep! impl/beep!)
