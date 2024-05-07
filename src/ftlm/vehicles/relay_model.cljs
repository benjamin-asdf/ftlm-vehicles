;;
;; -------------------------------------------
;; https://pubmed.ncbi.nlm.nih.gov/17707635/
;; The thalamus is more than just a relay (M. Sherman)
;; ---------------------------------------------
;;
;; Sherman SM. The thalamus is more than just a relay. Curr Opin Neurobiol. 2007 Aug;17(4):417-22. doi: 10.1016/j.conb.2007.07.003. Epub 2007 Aug 17. PMID: 17707635; PMCID: PMC2753250.
;;
;;
;;
;;
;; 1. Relay (first order)
;; 2. Gate (TRN)
;; 3. Contrast Neurons (Layer 6 -> thalamus)
;; 4. Motor output, efference copies, latent spaces, the higher order relay nuclei
;;    (Layer 5 -> higher order / motor)
;; -------------------------------------------------------

(ns ftlm.vehicles.relay-model
  (:require [ftlm.vehicles.art.lib :as lib :refer [*dt*]]
            [quil.core :as q :include-macros true]
            [ftlm.vehicles.art.extended :as elib]
            [ftlm.vehicles.art.controls :as controls :refer
             [versions]]
            [ftlm.vehicles.art.user-controls :as
             user-controls]
            [clojure.set :as set]
            [goog.style]
            [ftlm.vehicles.hdv]
            [ftlm.vehicles.audio :as audio]
            [ftlm.vehicles.assembly-calculus :as ac]
            [ftlm.vehicles.art.neuronal-area :as na]
            ["mathjs" :as mathjs]))

(defn ->contrast-model
  [n-relay-wires]
  {:neurons (ac/->neurons n-relay-wires)})

(defn ->relay-model
  [{:keys [n-relay-wires ->sensor-inputs]} n-area]
  {:contrast-model (->contrast-model n-relay-wires)
   :neurons (ac/->neurons n-relay-wires)
   :update (fn [s]
             ;; + sensor inputs
             ;; - contrast inputs
             (assoc s
               :neurons (ac/subtract
                          (or (->sensor-inputs)
                              (ac/->neurons n-relay-wires))
                          (:neurons (:contrast-model
                                      s)))))})

(defn ->target-projection
  [relay n-area]
  (let [target (into []
                      (take (:n-relay-wires relay)
                            (shuffle (filter #(< (mod % 20)
                                                 5)
                                       (range (:n-neurons
                                                n-area))))))
        wire (ac/->targets (:n-relay-wires relay) target)]
    {:wire wire}))

(defn ->relay
  [{:as opts
    :keys [n-relay-wires ->sensor-inputs target-model]}
   n-area]
  (let [relay-model (->relay-model opts n-area)
        e (na/->neurons
            (merge {:->activations
                      (fn [e]
                        (ac/indices-above-input-cutoff
                          (-> e
                              :relay-model
                              :neurons)
                          0))
                    :draw-i (fn [i] (q/ellipse 0 0 8 8))
                    :grid-width 5
                    :relay-model relay-model
                    :spacing 15}
                   opts))
        e (assoc e
            :target (->target-projection e n-area)
            :on-neuron-tick-map
              {:1 (fn [e _ _]
                    (update e
                            :relay-model
                            (fn [m] ((:update m) m))))})]
    e))


(defn target-inputs
  [{:as e :keys [target ->activations]}]
  (ac/target-inputs (:wire target) (->activations e)))
