(ns ftlm.vehicles.art.vehicles.carts
  (:require [ftlm.vehicles.art.lib :as lib :refer [*dt*]]))

(defn ->body
  [spawn-point scale rot color]
  (merge (lib/->entity :rect)
         {:body? true
          :color color
          :transform (assoc (lib/->transform spawn-point 50 80 scale)
                            :rotation rot)}))

(defn ->cart
  [body sensors motors opts]
  (let [body (merge (assoc body
                      :components (map :id (concat sensors motors))
                      :motors (map :id motors)
                      :sensors (map :id sensors)
                      :darts? true
                      :makes-trail? true)
                    opts)]
    (into [body] (concat sensors motors))))
