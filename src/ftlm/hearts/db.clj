(ns ftlm.hearts.db
  (:require
   [integrant.core :as ig]
   [xtdb.api :as xt]
   [clojure.java.io :as io]
   [ftlm.hearts.system :as sys]))

(defmethod ig/init-key :xtdb/node [_ opts]
  (xt/start-node opts))

(defmethod ig/halt-key! :xtdb/node [_ node]
  (.close node))

(defn node [] (get @sys/system :xtdb/node :not-initialized))


(comment
  (restart)
  (xt/submit-tx ( xtdb-node) [[::xt/put {:xt/id "hi2u" :user/name "zig"}]])
  (xt/q (xt/db (xtdb-node)) '{:find [e] :where [[e :user/name "zig"]]})

  ;; http://localhost:8093
  ;; http://localhost:8093/clip/foo

  )
