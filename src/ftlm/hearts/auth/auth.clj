(ns ftlm.hearts.auth.auth
  (:require
   [buddy.hashers :as hashers]
   [ring.util.response :refer [redirect]]
   [buddy.auth.backends :as backends]
   [malli.core :as m]
   [buddy.auth.middleware :as buddy]))

(defprotocol UserStore
  (user [this email])
  (assert-user [this user]))

(def store
  (let [db (atom {})
        !id (atom 0)]
    (reify UserStore
      (user [this k] (@db k))
      (assert-user [this {:keys [user/email] :as user}]
        (when-not
            (@db email)
          (let  [id (swap! !id inc)
                 user (assoc user :db/id id)]
            (swap! db assoc email user id user)))
        {:db-after this}))))

(def user-shape
  (m/schema
   [:map {}
    [:user/email [:string {:min 2 :max 50}]]
    [:user/pw [:string {:min 1}]]]))

(defn authenticate [{:keys [identity]}]
  (user store identity))

(defn login [req]
  (def req req)
  (assoc (redirect "/") :session {:identity 1}))

(def auth-middleware
  {:name ::auth
   :wrap #(buddy/wrap-authentication % (backends/session {:authfn authenticate}))})


(comment

  (assert-user store #:user{:email "foo" :pw (hashers/derive "foo")})
  (hashers/check "foo" (:user/pw (user store "foo")))
  (user store 1)
  (user store 2)


  (m/validate user-shape {:user/foo "foo"})
  (m/explain user-shape {:user/pw "foobarbarbar" :user/email "lol"})
  )
