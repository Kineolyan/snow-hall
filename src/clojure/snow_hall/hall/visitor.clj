(ns snow-hall.hall.visitor
  (:require
    [medley.core :refer [random-uuid]]))

(defn create-registry [] {})

(defn create-new-user
  "Creates a new user, with only a UUID"
  []
  {:uuid (str (random-uuid))})

(defn set-nickname
  [registry uuid nickname]
  (if-let [user (get registry uuid)]
    (assoc
      registry
      uuid
      (assoc user :nickname nickname))
    (throw (IllegalArgumentException. (str uuid)))))

(defn register-user [registry user]
  "Register a new user into the registry"
  (assoc registry (:uuid user) user))
