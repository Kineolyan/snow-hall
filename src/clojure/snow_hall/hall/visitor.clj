(ns snow-hall.hall.visitor
  (:require
    [medley.core :refer [random-uuid]]))

(defn create-registry [] {})

(defn create-new-user
  "Creates a new user, with only a UUID and its secret token"
  []
  {
    :uuid (str (random-uuid))
    :token (str (random-uuid))})

(defn set-nickname
  "Sets the nickname of the given user.
  If a nickname was already defined, the current value is erased."
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
