(ns snow-hall.hall.visitor
  (:require
    [:medley.core :refer random-uuid]))

(defn create-registry [] (ref {}))

(defn create-new-user
  "Creates a new user, with only a UUID"
  []
  {:uuid (str (random-uuid))})

(defn register-user [registry user]
  "Register a new user into the registry"
  (assoc registry (:uuid user) user))
