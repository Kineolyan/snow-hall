(ns snow-hall.hall.visitor
  (:require
   [medley.core :refer [random-uuid]]))

(defn create-registry [] {})

(defn create
  "Creates a new user, with only a UUID and its secret token"
  []
  {:uuid (str (random-uuid))
   :token (str (random-uuid))})

(defn on
  [registry uuid action]
  (if-let [user (get registry uuid)]
    (action registry user)
    (throw (IllegalArgumentException. (str uuid)))))

(defn validate-token
  [user token]
  (= (:token user) token))

(defn edit
  [registry uuid token action]
  (on
   registry
   uuid
   (fn [registry user]
     (if (validate-token user token)
       (assoc registry uuid (partial action registry))
       (throw (IllegalArgumentException. (str "Invalid token for " uuid)))))))

(defn register
  "Register a new user into the registry"
  [registry user]
  (assoc registry (:uuid user) user))

(defn set-nickname
  [user nickname]
  (assoc user :nickname nickname))
