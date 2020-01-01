(ns snow-hall.hall.visitor
  (:require
   [clojure.spec.alpha :as s]
   [medley.core :as uuids]
   [snow-hall.validate :refer [create-validation]]))

; Specs
(s/def ::uuid string?)
(s/def ::token string?)
(s/def ::nickname string?)
(s/def ::visitor (s/keys :req-un [::uuid ::token]
                         :opt-un [::nickname]))
(s/def ::visitors (s/map-of ::uuid ::visitor))

; Methods
(defn create-registry [] {})
(def validate-fn (create-validation ::visitors))

(defn create
  "Creates a new user, with only a UUID and its secret token"
  []
  {:uuid (str (uuids/random-uuid))
   :token (str (uuids/random-uuid))})

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
       (assoc registry uuid (action user))
       (throw (IllegalArgumentException. (str "Invalid token for " uuid)))))))

(defn register
  "Register a new user into the registry"
  [registry user]
  (assoc registry (:uuid user) user))

(defn set-nickname
  [user nickname]
  (assoc user :nickname nickname))
