(ns snow-hall.games.round
  (:require [clojure.spec.alpha :as s]
            [snow-hall.validate :refer [create-validation]]
            [snow-hall.uuid :as uuids]))

(s/def ::ruid uuids/uuid?)
(s/def ::engine nil?)
(s/def ::timestamp int?)
(s/def ::content (comp not nil?))
(s/def ::message (s/keys ::req-un [::timestamp ::content]))
(s/def ::messages-data (s/coll-of ::messages))
(s/def ::messages #(-> % meta ::round-messages))
(s/def ::round (s/keys :req-un [::ruid ::engine ::messages]))
(s/def ::rounds (s/map-of ::ruid ::round))

(defn create-store 
  "Creates a store for rounds"
  []
  {})
(def validate-fn (create-validation ::rounds))

(defn create-round
  [gathering]
  (let [player-uuids (:players gathering)
        message-list (into (hash-map) (map #(vector % []) player-uuids))]
    {:ruid (uuids/random-uuid)
     :engine nil
     :messages (agent message-list
                      :meta {::round-messages true}
                      :validator (create-validation ::messages-data))}))

(defn- clear-old-messages
  "Clear the old messages from a given user.
  uuid identifies the user and last-timestamp is the timestamp of the last
  message read by the user."
  [messages uuid last-timestamp]
  {:pre [(contains? messages uuid)]}
  (update-in messages 
             [uuid]
             (comp filter (partial < last-timestamp))))

(defn read-messages
  [round uuid]
  (let [a-messages (:messages round)
        messages (get @a-messages uuid)
        last-timestamp (-> messages last :timestamp)]
    (send a-messages clear-old-messages uuid last-timestamp)))
