(ns snow-hall.games.round
  (:require [clojure.spec.alpha :as s]
            [clojure.core.async :as async :refer [<! >!!]]
            [snow-hall.validate :refer [create-validation]]
            [snow-hall.uuid :as uuids]
            [snow-hall.hall.visitor :as visitors]
            [snow-hall.games.game :as game]))

(s/def ::ruid uuids/uuid?)
(s/def ::game string?)
(s/def ::engine (comp not nil?))
(s/def ::timestamp int?)
(s/def ::content (comp not nil?))
(s/def ::message (s/keys ::req-un [::timestamp ::content]))
(s/def ::messages (s/map-of ::visitors/uuid (s/coll-of ::message)))
(s/def ::last (s/map-of ::visitors/uuid ::content))
(s/def ::state-data (s/keys ::req-un [::messages ::last]))
(s/def ::state #(-> % meta ::round-state))
(s/def ::players (s/coll-of ::visitors/uuid))
(s/def ::round (s/keys :req-un [::ruid
                                ::game
                                ::players
                                ::engine
                                ::state]))
(s/def ::rounds (s/map-of ::ruid ::round))

(defn create-store
  "Creates a store for rounds"
  []
  {})
(def validate-fn (create-validation ::rounds))

(defn- clear-old-messages
  "Clear the old messages from a given user.
  uuid identifies the user and last-timestamp is the timestamp of the last
  message read by the user."
  [state uuid last-timestamp]
  {:pre [(contains? (:messages state) uuid)]}
  (update-in state
             [:messages uuid]
             (partial filter (partial < last-timestamp))))

(defn read-messages
  [round uuid]
  (let [a-state (:state round)
        messages (get-in @a-state [:messages uuid])
        last-timestamp (-> messages last :timestamp)]
    (send a-state clear-old-messages uuid last-timestamp)
    ; Returns the captured messages
    messages))

(defn- add-to-messages
  [state uuid content]
  (let [new-message {:timestamp (System/currentTimeMillis)
                     :content content}]
    (-> state
        (update-in [:messages uuid] conj new-message)
        (update-in [:last] assoc uuid new-message))))

(defn- send-message
  [state uuid content]
  (send state add-to-messages uuid content))

(defn- bind-engine
  "Binds the game engine to the state of the game.
  This mostly reads the outputs of the game engine and generate the
  appropriate messages"
  [a-state player-uuids round]
  (let [ios (game/ios round)]
    (doseq [[uuid {out :out}] (map vector player-uuids ios)]
      (async/go-loop [continue true]
        (when continue
          (->> (<! out)
               (send-message a-state uuid))
          (recur true))))))

(defn create-round
  [gathering game]
  (let [player-uuids (:players gathering)
        message-list (into (hash-map) (map #(vector % []) player-uuids))
        state {:messages message-list :last {}}
        a-state (agent state
                       :meta {::round-state true}
                       :validator (create-validation ::state-data))
        engine (game/create-engine game)]
    (bind-engine a-state player-uuids engine)
    {:ruid (uuids/random-uuid)
     :game (:game gathering)
     :players player-uuids
     :engine engine
     :state a-state}))

(defn read-last-state
  [round uuid]
  (-> round
      :state
      deref
      :last
      (get uuid)))

(defn play-round
  [round uuid move]
  (let [pid (.indexOf (:players round) uuid)
        ios (get-in round [:engine :ios])
        in (-> ios (nth pid) :in)]
    (>!! in move)))

(defn kill-game
  [round]
  (let [kill (get-in round [:engine :switch])]
    (kill)))

(comment
  ; small execution of the code for tests
  (def us (repeatedly 2 uuids/random-uuid))
  (def u1 (first us))
  (def u2 (second us))
  (def g {:players us :game "test"})
  (def r1 (create-round g nil))
  (play-round r1 u2 "IDLE")
  (play-round r1 u1 "MOVE 1")
  ((comp :last deref :state) r1))
