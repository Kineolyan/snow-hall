(ns snow-hall.games.manager
  (:require [clojure.spec.alpha :as s]
            [snow-hall.validate :refer [create-validation]]))

; Specs

(s/def ::name string?)
(s/def ::player-count int?)
(s/def ::java string?)
(s/def ::game (s/keys :req-un [::name ::player-count]
                      :opt-un [::java]))
(s/def ::games (s/map-of ::name ::game))

; methods

(def validate-fn (create-validation ::games))

(defn create-store
  "Creates a store holding all games that can be played"
  []
  (ref {} :validator validate-fn))

(defn add-game
  "Adds a new game to the server"
  [store game]
  (dosync
    (alter store assoc (:name game) game)))

(defn list-games
  "Lists all games available in this server"
  [store]
  (vals @store))
