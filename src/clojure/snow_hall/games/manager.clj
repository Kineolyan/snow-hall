(ns snow-hall.games.manager
  (:require [clojure.spec.alpha :as s]))

; Specs

(s/def ::name string?)
(s/def ::player-count int?)
(s/def ::java string?)
(s/def ::game (s/keys :req [::name ::player-count]
                      :opt [::java]))
(s/def ::games (s/map-of ::name ::game))

; methods

(defn create-store
  "Creates a store holding all games that can be played"
  []
  (ref {} :validator (partial s/valid? ::games)))

(defn add-game
  "Adds a new game to the server"
  [store game]
  (dosync
    (alter store assoc (game :name) game)))

(defn list-games
  "Lists all games available in this server"
  [store]
  (vals @store))
