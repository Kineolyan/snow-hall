(ns snow-hall.games.manager)

(defn create-store
  "Creates a store holding all games that can be played"
  []
  (ref {}))

(defn add-game
  "Adds a new game to the server"
  [store game]
  (dosync
    (alter store assoc (game :name) game)))

(defn list-games
  "Lists all games available in this server"
  [store]
  (vals @store))
