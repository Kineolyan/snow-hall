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

(defn remove-game
  "Removes a game from the server"
  [store store-name]
  (dosync
    (alter store dissoc store-name)))

(defn list-games
  "Lists all games available in this server"
  [store]
  (vals @store))
