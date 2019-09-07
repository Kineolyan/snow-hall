(ns snow-hall.games.manager)

(def games (ref {}))

(defn add-game
  "Adds a new game to the server"
  []
  nil)

(defn remove-game
  "Removes a game from the server"
  []
  nil)

(defn list-games
  "Lists all games started in this server"
  []
  @games)
