(ns snow-hall.games.game)

(defprotocol Game
  "Abstraction of a game"
  (get-specs
   [game]
   "Gets the specs of the game")
  (read-options 
   [game options]
   "Reads user options to create options specific to this game")
  (get-player-count
   [game options]
   "Gets the number of players participating to this game")
  (create-engine 
   [game options] 
   "Creates a new round engine for this game.
   c is the number of players participating the round.
   Returns a Round."))

(defn get-name
  "Gets the name of the game."
  [game]
  (-> game (get-specs) (:name)))

(defn get-player-range
  [game]
  {:post [(some? %)]}
  (-> game (get-specs)( :players)))

(defprotocol RoundEngine
  (ios [e] "Returns the arrays of IOs for each player in the Round.
  Each IO is made of :in chan for player move and :out chan for Round messages." )
  (stop [e] "Stops the round."))
