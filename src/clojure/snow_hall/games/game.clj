(ns snow-hall.games.game)

(defprotocol GameFactory
  "Abstraction of a game"
  (create-engine [] "Creates a new round engine for this game.
  c is the number of players participating the round.
  Returns a Round."))

(defprotocol RoundEngine
  (ios [] "Returns the arrays of IOs for each player in the Round.
  Each IO is made of :in chan for player move and :out chan for Round messages." )
  (stop [] "Stops the round."))