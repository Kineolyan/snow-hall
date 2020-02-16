(ns snow-hall.games.library.tic-tac-toe
  (:require [clojure.core.async :as async :refer [chan go <! >! close!]]
            [snow-hall.games.game :refer [GameFactory RoundEngine]]))

(defn create-io
  []
  {:in (chan 1) :out (chan 1)})

(defrecord TicTacToeRound [ios stop]
  RoundEngine
  (ios [e] ios)
  (stop [e] (compare-and-set! stop false true)))

(def end-message "-THE END-")

(defn get-value
  "Gets the cell at [x, y] in the game board."
  [game [x y]]
  {:pre [(or (pos? x) (zero? x))
         (< x (count game))
         (or (pos? y) (zero? y))
         (< y (count (first game)))]}
  (-> game
      (nth x)
      (nth y)))

(def player-symbols
  {0 "X"
   1 "O"})

(defn create-game
  "Creates a standard board of Tic-Tac-Toe of 3x3.
  The board is filled with nil."
  []
  (into [] (repeatedly 3 (partial vector nil nil nil))))

(defn play-move
  [game player-id [x y]]
  {:pre [(or (pos? x) (zero? x))
         (< x (count game))
         (or (pos? y) (zero? y))
         (< y (count (first game)))]}
  (let [s (player-symbols player-id)]
    (update-in game [x y] (constantly s))))

(defn update-game
  [game player-id pos]
  (if (get-value game pos)
    "LOSS"
    (let [next-game (play-move game player-id pos)]
      next-game)))

(defn- start
  [{:keys [ios stop]}]
  (let [[io1 io2] ios]
    (go (do
          (while (not @stop)
            (let [m1 (<! (:in io1))
                  m2 (<! (:in io2))]
              (println (str m1 " - " m2))
              (>! (:out io1) 1)
              (>! (:out io2) 2)))
          (doseq [io [io1 io2]]
            (>! (:out io) end-message)
            (close! (:out io))
            (close! (:in io)))))))

(defn- create
  []
  (SampleRound.
   (repeatedly 2 create-io)
   (atom false)))

(def game-factory
  (reify
    GameFactory
    (create-engine [f]
      (let [round (create)]
        (start round)
        round))))

(def game-definition
  {:name "Tic Tac Toe"
   :player-count 2
   :factory game-factory})

(comment
  (def g (create-game))
  (play-move g 0 [0 0])
  (play-move g 1 [2 1])
  (snow-hall.games.game/create-engine game-factory))
