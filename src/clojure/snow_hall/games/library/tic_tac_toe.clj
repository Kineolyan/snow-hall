(ns snow-hall.games.library.tic-tac-toe
  (:require [clojure.core.async :as async :refer [chan go <! >! close!]]
            [snow-hall.games.game :refer [GameFactory RoundEngine]]))

(def player-symbols
  {0 "X"
   1 "O"})

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

(defn get-row
  "Gets the row x of the game"
  [game x]
  (nth game x))

(defn get-col
  "Gets the column y of the game"
  [game y]
  (map #(nth % y) game))

(defn get-diag-lurd
  "Game left-up to right-down diagonal"
  [game]
  (map #(get-value game [% %]) (range 3)))

(defn get-diag-ldru
  "Game left-down to right-up diagonal"
  [game]
  (map #(get-value game [(- 2 %) %]) (range 3)))

(defn get-combinations
  [game]
  (concat (map (partial get-row game) (range 3))
          (map (partial get-col game) (range 3))
          [(get-diag-ldru game) (get-diag-lurd game)]))

(defn winning-combination?
  [player row]
  (every? (partial = (player-symbols player)) row))

(defn win?
  "Tests if a game is a win for a player"
  [game player]
  (->> game
       (get-combinations)
       (some (partial winning-combination? player))))

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
    "CHEATING"
    (let [next-game (play-move game player-id pos)]
      next-game)))

(comment
  (def g (create-game))
  (def g1 (-> g
              (play-move 0 [0 0])
              (play-move 1 [2 1])))
  (get-row g1 0)
  (get-col g1 1)
  (play-move g 0 [0 0])
  (play-move g 1 [2 1])
  (get-combinations g1)
  (win? g1 0)
  
  (def g2 (-> g1
              (play-move 0 [0 0])
              (play-move 0 [1 0])
              (play-move 0 [2 0])))
  (map (partial win? g2) (range 2)))

; -- Creating the rounds and piping

(defn create-io
  []
  {:in (chan 1) :out (chan 1)})

(defrecord TicTacToeRound [ios stop]
  RoundEngine
  (ios [e] ios)
  (stop [e] (compare-and-set! stop false true)))

(def end-message "-THE END-")

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
  (TicTacToeRound.
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
  (snow-hall.games.game/create-engine game-factory))
