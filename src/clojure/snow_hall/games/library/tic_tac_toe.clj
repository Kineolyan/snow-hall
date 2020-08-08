(ns snow-hall.games.library.tic-tac-toe
  (:require [clojure.core.async :as async :refer [chan go-loop alts! >! close!]]
            [snow-hall.games.game :as game]))

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

(defn get-positions
  "Gets all positions back to back."
  [game]
  (flatten game))

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
  (get-positions g1)
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
  game/RoundEngine
  (ios [e] ios)
  (stop [e] (async/offer! stop true)))

(defn game->str
  [game]
  (->> game
       (get-positions)
       (map #(or % "-"))
       (apply str)))

(defn- create
  []
  (TicTacToeRound.
   (repeatedly 2 create-io)
   (chan 1)))

(defn- start
  [{:keys [ios stop]}]
  (let [[{in1 :in out1 :out :as io1} {in2 :in out2 :out :as io2}] ios]
    (go-loop [turn :p1
              game (create-game)
              stopped false]
      (if-not stopped
        (do
          (doseq [out [out1 out2]]
            (>! out (game->str game)))
          (cond
            (win? game 0) (do (>! out1 "WIN")
                              (>! out2 "LOSS")
                              (recur turn game true))
            (win? game 1) (do (>! out1 "LOSS")
                              (>! out2 "WIN")
                              (recur turn game true))
            :else (let [[m c] (alts! [stop in1 in2])]
                    (cond
            ; Halt message, we must stop
                      (= c stop) (recur turn game true)
            ; legal moves from one of the players
                      (and (= turn :p1) (= c in1)) (recur :p2
                                                          (update-game game 0 m)
                                                          false)
                      (and (= turn :p2) (= c in2)) (recur :p1
                                                          (update-game game 1 m)
                                                          false)
            ; handling illegal moves
                      (and (= turn :p1) (= c in2)) (do (>! out2 "LOSS: NOT YOUR TURN")
                                                       (>! out1 "WIN: ILLEGAL MOVE")
                                                       (recur turn game true))
                      (and (= turn :p2) (= c in1)) (do (>! out1 "LOSS: NOT YOUR TURN")
                                                       (>! out2 "WIN: ILLEGAL MOVE")
                                                       (recur turn game true))))))
        (doseq [io [io1 io2]]
          (close! (:out io))
          (close! (:in io)))))))

(defn create-and-start
  []
  (let [round (create)]
    (start round)
    round))

(def game-definition
  (reify
    game/Game
    (get-specs [this] {:name "Tic Tac Toe"
                      :player-count {:exact 2}})
    (read-options [this options] nil)
    (get-player-count [this options] 2)
    (create-engine [this options] (create-and-start))))

(comment
  (def round (game/create-engine game-definition nil))
  round
  (def in1 ((comp :in first :ios) round))
  (def out1 ((comp :out first :ios) round))
  (def in2 ((comp :in second :ios) round))
  (def out2 ((comp :out second :ios) round))
  
  (clojure.core.async/offer! in1 [0 0])
  (clojure.core.async/offer! in2 [0 1])
  (clojure.core.async/offer! in1 [1 0])
  (clojure.core.async/offer! in2 [1 2])
  (clojure.core.async/offer! in1 [2 2])
  (clojure.core.async/offer! in1 [0 2])
  (clojure.core.async/poll! out2))

