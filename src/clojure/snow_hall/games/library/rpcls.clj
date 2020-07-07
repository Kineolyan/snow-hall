(ns snow-hall.games.library.rpcls
  (:require [clojure.core.async :as async :refer [chan go-loop alts! >! close!]]
            [snow-hall.games.game :refer [GameFactory RoundEngine]]))

; -- Creating the rounds and piping

(defn create-io
  []
  {:in (chan 1) :out (chan 1)})

(defrecord RpclsRound [ios stop]
  RoundEngine
  (ios [e] ios)
  (stop [e] (async/offer! stop true)))

(defn create-scoreboard
  []
  {:p1 0 :p2 0})

(defn scores->str
  [scoreboard]
  (str (:p1 scoreboard) "|" (:p2 scoreboard)))

(def win-score 5)
(defn win?
  [scoreboard i]
  (let [score  (case i
                 0 (:p1 scoreboard)
                 1 (:p2 scoreboard))]
    (= score win-score)))

(defn end-game
  [round]
  (doseq [{:keys [in out]} (:ios round)]
    (close! in)
    (close! out)))

(defn all-but-one
  "Gets all values but the one at the given position."
  [values i]
  (->> values
       (map vector (range))
       (filter #(not= i (first %)))
       (map second)))

(defn notify-victory
  [round winner-idx messages]
  (let [winner-io (get round winner-idx)
        losers (all-but-one (:ios round) winner-idx)]
    (>! (:out winner-io) (:win messages))
    (doseq [loser-io losers]
      (>! (:out loser-io) (:loss messages)))))

(defn end-with-victory
  [round winner-idx]
  (notify-victory round winner-idx {:win "WIN" :loss "LOSS"})
  (end-game round))

(defn end-with-mistake
  [round winner-idx]
  (notify-victory round winner-idx {:win "WIN: ILLEGAL MOVE"
                                    :loss "LOSS: NOT YOUR TURN"})
  (end-game round))

(defn end-prematurely
  [round]
  (doseq [io (:ios round)]
    (>! (:out io) "ABORTED"))
  (end-game round))

(defn publish-state
  [round scoreboard]
  (doseq [io (:ios round)]
    (>! (:out io) (scores->str scoreboard))))

(defn handle-loop
  [scoreboard signs]
  nil)

(defn- start
  [{:keys [ios stop] :as round}]
  (let [[{in1 :in} {in2 :in}] ios]
    (go-loop [scoreboard (create-scoreboard)
              signs {}]
      (publish-state round scoreboard)
      (cond
        (win? scoreboard 0) (end-with-victory round 0)
        (win? scoreboard 1) (end-with-victory round 1)
        :else (let [[m c] (alts! [stop in1 in2])
                    p1-played? (:p1 signs)
                    p2-played? (:p2 signs)]
                (if
            ; Halt message, we must stop
                  (= c stop) (end-prematurely round)
                  (apply recur )
            ; legal moves from one of the players
                  (and (= c in1) (not p1-played?)) (recur (update-game scoreboard 0 m)
                                                          signs
                                                          )
                  (and (= c in2) (not p2-played?)) (recur (update-game scoreboard 1 m)
                                                          signs
                                                          )
            ; handling illegal moves
                  (and (= c in2) p2-played?) (end-with-mistake round 0)
                  (and (= c in1) p1-played?) (end-with-mistake round 1))))
      )))

(defn- create
  []
  (RpclsRound.
   (repeatedly 2 create-io)
   (chan 1)))

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
  (def round (snow-hall.games.game/create-engine game-factory))
  round
  (def in1 ((comp :in first :ios) round))
  (def out1 ((comp :out first :ios) round))
  (def in2 ((comp :in second :ios) round))
  (def out2 ((comp :out second :ios) round))
  (clojure.core.async/go (while true
                           (do (println (str "p1 -> " (clojure.core.async/<! out1)))
                               (println (str "p2 -> " (clojure.core.async/<! out2))))))
  (clojure.core.async/offer! in1 [0 0])
  (clojure.core.async/offer! in2 [0 1])
  (clojure.core.async/offer! in1 [1 0])
  (clojure.core.async/offer! in2 [1 2])
  (clojure.core.async/offer! in1 [2 2])
  (clojure.core.async/offer! in1 [0 2])
  (clojure.core.async/poll! out2))

