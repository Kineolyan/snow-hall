(ns snow-hall.games.library.rpcls
  (:require [clojure.core.async :as async :refer [chan go-loop alts! >! close!]]
            [snow-hall.games.game :refer [GameFactory RoundEngine]]))

(def win-matrix
  {:rock #{:scissors :lizard}
   :paper #{:rock :spock}
   :scissors #{:paper :lizard}
   :lizard #{:paper :spock}
   :spock #{:rock :scissors}})

(def allowed-signs (apply hash-set (keys win-matrix)))

(defn valid-sign?
  [sign]
  (allowed-signs sign))

(defn str->sign
  [input]
  (let [sign (keyword input)]
    (when (valid-sign? sign) sign)))

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

(defn state->str
  [{:keys [scores last-signs]}]
  (str (:p1 scores) "|" (:p2 scores) " - " (:p1 last-signs) "|" (:p2 last-signs)))

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

(defn send-message
  [ios message]
  (doseq [io ios]
    (>! (:out io) message)))

(defn notify-victory
  "Notifies all players matched by f of the victory, and others of defeat"
  [round f messages]
  (let [winners (filter f (:ios round))
        losers (filter (complement f) (:ios round))]
    (send-message winners (:win messages))
    (send-message losers (:loss messages))))

(defn end-with-victory
  [round f]
  (notify-victory round f {:win "WIN" :loss "LOSS"})
  (end-game round))

(defn end-with-mistake
  [round f]
  (notify-victory round f {:win "WIN: ILLEGAL MOVE"
                           :loss "LOSS: NOT YOUR TURN"})
  (end-game round))

(defn end-prematurely
  [round]
  (send-message (:ios round) "ABORTED")
  (end-game round))

(defn publish-state
  [round state]
  (doseq [io (:ios round)]
    (>! (:out io) (state->str state))))

(defn set-sign-in-state
  [state player-idx sign]
  (update-in state [:signs player-idx] (constantly sign)))

(defn winning-sign-over?
  [s1 s2]
  (-> win-matrix s1 s2))

(defn get-winner
  [{:keys [p1 p2]}]
  (cond
    (winning-sign-over? p1 p2) :p1
    (winning-sign-over? p2 p1) :p2))

(defn update-score-in-state
  [state]
  (let [winner (get-winner (:signs state))]
    (update-in state [:scores winner] inc)))

(defn reset-game-in-state
  [{:keys [signs] :as state}]
  (assoc state
         :signs {}
         :last-signs signs))

(defn all-signs?
  [{:keys [signs]}]
  (every? signs [:p1 :p2]))


(defn update-state
  [state player-idx input]
  (if-let [sign (str->sign input)]
    (let [state-with-signs (set-sign-in-state state player-idx sign)]
      (if (all-signs? state-with-signs)
        (-> state-with-signs update-score-in-state reset-game-in-state)
        state-with-signs))
    (end-with-mistake)))

(defn score-changed?
  [prev-state next-state]
  (not= (:scores prev-state) (:scores next-state)))

(defn publish-state?
  [prev-state next-state]
  (score-changed? prev-state next-state))

(defn handle-loop
  [{[{in1 :in} {in2 :in}] :ios stop :stop :as round} {:keys [scores signs] :as state}]
  (cond
    (win? scores 0) (end-with-victory round 0)
    (win? scores 1) (end-with-victory round 1)
    :else (let [[m c] (alts! [stop in1 in2])
                p1-played? (:p1 signs)
                p2-played? (:p2 signs)]
            (cond
            ; Halt message, we must stop
              (= c stop) (end-prematurely round)
            ; legal moves from one of the players
              (and (= c in1) (not p1-played?)) (update-state state 0 m)
              (and (= c in2) (not p2-played?)) (update-state state 1 m)
            ; handling illegal moves
              (and (= c in2) p2-played?) (end-with-mistake round 0)
              (and (= c in1) p1-played?) (end-with-mistake round 1)))))

(defn- start
  [round]
  (go-loop [state {:scores (create-scoreboard)
                   :last-signs nil
                   :signs {}}]
    (when-let [next-state (handle-loop round state)]
      (when (publish-state? state next-state)
        (publish-state round next-state))
      (recur next-state))))

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

