(ns snow-hall.games.library.rpsls
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

(defrecord RpclsRound [ios stop state]
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
  [round f rationale]
  (notify-victory round f {:win "WIN: OPPONENT MISTAKE"
                           :loss (str "LOSS: " rationale)})
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
  (if-let [winner (get-winner (:signs state))]
    (update-in state [:scores winner] inc)
    state))

(defn reset-game-in-state
  [{:keys [signs] :as state}]
  (assoc state
         :signs {}
         :last-signs signs))

(defn find-winner
  [{:keys [scores]}]
  (cond
    (win? scores 0) :p1
    (win? scores 1) :p2))

(defn check-for-victory
  [state]
  (if-let [winner (find-winner state)]
    (assoc state
           :winner winner
           :reason :victory)
    state))

(defn all-signs?
  [{:keys [signs]}]
  (every? signs [:p1 :p2]))

(defn update-state
  [state player-id sign]
  (let [state-with-signs (set-sign-in-state state player-id sign)]
    (if (all-signs? state-with-signs)
      (-> state-with-signs 
          update-score-in-state 
          reset-game-in-state
          check-for-victory)
      state-with-signs)))

(defn publish-state?
  [next-state]
  (empty? (:signs next-state)))

(defn publish-completion
  [round state]
  nil) ; publish differently according to the status

(defn get-move
  "Gets the move to play, either as `[player, move]` or :stop to end the game"
  [{[{in1 :in} {in2 :in}] :ios stop :stop}]
  (let [[m c] (alts! [stop in1 in2])]
            (if
              (= c stop) :stop
              [
               (cond 
                 (= c in1) :p1
                 (= c in2) :p2)
               (str->sign m)])))

(defn play-move
  [state player move]
  (update-state state player move))

(defn mark-illegal-turn
  [state player]
  (assoc state
         :status :ended
         :winner (if (= player :p1) :p2 :p1)
         :reason :illegal-turn))

(defn mark-illegal-move
  [state player]
  (assoc state
         :status :ended
         :winner (if (= player :p1) :p2 :p1)
         :reason :illegal-move))

(defn apply-move
  [{:keys [signs] :as state} [player move]]
  (let [p1-played? (:p1 signs)
        p2-played? (:p2 signs)]
    (cond
      (and (= player :p1) p1-played?) (mark-illegal-turn state player)
      (and (= player :p2) p2-played?) (mark-illegal-turn state player)
      (nil? move) (mark-illegal-move state player)
      :else (play-move state player move))))

(defn create-state
  []
  {:scores (create-scoreboard)
   :last-signs nil
   :signs {}
   :status :created
   :winner nil
   :reason nil})

(defn mark-round-as-started
  [round]
  (swap! (:state round) assoc :status :playing))

(defn round-completed?
  [round]
  (= :ended 
     ((comp :status deref :state) round)))

(defn- start
  [round]
  (mark-round-as-started round)
  (go-loop []
    (let [move (get-move round)]
      (if (= move :stop)
        (end-prematurely round)
        (let [next-state (swap! (:state round) apply-move move)]
          (when (publish-state? next-state)
            (publish-state round next-state))
          (if (round-completed? round)
            (publish-completion round next-state)
            (recur)))))))

(defn- create
  []
  (RpclsRound.
   (repeatedly 2 create-io)
   (chan 1)
   (atom (create-state))))

(def game-factory
  (reify
    GameFactory
    (create-engine [f]
      (let [round (create)]
        (start round)
        round))))

(def game-definition
  {:name "Rock Paper Scissors Lizard Spock"
   :player-count 2
   :factory game-factory})

(comment
  (def round (snow-hall.games.game/create-engine game-factory))
  round
  (snow-hall.games.game/stop round)
  
  (def in1 ((comp :in first :ios) round))
  (def out1 ((comp :out first :ios) round))
  (def in2 ((comp :in second :ios) round))
  (def out2 ((comp :out second :ios) round))
  (clojure.core.async/go (while true
                           (do (println (str "p1 -> " (clojure.core.async/<! out1)))
                               (println (str "p2 -> " (clojure.core.async/<! out2))))))
  (clojure.core.async/offer! in1 "rock")
  (clojure.core.async/offer! in2 "paper")
  (clojure.core.async/offer! in1 [1 0])
  (clojure.core.async/offer! in2 [1 2])
  (clojure.core.async/offer! in1 [2 2])
  (clojure.core.async/offer! in1 [0 2])
  (clojure.core.async/poll! out2))

