(ns snow-hall.games.library.rpsls
  (:require [clojure.core.async :as async :refer [chan go alts!! >!! close!]]
            [snow-hall.games.game :refer [GameFactory RoundEngine]]
            [snow-hall.games.seq-chan :as seq-chan]))

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

(defrecord RpclsRound [ios stop state]
  RoundEngine
  (ios [e] ios)
  (stop [e] (async/offer! stop true)))

; -- State and management

(defn create-scoreboard
  []
  {:p1 0 :p2 0})

(defn create-state
  []
  {:scores (create-scoreboard)
   :last-signs nil
   :signs {}
   :status :created
   :winner nil
   :messages nil})

(defn state->str
  [{:keys [scores last-signs]}]
  (str (:p1 scores) "|" (:p2 scores) ";" (some-> last-signs :p1 name) "|" (some-> last-signs :p2 name)))

(def win-score 5)
(defn win?
  [scoreboard i]
  (let [score  (case i
                 0 (:p1 scoreboard)
                 1 (:p2 scoreboard))]
    (= score win-score)))

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
           :messages {:win "WIN BY POINTS"
                      :loss "LOSS BY POINTS"}
           :status :ended)
    state))

(defn all-signs?
  [{:keys [signs]}]
  (every? signs [:p1 :p2]))

(defn play-move
  [state player-id sign]
  (let [state-with-signs (set-sign-in-state state player-id sign)]
    (if (all-signs? state-with-signs)
      (-> state-with-signs
          update-score-in-state
          reset-game-in-state
          check-for-victory)
      state-with-signs)))

(defn mark-round-as-started!
  [round]
  (swap! (:state round) assoc :status :playing))

(defn round-completed?
  [round]
  (= :ended
     ((comp :status deref :state) round)))

(defn mark-illegal-turn
  [state player]
  (assoc state
         :status :ended
         :winner (if (= player :p1) :p2 :p1)
         :messages {:win "WIN: OPPONENT ILLEGAL TURN"
                    :loss "LOSS: NOT YOUR TURN"}))

(defn mark-illegal-move
  [state player]
  (assoc state
         :status :ended
         :winner (if (= player :p1) :p2 :p1)
         :messages {:win "WIN: OPPONENT ILLEGAL MOVE"
                    :loss "LOSS: ILLEGAL MOVE"}))

(defn apply-move
  [{:keys [signs winner] :as state} [player move]]
  (let [p1-played? (:p1 signs)
        p2-played? (:p2 signs)]
    (cond
      (some? winner) state ; do not change the state once the game ended
      (and (= player :p1) p1-played?) (mark-illegal-turn state player)
      (and (= player :p2) p2-played?) (mark-illegal-turn state player)
      (nil? move) (mark-illegal-move state player)
      :else (play-move state player move))))

(defn compute-next-state!
  [round move]
  (swap! (:state round) apply-move move))

(defn publish-state?
  [next-state]
  (empty? (:signs next-state)))

; -- side-effect interaction

(defn end-game!
  [round]
  (doseq [{:keys [in out]} (:ios round)]
    (close! in)
    (close! out)))

(defn send-message!
  [ios message]
  (doseq [io ios]
    (>!! (:out io) message)))

(defn notify-victory!
  "Notifies all players matched by f of the victory, and others of defeat"
  [round f messages]
  (let [winners (filter f (:ios round))
        losers (filter (complement f) (:ios round))]
    (send-message! winners (:win messages))
    (send-message! losers (:loss messages))))

(defn end-prematurely!
  [round]
  (swap! (:state round) assoc :status :ended)
  (send-message! (:ios round) "ABORTED")
  (end-game! round))

(defn publish-state!
  [round state]
  (doseq [io (:ios round)]
    (>!! (:out io) (state->str state))))

(defn get-winner-io
  [ios winner]
  (case winner
    :p1 (nth ios 0)
    :p2 (nth ios 1)))

(defn publish-completion!
  [round {:keys [winner messages]}]
  (notify-victory! round (partial = (get-winner-io (:ios round) winner)) messages)
  (end-game! round))

(defn get-move!
  "Gets the move to play, either as `[player, move]` or :stop to end the game"
  [{[{in1 :in} {in2 :in}] :ios stop :stop}]
  (let [[m c] (alts!! [stop in1 in2])]
    (if
     (= c stop) :stop
     [(cond
        (= c in1) :p1
        (= c in2) :p2)
      (str->sign m)])))

(defn run-loop!
  [round]
  (publish-state! round @(:state round))
  (println "init with state")
  (loop []
    (let [move (get-move! round)]
      (println (str "move is " move) )
      (if (= move :stop)
        (end-prematurely! round)
        (let [next-state (compute-next-state! round move)]
          (when (publish-state? next-state)
            (publish-state! round next-state))
          (if (round-completed? round)
            (publish-completion! round next-state)
            (recur)))))))

(defn- start
  [round]
  (mark-round-as-started! round)
  (go (run-loop! round)))

(defn- create
  []
  (let [ins (seq-chan/make-chans [:p1 :p2])
        outs (repeatedly 2 (partial chan 1))
        ios (map #(hash-map :in %1 :out %2) ins outs)]
    (RpclsRound.
     ios
     (chan 1)
     (atom (create-state)))))

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

  (def s0 (create-state))
  (def s1 (apply-move s0 [:p1 :rock]))
  (def s2 (apply-move s1 [:p2 :paper]))
  (def s3 (update-in s2 [:scores :p1] (constantly (dec win-score))))
  (def s4 (apply-move s3 [:p1 :paper]))
  (def s5 (apply-move s4 [:p2 :spock]))

  (apply-move s1 [:p1 :paper])
  (apply-move s1 [:p2 nil])
  (publish-state? s1)
  (publish-state? s5)
  (apply-move s5 [:p1 :rock])
  (state->str s2))

