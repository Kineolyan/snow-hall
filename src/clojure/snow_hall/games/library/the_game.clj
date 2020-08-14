(ns snow-hall.games.library.the-game
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.core.async :as async :refer [chan go alts!! >!! close!]]
            [snow-hall.validate :as validate]
            [snow-hall.games.game :as game]))

(s/def ::card (s/and int?))
(s/def ::cards (s/coll-of ::card))
(s/def ::deck (s/coll-of ::card))
(s/def ::decks (s/coll-of ::deck))
(s/def ::stack-card #(and (pos? %) (<= % 99)))
(s/def ::stack (s/coll-of ::card))
(s/def ::stacks (s/map-of :up ::stacks
                          :down ::stacks))
(s/def ::current-player #(and (<= 0 %) (< % 5)))
(s/def ::state (s/keys ::req-un [::cards ::decks ::stacks ::current-player]))

(def validate-state (validate/create-validation ::state))
(comment
  ; FIXME should be failing
  (validate-state {:cards [1 2]}))

; -- Creating the rounds and piping

(defn create-io
  []
  {:in (chan 1) :out (chan 1)})

(defrecord Round [ios stop state]
  game/RoundEngine
  (ios [e] ios)
  (stop [e] (async/offer! stop true)))

; -- State and management

(defn generate-cards
  "Creates the list of cards to draw"
  []
  (shuffle (range 2 99)))

(defn get-cards-per-deck
  [n]
  {:pre [(pos? n) (<= n 5)]}
  (case n
    1 8
    2 7
    6))

(defn create-decks
  "Creates decks from the cards for n players"
  [cards n]
  (let [deck-size (get-cards-per-deck n)
        total (* deck-size n)
        picked (take total cards)
        remaining (drop total cards)]
    {:cards remaining
     :decks (partition deck-size picked)}))

(defn create-initial-stacks
  []
  {:up [1 1]
   :down [99 99]})

(defn create-state
  [{:keys [players]}]
  {:post [(validate-state %)]}
  (let [cards (generate-cards)
        state-with-cards (create-decks cards players)
        stacks (create-initial-stacks)
        first-player (rand-int players)]
    (assoc state-with-cards
           :stacks stacks
           :current-player first-player)))

(def one-state (create-state {:players 3}))

(defn decks->str
  [decks]
  (->> decks
       (map (partial str/join " "))
       (str/join ",")))

(defn stacks->str
  [{:keys [up down]}]
  (let [joiner (partial str/join " ")]
    (str (joiner up) " - " (joiner down))))

(defn state->str
  [{:keys [current-player stacks decks]}]
  (str current-player "|" (decks->str decks) "|" (stacks->str stacks)))

(defn win?
  [{:keys [decks]}]
  (every? zero? (map count decks)))

(defn can-play-below?
  [card stack]
  (or
   (< card stack)
   (= card (+ stack 10))))

(defn can-play-above?
  [card stack]
  (or
   (< stack card)
   (= card (- stack 10))))

(defn can-play-card?
  [{:keys [up down]} card]
  (or (some (partial can-play-below? card) down)
      (some (partial can-play-above? card) up)))

(can-play-below? 20 24)
(can-play-card? (:stacks {:up [97 98] :down [24 3]}) 20)

(defn can-play-with-deck?
  [stacks deck]
  (some (partial can-play-card? stacks) deck))

(can-play-with-deck? (:stacks {:up [97 98] :down [24 3]}) [10 20 30])

(defn loss?
  [{:keys [current-player decks stacks]}]
  true)

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
  [state]
  (cond
    (win? state :p1) :p1
    (win? state :p2) :p2))

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
  (loop []
    (let [move (get-move! round)]
      (if (= move :stop)
        (end-prematurely! round)
        (let [next-state (compute-next-state! round move)]
          (when (publish-state? next-state)
            (publish-state! round next-state))
          (if (round-completed? round)
            (publish-completion! round next-state)
            (recur)))))))

(defn- create
  [options]
  (RpclsRound.
   (repeatedly 2 create-io)
   (chan 1)
   (atom (create-state options))))

(defn- start
  [round]
  (mark-round-as-started! round)
  (go (run-loop! round)))

(defn create-and-start
  [options]
  (let [round (create options)]
    (start round)
    round))

(def game-definition
  (reify
    game/Game
    (get-specs [this] {:name "Rock Paper Scissors Lizard Spock"
                       :player-count {:exact 2}})
    (read-options
      [this options]
      {:win-score (get options "win-score" 5)})
    (get-player-count [this options] 2)
    (create-engine [this options] (create-and-start options))))
