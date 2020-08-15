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

(defn merge-pos-and-input
  [pos input]
  (concat [pos] input))

(defn create-input-io
  [i]
  (async/map (partial merge-pos-and-input i) [(chan 1)] 1))

(comment
  (def c (create-input-io 3))
  (async/offer! c [45 97])
  (async/poll! c))
; FIXME not working yet

(defn create-io
  [i]
  {:in (chan 1) :out (chan 1)})

(defn create-ios
  [{:keys [players]}]
  (repeatedly players create-io))

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

(defn can-play-with-deck?
  [stacks deck]
  (some (partial can-play-card? stacks) deck))

(defn loss?
  [{:keys [current-player decks stacks]}]
  (let [player-deck (nth decks current-player)
        can-play? (can-play-with-deck? stacks player-deck)]
    (not can-play?)))

(defn mark-round-as-started!
  [round]
  (swap! (:state round) assoc :status :playing))

(defn round-completed?
  [round]
  (= :ended
     ((comp :status deref :state) round)))

(defn mark-illegal-turn
  [state player]
  nil)

(defn mark-illegal-move
  [state player]
  nil)

(defn apply-move
  [{:keys [] :as state} [player move]]
  nil)

(defn compute-next-state!
  [round move]
  (swap! (:state round) apply-move move))

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
  nil)

(defn end-prematurely!
  [round]
  (swap! (:state round) assoc :status :ended)
  (send-message! (:ios round) "ABORTED")
  (end-game! round))

(defn publish-state!
  [round state]
  (doseq [io (:ios round)]
    (>!! (:out io) (state->str state))))

(defn publish-completion!
  [round {:keys []}]
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
      (str->cards m)])))

(defn run-loop!
  [round]
  (publish-state! round @(:state round))
  (loop []
    (let [move (get-move! round)]
      (if (= move :stop)
        (end-prematurely! round)
        (let [next-state (compute-next-state! round move)]
          (publish-state! round next-state)
          (if (round-completed? round)
            (publish-completion! round next-state)
            (recur)))))))

(defn- create
  [options]
  (Round.
   (create-ios options)
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
