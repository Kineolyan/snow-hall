(ns snow-hall.games.library.the-game
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.core.async :as async :refer [chan go >!! close!]]
            [snow-hall.validate :as validate]
            [snow-hall.games.game :as game]))

(s/def ::card (s/and int?))
(s/def ::cards (s/coll-of ::card))
(s/def ::deck (s/coll-of ::card))
(s/def ::decks (s/coll-of ::deck))
(s/def ::stack-card #(and (pos? %) (<= % 99)))
(s/def ::stack (s/and (s/coll-of ::card) vector?))
(s/def ::stacks (s/map-of :up ::stacks
                          :down ::stacks))
(s/def ::current-player #(and (<= 0 %) (< % 5)))
(s/def ::status #{:created :playing :ended})
(s/def ::message string?)
(s/def ::state (s/keys ::req-un [::cards ::decks ::stacks ::current-player ::status ::message]))

(def validate-state (validate/create-validation ::state))
(comment
  ; FIXME should be failing
  (validate-state {:cards [1 2]}))

; -- Creating the rounds and piping

(defn create-io
  []
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
     :decks (into [] (partition deck-size picked))}))

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
           :current-player first-player
           :status :created)))
(comment
  (def one-state (create-state {:players 3})))

(defn decks->str
  [decks]
  (->> decks
       (map (partial str/join " "))
       (str/join ",")))

(defn stacks->str
  [{:keys [up down]}]
  (let [joiner (partial str/join " ")]
    (str (joiner up) " - " (joiner down))))

(defn str->stack
  [input]
  (if (or (nil? input) (str/blank? input))
    []
    (let [split-values #(str/split % #" ")]
      (->> input
           split-values
           (map #(Integer/parseInt %))))))

(defn str->stacks
  [input]
  (let [[u1 u2 d1 d2] (str/split input #"\|")]
    {:up (mapv str->stack [u1 u2])
     :down (mapv str->stack [d1 d2])}))

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

(defn get-all-played-cards
  [moves]
  (flatten (vals moves)))

(defn are-illegal-moves?
  [state moves]
  (throw (UnsupportedOperationException.)))

(defn play-cards-on-stack
  [getter cards stack]
  (if (empty? cards)
    stack
    (getter cards)))

(defn play-cards-on-stacks
  [state {:keys [up down]}]
  (-> state
      (update-in [:stacks :up] #(mapv (partial play-cards-on-stack (partial apply max)) up %))
      (update-in [:stacks :down] #(mapv (partial play-cards-on-stack (partial apply min)) down %))))

(defn update-current-player-deck
  [{:keys [current-player] :as state} f]
  (update-in state [:decks current-player] f))

(defn remove-played-cards
  [state moves]
  (let [played-cards (into #{} (get-all-played-cards moves))
        update-fn (partial filter (complement played-cards))]
    (update-current-player-deck state update-fn)))

(defn refill-deck
  [{:keys [decks current-player cards] :as state}]
  (let [target-count (get-cards-per-deck (count decks))
        player-deck (nth decks current-player)
        missing-count (- target-count (count player-deck))
        refill-cards (take missing-count cards)]
    (-> state
        (update-current-player-deck (partial concat refill-cards))
        (update-in [:cards] (partial drop missing-count)))))

(comment
  (def s (update-in one-state [:decks 0] (partial drop 2)))
  (refill-deck s)
  (def s-end (update-in s [:cards] #(list (last %))))
  (refill-deck s-end))

(defn advance-to-next-player
  [{:keys [decks current-player] :as state}]
  (let [player-count (count decks)
        next-index (mod (inc current-player) player-count)]
    (assoc state :current-player next-index)))

(comment
  (def s (create-state {:players 3}))
  (advance-to-next-player s))

(defn play-moves
  [state moves]
  (-> state
      (play-cards-on-stacks moves)
      (remove-played-cards moves)
      refill-deck
      advance-to-next-player))

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
         :message (str "LOSS: ILLEGAL TURN FOR " player)))

(defn mark-illegal-move
  [state player]
  (assoc state
         :status :ended
         :winner (if (= player :p1) :p2 :p1)
         :message (str "LOSS: ILLEGAL MOVE OF " player)))

(defn apply-move
  [{:keys [current-player] :as state} [player moves]]
  (cond
    (not= player current-player) (mark-illegal-turn state player)
    (are-illegal-moves? state moves) (mark-illegal-move state player)
    :else (play-moves state moves)))

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
  [round]
  (send-message! (:ios round) (:message round)))

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
  (notify-victory! round)
  (end-game! round))

(defn chan-of-value
  [c value]
  (async/map (constantly value) [c]))

(defn merge-pos-and-input
  [pos input]
  (concat [pos] input))

(defn indexed-inputs
  "Prefix every input values with the input index"
  [ins]
  (map-indexed #(async/map (partial merge-pos-and-input %1) [%2]) ins))

(defn format-ins
  "Formats all game inputs to produce (<player> <values ...>) or :stop."
  [{:keys [ios stop]}]
  (let [stop-c (chan-of-value stop :stop)
        prefixed-ins (indexed-inputs (map :in ios))]
    (async/merge (concat [stop-c] prefixed-ins))))

(defn run-loop!
  [round input-chan]
  (publish-state! round @(:state round))
  (loop []
    (let [move (async/<! input-chan)]
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
  (let [input-chan (format-ins round)]
    (go (run-loop! round input-chan))))

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
