(ns snow-hall.hall.butler
  (:require
    [medley.core :refer [random-uuid]]))

(defn generate-id
  [tab]
  (let [ids (keys tab)]
    (if (empty? ids)
      1
      (inc (apply max ids)))))

(defn create-tab
  "Creates the initial tab"
  []
  {})

(defn create-player-list
  [player-count first-player]
  (reduce
    (fn [acc _] (conj acc {:token (random-uuid)}))
    [(:id first-player)]
    (range (dec player-count))))

(defn create-gathering
  "Creates a new game for a user"
  [{:keys [tab user game]}]
  (let [game-id (generate-id tab)
        players (create-player-list (:player-count game) user)
        new-game {:id game-id
                  :game (:name game)
                  :players players}]
    (assoc tab game-id new-game)))

(defn get-token-idx
  [players token]
  (->> (map :token players)
       (map vector (range))
       (filter (partial = token))
       (ffirst)))

(defn integrate-visitor
  [game user token]
  (if-let [i (get-token-idx (:players game) token)]
    (assoc (:players game) i (:id user))
    {:error (str "Token not in the gathering list: " token)}))

(defn join-gathering
  "Joins an existing gathering"
  [{:keys [tab user gathering-id token]}]
  (if-let [game (get tab gathering-id)]
    (integrate-visitor game user token)
    {:error (str "Not a gathering id " gathering-id)}))

