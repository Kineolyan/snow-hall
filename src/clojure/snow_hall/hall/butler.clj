(ns snow-hall.hall.butler
  (:require    [clojure.spec.alpha :as s]
               [snow-hall.uuid :as uuids]
               [snow-hall.validate :refer [create-validation]]
               [snow-hall.games.game :as games]))

(s/def ::id string?)
(s/def ::token uuids/uuid?)
(s/def ::game string?)
(s/def ::player-list
  (s/coll-of (s/alt
              :player :snow-hall.hall.visitors/uuid
              :token (s/keys :req-un [::token]))))
(s/def ::gathering (s/keys :req-un [::id ::game]))
(s/def ::tab (s/map-of ::id ::gathering))

(defn generate-id
  [tab]
  (let [ids (keys tab)]
    (if (empty? ids)
      "1"
      (->> ids (map #(Integer/parseInt %)) (apply max) (inc) (str)))))

(defn create-tab
  "Creates the initial tab"
  []
  {})
(def validate-fn (create-validation ::tab))

(defn create-player-list
  [player-count first-player]
  (reduce
   (fn [acc _] (conj acc {:token (uuids/random-uuid)}))
   [(:uuid first-player)]
   (range (dec player-count))))

(defn create-gathering
  "Creates a new game for a user"
  [{:keys [tab user game user-options]}]
  (let [game-id (generate-id tab)
        game-options (games/read-options game user-options)
        player-count (games/get-player-count game game-options)
        players (create-player-list player-count user)]
    {:id game-id
     :game (games/get-name game)
     :players players
     :options game-options}))

(defn register-gathering
  "Registers a new gathering to the tab."
  [tab gathering]
  (assoc tab (:id gathering) gathering))

(defn get-invit-tokens
  "Retrieves the free invits for a given gathering"
  [{:keys [tab gathering-id]}]
  (let [gathering (get tab gathering-id)]
    (->> (:players gathering)
         (map :token)
         (filter (complement nil?)))))

(defn has-more-invits?
  "Tests if a given gathering has more free invits"
  [args]
  ((comp not empty? get-invit-tokens) args))

(defn get-token-idx
  [players token]
  (->> (map :token players)
       (map vector (range))
       (filter #(= token (second %)))
       (ffirst)))

(defn integrate-visitor
  [game user token]
  (if-let [i (get-token-idx (:players game) token)]
    (assoc
     game
     :players
     (assoc (:players game) i (:uuid user)))
    (throw (IllegalArgumentException. (str "Token not in the gathering list: " token)))))

(defn join-gathering
  "Joins an existing gathering"
  [{:keys [tab user gathering-id token]}]
  (if-let [game (get tab gathering-id)]
    (assoc
     tab
     gathering-id
     (integrate-visitor game user token))
    (throw (IllegalArgumentException. (str "Not a gathering id " gathering-id)))))

