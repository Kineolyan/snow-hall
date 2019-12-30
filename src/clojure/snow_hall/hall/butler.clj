(ns snow-hall.hall.butler
  (:require
  ;  [clojure.spec.alpha :as spec]
   [snow-hall.uuid :refer [random-uuid]]))

; (spec/def ::id string?)
; (spec/def ::token string?)
; (spec/def ::game-name string?)
; (spec/def ::player-list 
;   (spec/coll-of (spec/alt
;               :player (spec/keys [::id])
;               :token ::token)))
; (spec/def ::gathering (spec/keys [::id ::game-name]))
; (spec/def ::tab (spec/map-of ::id ::gathering))

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

(defn create-player-list
  [player-count first-player]
  (reduce
    (fn [acc _] (conj acc {:token (random-uuid)}))
    [(:uuid first-player)]
    (range (dec player-count))))

(defn create-gathering
  "Creates a new game for a user"
  [{:keys [tab user game]}]
  (let [game-id (generate-id tab)
        players (create-player-list (:player-count game) user)]
    {:id game-id
     :game (:name game)
     :players players}))

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

