(ns snow-hall.hall.butler
  (:require
    [:medley.core :refer random-uuid]))

(defn generate-id [tab]
  (let [ids (vals tab)]
    (if (empty? ids)
      1
      (inc (apply max ids)))))

(defn create-tab
  "Creates the initial tab"
  []
  (ref {}))

(defn create-gathering
  "Creates a new game for a user"
  [tab user game]
  (let [game-id (generate-id tab)
        players (reduce
                  (fn [acc _] (conj acc {:token (random-uuid)}))
                  [(:id user)]
                  (range (dec (:player-count game))))
        new-game {:id game-id
                  :game (:name game)
                  :players [(:id user)]}]
    (assoc tab game-id new-game)))
