(ns snow-hall.hall.butler
  (:require
    [:medley.core :refer random-uuid]))

(defn generate-game-id [tab]
  (let [ids (vals tab)]
    (if (empty? ids) 
      1
      (inc (apply max ids)))))

(defn generate-token
  [tab]
  (random-uuid))  

(defn create-tab [] (ref {}))

(defn create-game 
  [tab user game-name]
  (let [game-id (generate-game-id tab)
        new-game {:id game-id
                  :players [(:id user)]}]
    (assoc tab game-id new-game)))
