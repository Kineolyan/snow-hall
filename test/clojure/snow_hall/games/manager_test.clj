(ns snow-hall.games.manager-test
  (:require [clojure.test :refer [deftest testing is]]
            [snow-hall.games.manager :as mgr]
            [snow-hall.games.game :as game]))

(deftest initial-store-test []
  (testing "The initial store is empty"
    (let [store (mgr/create-store)]
      (is (= store {})))))

(defn create-fake-game
  []
  (reify
    game/Game
    (get-specs [this] {:name "GameName"
                       :player-count {:exact 2}})
    (read-options [this options] nil)
    (get-player-count [this opions] nil)
    (create-engine [this options] (throw (UnsupportedOperationException.)))))

(deftest add-game-test []
  (testing "Adding a game to the store"
    (let [store (mgr/create-store)
          new-game (create-fake-game)
          edited (mgr/add-game store new-game)]
      (is (= edited {"GameName" new-game})))))
