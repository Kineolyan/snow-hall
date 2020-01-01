(ns snow-hall.games.manager-test
  (:require [clojure.test :refer [deftest testing is]]
            [snow-hall.games.manager :as mgr]))

(deftest test-initial-store []
  (testing "The initial store is empty"
    (let [store (mgr/create-store)]
      (is (= @store {})))))

(deftest test-add-game []
  (testing "Adding a game to the store"
    (let [store (mgr/create-store)
          new-game {:name "GameName" :player-count 2}
          edited (mgr/add-game store new-game)]
      (is (= edited {"GameName" new-game}))
      (is (= @store {"GameName" new-game})))))
