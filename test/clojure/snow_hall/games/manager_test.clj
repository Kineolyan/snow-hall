(ns snow-hall.games.manager-test
  (:require [clojure.test :refer :all]
            [snow-hall.games.manager :as mgr]))

(deftest test-initial-store
  (testing "The initial store is empty"
    (let [store (mgr/create-store)]
      (is (= @store {})))))

(deftest test-add-game
  (testing "Adding a game to the store"
    (let [store (mgr/create-store)
          edited (mgr/add-game store {:name "GameName"})]
      (is (= edited {"GameName" {:name "GameName"}}))
      (is (= @store {"GameName" {:name "GameName"}})))))
