(ns snow-hall.hall.butler-test
  (:require
    [clojure.test :refer :all]
    [snow-hall.hall.butler :as m]))

(deftest create-tab
  (testing "creates an empty tab"
    (is (= (m/create-tab) {}))))

(deftest generate-id
  (testing "can generate ids without collision"
    (let [tab (reduce
                (fn [acc i] (assoc acc (m/generate-id acc) i))
                {}
                (range 100))]
      (is (= (count tab) 100)))))

(deftest create-gathering
  (let [tab {}
        user {:id 1}
        game {:mame  "g" :player-count 3}
        updated (m/create-gathering tab user game)]
    (testing "registers a new gathering into the tab"
      (= (count updated) 1)
      (= (keys updated) ((comp :id first vals) updated)))

    (testing "records the game to play"
      (let [g (first (vals updated))]
        (= (:game g) "g")))

    (testing "includes the user as first player"
      (let [g (first (vals updated))]
        (= (first (:players g))
           1)))

    (testing "creates connection token for the other players"
      (let [g (first (vals updated))
            others (rest (:players g))
            tokens (map :token others)]
        (= 2 (count (set tokens)))
        (= true (not-any? nil? tokens))))))

