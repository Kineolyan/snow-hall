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
        game {:name "g" :player-count 3}
        updated (m/create-gathering {:tab tab
                                     :user user
                                     :game game})]
    (testing "registers a new gathering into the tab"
      (is (= (count updated) 1))
      (is (= (keys updated)
             (map :id (vals updated)))))

    (testing "records the game to play"
      (let [g (first (vals updated))]
        (is (= (:game g) "g"))))

    (testing "includes the user as first player"
      (let [g (first (vals updated))]
        (is (= (first (:players g))
               1))))

    (testing "creates connection token for the other players"
      (let [g (first (vals updated))
            others (rest (:players g))
            tokens (map :token others)]
        (is (= 2 (count (set tokens))))
        (is (= true (not-any? nil? tokens)))))))

(deftest get-token-idx
  (let [players [1 {:token "abc"} 2 {:token "def"}]]
    (testing "finds the index of an existing token"
      (is (= 1 (m/get-token-idx players "abc")))
      (is (= 3 (m/get-token-idx players "def"))))

    (testing "returns falsy for an unknown token"
      (is (not (m/get-token-idx players "ghi"))))))

(deftest join-gathering
  (let [initial-tab (m/create-gathering
                      {:tab {}
                       :user {:id 1}
                       :game {:name "g" :player-count 3}})
          gathering (first (vals initial-tab))
          updated-tab (m/join-gathering
                        {:tab initial-tab
                         :user {:id 2}
                         :gathering-id (:id gathering)
                         :token (:token (nth (:players gathering) 1))})]
    (println updated-tab)
    (testing "adds the visitor to the gathering"
      (let [updated-gathering (get updated-tab (:id gathering))]
        (is (some #{2} (:players updated-gathering)))))

    (testing "still contains the initial visitor"
      (let [updated-gathering (get updated-tab (:id gathering))]
        (is (some #{1} (:players updated-gathering)))))

    (testing "consumes a token"
      (let [updated-gathering (get updated-tab (:id gathering))
            tokens (->> (:players updated-gathering)
                        (map :token)
                        (filter (complement nil?)))]
        (is (= (count tokens) 1))))))



