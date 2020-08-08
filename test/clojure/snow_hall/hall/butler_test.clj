(ns snow-hall.hall.butler-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [snow-hall.hall.butler :as m]
   [snow-hall.games.game :as game]))

(deftest create-tab []
  (testing "creates an empty tab"
    (is (= (m/create-tab) {}))))

(deftest generate-id []
  (testing "can generate ids without collision"
    (let [tab (reduce
               (fn [acc i] (assoc acc (m/generate-id acc) i))
               {}
               (range 100))]
      (is (= (count tab) 100)))))

(def dummy-game
  (reify game/Game
    (get-specs [this] {:name "g"
                      :players {:exact 2}})
    (read-options [this options] {})
    (get-player-count [this options] 3)
    (create-engine [this options] (throw (UnsupportedOperationException.)))))

(deftest create-gathering []
  (let [tab {}
        user {:uuid 1}
        game dummy-game
        new-gathering (m/create-gathering
                       {:tab tab
                        :user user
                        :game game})
        updated (m/register-gathering tab new-gathering)]
    (testing "registers a new gathering into the tab"
      (is (= (count updated) 1))
      (is (= (keys updated)
             (map :id (vals updated))))
      (is (contains? updated (:id new-gathering))))

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

(deftest get-token-idx []
  (let [players [1 {:token "abc"} 2 {:token "def"}]]
    (testing "finds the index of an existing token"
      (is (= 1 (m/get-token-idx players "abc")))
      (is (= 3 (m/get-token-idx players "def"))))

    (testing "returns falsy for an unknown token"
      (is (not (m/get-token-idx players "ghi"))))))

(deftest join-gathering []
  (let [gathering (m/create-gathering
                   {:tab {}
                    :user {:uuid 1}
                    :game dummy-game})
        initial-tab (m/register-gathering {} gathering)
        gathering (first (vals initial-tab))
        updated-tab (m/join-gathering
                     {:tab initial-tab
                      :user {:uuid 2}
                      :gathering-id (:id gathering)
                      :token (:token (nth (:players gathering) 1))})]

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
        (is (= (count tokens) 1))))

    (testing "can consume any item"
      (let [other-tab (m/join-gathering
                       {:tab initial-tab
                        :user {:uuid 3}
                        :gathering-id (:id gathering)
                        :token ((comp :token last :players) gathering)})
            updated-gathering (get other-tab (:id gathering))]
        (is (some #{3} (:players updated-gathering)))))))

(deftest get-invit-tokens []
  (let [gathering (m/create-gathering
                   {:tab {}
                    :user {:uuid 1}
                    :game dummy-game})
        initial-tab (m/register-gathering {} gathering)
        id (:id gathering)
        updated-tab (m/join-gathering
                     {:tab initial-tab
                      :user {:uuid 2}
                      :gathering-id id
                      :token (:token (nth (:players gathering) 2))})
        full-tab (m/join-gathering
                  {:tab updated-tab
                   :user {:uuid 3}
                   :gathering-id id
                   :token (:token (nth (:players gathering) 1))})]

    (testing "returns all free tokens at start"
      (is (= (m/get-invit-tokens {:tab initial-tab
                                  :gathering-id id})
             (map :token (rest (:players gathering))))))

    (testing "gets all free tokens discarding used"
      (is (= (m/get-invit-tokens {:tab updated-tab
                                  :gathering-id id})
             [(:token (nth (:players gathering) 1))])))

    (testing "returns an empty list when the game is complete"
      (is (empty? (m/get-invit-tokens {:tab full-tab
                                       :gathering-id id}))))))


