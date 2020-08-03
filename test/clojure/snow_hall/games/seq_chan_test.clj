(ns snow-hall.games.seq-chan-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.core.async :as async]
            [snow-hall.games.seq-chan :as m]))

(deftest create-cell
  (let [cell (m/create-cell [:p1 :p2 :p3])]
    (testing "has no content"
      (is (nil? (:content @cell))))

    (testing "has first item as owner"
      (is (= :p1 (:owner @cell)))))

  (testing "requires list of owners"
    (is (thrown? AssertionError (m/create-cell :p1)))))

(defn cell-012 [] (m/create-cell (range 3)))

(deftest fill-cell
  (testing "fill empty cell with correct owner"
    (let [cell (cell-012)]
      (m/fill-cell! cell 0 :value)
      (is (= (:owner @cell) 0))
      (is (= (:content @cell) :value))))

  (testing "cannot fill cell with another owner"
    (let [cell (cell-012)
          before @cell]
      (m/fill-cell! cell 2 :value)
      (is (= before @cell))))

  (testing "cannot fill with nil"
    (let [cell (cell-012)]
      (is (thrown? AssertionError (m/fill-cell! cell 0 nil))))))

(deftest consume-cell
  (testing "with the owner"
    (let [cell (cell-012)
          _ (m/fill-cell! cell 0 :value)
          value (m/consume-cell! cell 0)]
      (testing "returns the value in the cell"
        (is (= value :value)))
      (testing "moves to the next owner"
        (is (= (:owner @cell) 1)))))

  (testing "with another key"
    (testing "returns nil"
      (let [cell (cell-012)
            _ (m/fill-cell! cell 0 :value)
            value (m/consume-cell! cell 1)]
        (is (nil? value)))))

  (testing "loops over the owners on consumption"
    (let [cell (cell-012)
          consume-and-get-owner #(do (m/fill-cell! cell % :value)
                                     (m/consume-cell! cell %)
                                     (:owner @cell))
          owners (->> (range 5) (map #(mod % 3)) (map consume-and-get-owner))]
      (is (= owners [1 2 0 1 2])))))

(deftest make-chans
  (testing "create the correct number of channels"
    (let [cs (m/make-chans (range 3))]
      (is (= (count cs) 3))))

  (testing "accepts input only for 'current' chan"
    (let [[c1 & _] (m/make-chans [:a :b :c])
          success (async/offer! c1 :value)]
      (is success "could not offer value")))

  (testing "rejects input for other chans"
    (let [[_ c2 c3] (m/make-chans [:a :b :c])]
      (is (not (async/offer! c2 :value)))
      (is (not (async/offer! c3 :value)))))

  (testing "cannot read from an empty 'current' chan"
    (let [[c1 & _] (m/make-chans [:a :b :c])]
      (is (not (async/poll! c1)) "unexpected read")
      (is (async/offer! c1 :value) "cannot write")
      (is (async/poll! c1) "cannot read")))

  (testing "rejects input for other chans"
    (let [[c1 c2 c3] (m/make-chans [:a :b :c])
          read-others #(do (is (not (async/offer! c2 :value)))
                           (is (not (async/offer! c3 :value))))]
      (read-others)
      (async/offer! c1 :value)
      (read-others)))

  (testing "can only use the chans sequentially"
    (let [chans (m/make-chans (range 3))]
      (doseq [[i c] (map vector (range 1) (cycle chans))]
        (is (async/offer! c i) "cannot write")
        (is (= (async/poll! c) i) "cannot read")))))

(clojure.test/run-tests *ns*)