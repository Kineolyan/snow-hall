(ns snow-hall.games.seq-chan-test
  (:require [clojure.test :refer [is deftest testing]]
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
