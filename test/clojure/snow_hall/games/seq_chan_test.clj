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

(clojure.test/run-tests *ns*)