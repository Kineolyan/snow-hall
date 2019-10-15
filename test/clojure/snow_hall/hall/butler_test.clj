(ns snow-hall.hall.butler-test
  (:require
    [clojure.test :refer :all]
    [snow-hall.hall.butler :refer m]))

(deftest generate-id
  (testing "can generate ids without collision"
    (let [tab {}
          ids (repeatedly ())])
    (is (= 0 1))))

