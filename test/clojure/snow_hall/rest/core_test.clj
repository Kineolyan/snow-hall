(ns snow-hall.rest.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [snow-hall.rest.core :as m :refer [resolved rejected]]))

(deftest resolve-component []
  (let [f #'m/resolve-component]
    (testing "resolve from closure"
      (let [result (f [nil {}] [:a (constantly (resolved 1))])]
        (is (= result (resolved {:a 1})))))
    (testing "resolve from previous context"
      (let [result (f [nil {:a 1}]
                      [:b (comp resolved inc :a)])]
        (is (= result (resolved {:a 1 :b 2})))))
    (testing "returns rejection instead of result"
      (let [error (rejected "Oops")
            result (f [nil {:a 1}]
                      [:c (constantly error)])]
        (is (= result error))))))

(deftest with []
  (let [get-a (constantly (resolved 1))
        get-b (comp resolved inc :a)
        get-c (constantly (resolved "c"))]
    (testing "single resolution"
      (let [result (m/with {:c get-c})]
        (is (= result (resolved {:c "c"})))))
    (testing "multiple resolution"
      (let [result (m/with {:a get-a
                            :b get-b
                            :c get-c})]
        (is (= result (resolved {:a 1 :b 2 :c "c"})))))
    (testing "rejection at start"
      (let [do-fail #(if (seq %) (resolved "ok") (rejected "ko"))]
        (is (= (m/with {:d do-fail
                        :a get-a})
               "ko"))))
    (testing "rejection in the middle"
      (let [do-fail #(if (and (= (:a %) 1) (= 1 (count %)))
                       (rejected "ko")
                       (resolved "ok"))]
        (is (= (m/with {:a get-a
                        :d do-fail
                        :b get-b})
               "ko"))))
    (testing "rejecion at the end"
      (let [do-fail #(if (= 2 (count %))
                      (rejected "ko")
                      (resolved "ok"))]
        (is (= (m/with {:a get-a
                        :b get-b
                        :d do-fail})
               "ko"))))))
