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

(deftest check-guards
  (let [f #'m/check-guards
        values {:a 1}
        is-ok (constantly nil)
        a-is-neg? #(when-not (neg? (:a %)) "neg")
        a-is-zero? #(when-not (zero? (:a %)) "zero")]
    (testing "returns first failure"
      (is (= (rejected "neg") (f [is-ok a-is-neg? a-is-zero?] (resolved values)))))
    (testing "returns error of failure - start"
      (is (= (rejected "zero") (f [a-is-zero? is-ok] (resolved values)))))
    (testing "returns error of failure - middle"
      (is (= (rejected "zero") (f [is-ok a-is-zero? is-ok] (resolved values)))))
    (testing "returns error of failure - start"
      (is (= (rejected "zero") (f [is-ok is-ok a-is-zero?] (resolved values)))))))

(deftest with []
  (let [get-a (constantly (resolved 1))
        get-b (comp resolved inc :a)
        get-c (constantly (resolved "c"))]
    (testing "single resolution"
      (let [result (m/with {:c get-c})]
        (is (= result {:c "c"}))))
    (testing "multiple resolution"
      (let [result (m/with {:a get-a
                            :b get-a
                            :c get-c})]
        (is (= result {:a 1 :b 1 :c "c"}))))
    (testing "multiple resolution in order"
      (let [result (m/with [[:a get-a]
                            [:b get-a]
                            [:c get-c]])]
        (is (= result {:a 1 :b 1 :c "c"}))))
    (testing "rejection at start"
      (let [do-fail #(if (seq %) (resolved "ok") (rejected "ko"))]
        (is (= (m/with [[:d do-fail]
                        [:a get-a]])
               "ko"))))
    (testing "rejection in the middle"
      (let [do-fail #(if (and (= (:a %) 1) (= 1 (count %)))
                       (rejected "ko")
                       (resolved "ok"))]
        (is (= (m/with [[:a get-a]
                        [:d do-fail]
                        [:b get-b]])
               "ko"))))
    (testing "rejecion at the end"
      (let [do-fail #(if (= 2 (count %))
                       (rejected "ko")
                       (resolved "ok"))]
        (is (= (m/with [[:a get-a]
                        [:b get-b]
                        [:d do-fail]])
               "ko"))))
    (testing "returns the resolved values to cbk"
      (let [r (ref nil)]
        (m/with {:a get-a} #(dosync (ref-set r %)))
        (is (= {:a 1} @r))))))

(deftest checked-with
  (let [get-1 (constantly (resolved 1))
        get-0 (constantly (resolved 0))
        is-a-pos? #(when-not (pos? (:a %)) "Not pos")
        is-b-zero? #(when-not (zero? (:b %)) "Not zero")]
    (testing "resolution with passing checks"
      (is (= (m/checked-with {:a get-1
                              :b get-0}
                             [is-a-pos? is-a-pos?])
             {:a 1 :b 0})))
    (testing "failure on first failed check"
      (is (= (m/checked-with {:a get-0 :b get-1}
                             [is-b-zero? is-a-pos?])
             "Not zero")))
    (testing "failure after various successes"
      (is (= (m/checked-with {:a get-1 :b get-1}
                             [is-a-pos? is-a-pos? is-b-zero?])
             "Not zero")))))
