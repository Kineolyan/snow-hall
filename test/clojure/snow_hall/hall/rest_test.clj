(ns snow-hall.hall.rest-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [snow-hall.hall.visitor :as visitor]
   [snow-hall.rest.visitors :as m]))

(defn request
  [body]
  {:body body})

(deftest register-visitor []
  (let [registry (ref (visitor/create-registry))
        answer (m/register-visitor-request
                registry
                (request {"nickname" "me"}))]
    (testing "adds a new visitor to the registry"
      (is (= (->> @registry (vals) (map :nickname))
             ["me"])))
    (testing "returns the uuid of the new visitor"
      (is (contains?
           @registry
           (get-in answer [:body :uuid]))))
    (testing "returns the token of the visitor"
      (let [{:keys [token uuid]} (:body answer)]
        (is (= token
               (get-in @registry [uuid :token])))))
    (testing "returns the nickname of the visitor"
      (let [{:keys [uuid nickname]} (:body answer)]
        (is (= nickname
               (get-in @registry [uuid :nickname])))))))
