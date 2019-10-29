(ns snow-hall.hall.visitor-test
  (:require
    [clojure.test :refer :all]
    [snow-hall.hall.visitor :as m]))

(deftest create-registry
  (testing "creates an empty user registry"
    (is (= (m/create-registry) {}))))

(deftest create-new-user
  (testing "creates user with all info"
    (let [user (m/create-new-user)]
      ; We have a UUID and a token but not a name yet
      (is ((comp not nil? :uuid) user))
      (is ((comp not nil? :token) user))
      (is ((comp nil? :name) user))))

  (testing "creates many different users"
    (let [users (repeatedly 10 m/create-new-user)
          ids (map :uuid users)]
      (is (= (count (set ids)) 10)))))

(deftest set-nickname
  (testing "sets once the nickname of the user"
    (let [id "user-id"
          registry {id {:uuid id}}
          updated (m/set-nickname registry id "oli")
          complete-user (updated id)]
      (is (= (:nickname complete-user) "oli"))))

  (testing "can change the nickname of the user"
    (let [id "user-id"
          registry {id {:uuid id :nickname "oli"}}
          updated (m/set-nickname registry id "ilo")
          updated-user (updated id)]
      (is (= (:nickname updated-user) "ilo"))))

  (testing "throws if the user does not exist"
    (is (thrown? IllegalArgumentException (m/set-nickname {} "id" "me")))))

