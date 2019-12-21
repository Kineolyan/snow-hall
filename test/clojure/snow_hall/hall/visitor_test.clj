(ns snow-hall.hall.visitor-test
  (:require
    [clojure.test :refer [deftest testing is]]
    [snow-hall.hall.visitor :as m]))

(deftest create-registry
  (testing "creates an empty user registry"
    (is (= (m/create-registry) {}))))

(deftest create
  (testing "creates user with all info"
    (let [user (m/create)]
      ; We have a UUID and a token but not a name yet
      (is ((comp not nil? :uuid) user))
      (is ((comp not nil? :token) user))
      (is ((comp nil? :name) user))))

  (testing "creates many different users"
    (let [users (repeatedly 10 m/create)
          ids (map :uuid users)]
      (is (= (count (set ids)) 10)))))

(deftest on-visitor
  (let [users (repeatedly 10 m/create)
        registry (reduce m/register (m/create-registry) users)
        user-5 (nth users 5)]
    (testing "gives access to the wanted user"
      (m/on
       registry
       (:uuid user-5)
       #(do 
          (is (= %2 user-5))
          (is (= %1 registry)))))
    
    (testing "throws for non-existing user"
      (is (thrown? 
           IllegalArgumentException
           (m/on registry "something-wrong" nil))))))

(deftest edit-visitor
  (let [users (repeatedly 10 m/create)
        registry (reduce m/register (m/create-registry) users)
        user-5 (nth users 5)]
    (testing "gives access to the wanted user"
      (m/on
       registry
       (:uuid user-5)
       #(do
          (is (= %2 user-5))
          (is (= %1 registry)))))

    (testing "throws for non-existing user"
      (is (thrown?
           IllegalArgumentException
           (m/on registry "something-wrong" nil))))))

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

