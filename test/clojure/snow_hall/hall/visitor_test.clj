(ns snow-hall.hall.visitor-test
  (:require
    [clojure.test :refer [deftest testing is]]
    [snow-hall.hall.visitor :as m]))

(deftest create-registry []
  (testing "creates an empty user registry"
    (is (= (m/create-registry) {}))))

(deftest create []
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

(deftest on-visitor []
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

(deftest edit-visitor []
  (let [users (repeatedly 10 m/create)
        registry (reduce m/register (m/create-registry) users)
        user-5 (nth users 5)
        add-mark-action #(assoc % :marked true)]
    (testing "gives access to the wanted user"
      (let [updated-registry (m/edit
                              registry
                              (:uuid user-5)
                              (:token user-5)
                              add-mark-action)]
        (is (= (get-in updated-registry [(:uuid user-5) :marked]) true))))

    (testing "throws for non-existing user"
      (is (thrown?
           IllegalArgumentException
           (m/edit 
            registry 
            "something-wrong" 
            (:token user-5) 
            add-mark-action))))
    
    (testing "throws for an invalid token"
      (is (thrown?
           IllegalArgumentException
           (m/edit
            registry
            (:uuid user-5)
            "not-a-token"
            add-mark-action))))))
