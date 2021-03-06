(ns snow-hall.games.round-test
  (:require [snow-hall.games.round :as m]
            [clojure.test :refer [is deftest testing]]))

(deftest clear-old-messages []
  (testing "with a list of messages"
    (let [uuid "abc"
          make-message #(hash-map :timestamp % :content %)
          initial-state @(m/create-state-agent [uuid])
          filled-state (update-in initial-state
                                  [:messages uuid]
                                  (constantly (map make-message [1 5 10 11 20])))
          next-state (m/clear-old-messages filled-state uuid 10)]
      (is (= (:messages next-state) {uuid (map make-message [11 20])}))))
  (testing "with empty list of messages"
    (let [uuid "abc"
          state @(m/create-state-agent [uuid])
          next-state (m/clear-old-messages state uuid 10)]
      (is (empty? (get-in next-state [:messages uuid])))))
  (testing "with cleared list of messages"
    (let [uuid "abc"
          make-message #(hash-map :timestamp % :content %)
          state @(m/create-state-agent [uuid])
          _ (update-in state
                       [:messages uuid]
                       (constantly (map make-message [1 5 10 11 20])))
          next-state (m/clear-old-messages state uuid 102)]
      (is (empty? (get-in next-state [:messages uuid]))))))

(deftest read-messages []
  (testing "without new messages"
    (let [uuid "abc"
          state (m/create-state-agent [uuid])
          round {:state state}
          messages (m/read-messages round uuid)]
      (is (= 0 (count messages)))))
  (testing "with something to read"
    (let [uuid "abc"
          state (m/create-state-agent [uuid])
          round {:state state}]
      ; init with some content 
      (m/send-message state uuid "msg-1")
      (m/send-message state uuid "msg-2")
      (await-for 1000 state)
      ; access the messages
      (let [messages (m/read-messages round uuid)
            contents (map :content messages)]
        (is (= contents ["msg-1" "msg-2"])))
      ; wait for the end of the cleaning
      (await-for 1000 state)
      (is (empty? (get-in @state [:messages uuid])))))
  (testing "after reading all messages"
    (let [uuid "abc"
          state (m/create-state-agent [uuid])
          round {:state state}]
      ; init with some content 
      (m/send-message state uuid "msg-1")
      (m/send-message state uuid "msg-2")
      (await-for 1000 state)
      ; access the messages
      (m/read-messages round uuid)
      ; wait for the end of the cleaning
      (await-for 1000 state)
      (is (empty? (m/read-messages round uuid)))))
  (testing "only affects provided uuid"
    (let [uuid "abc"
          other-uuid "cde"
          state (m/create-state-agent [uuid other-uuid])
          round {:state state}]
      ; init with some content 
      (m/send-message state uuid "msg-1")
      (m/send-message state other-uuid "msg-2")
      (await-for 1000 state)
      ; access the messages
      (let [snapshot (:messages @state)]
        (m/read-messages round uuid)
        (await-for 1000 state)
        (is (= (:messages @state)
               (update-in snapshot [uuid] (constantly []))))))))

(deftest send-message []
  (testing "sending on initial list"
    (let [uuid "abc"
          state (m/create-state-agent [uuid])
          round {:state state}]
      ; init with some content 
      (m/send-message state uuid "msg-1")
      (m/send-message state uuid "msg-2")
      (await-for 1000 state)
      ; access the messages
      (is (= (map :content (get-in @state [:messages uuid]))
             ["msg-1" "msg-2"]))))
  (testing "sending after reading messages"
    (let [uuid "abc"
          state (m/create-state-agent [uuid])
          round {:state state}]
      ; init with some content 
      (m/send-message state uuid "msg-1")
      (m/send-message state uuid "msg-2")
      (await-for 1000 state)
      ; access the messages
      (m/read-messages round uuid)
      (await-for 1000 state)
      ; send more messages
      (m/send-message state uuid "msg-3")
      (m/send-message state uuid "msg-4")
      (await-for 1000 state)
      (is (= (map :content (get-in @state [:messages uuid]))
             ["msg-3" "msg-4"])))))
