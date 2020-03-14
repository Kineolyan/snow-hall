(ns integration.play-tic-tac-toe-test
  (:require [integration.story :as s :refer [story step]]
            [integration.common :refer [authenticate
                                        auth-header
                                        create-visitors
                                        create-gathering
                                        start-round]]
            [clojure.test :refer (is)]))

(defn consume-all-messages
  [round player]
  (s/get
   (str "/rounds/" round "/messages")
   {"Authorization" (auth-header player)}))

(defn wait-for-next-messages
  [round player n]
  (loop [i 0 received []]
    (let [msgs (s/get
                (str "/rounds/" round "/messages")
                {"Authorization" (auth-header player)})
          rcv (concat received msgs)]
      (when (>= i 100)
        (throw (AssertionError. (str "Too many iterations without all messages. Got: " rcv))))
      (condp #(%1 %2 n) (count rcv)
        = rcv
        > (throw (AssertionError. (str "Too many messages: " rcv)))
        < (do (Thread/sleep 10)
              (recur (inc i) rcv))))))

(defn play
  "Plays for the player and checks the new state"
  [round player position & expected-msgs]
  (consume-all-messages round player)
  (s/post
   (str "/rounds/" round "/messages")
   {"user" (authenticate player)
    "move" position})
  (let [msgs (wait-for-next-messages round player (count expected-msgs))]
    (is (= (map #(get % "content") msgs) expected-msgs))))

(story
 play-tic-tac-toe
 (let [context (atom {})]
   (step "create the users"
         (let [[u1 & r] (create-visitors 5)]
           (swap! context assoc
                  :creator u1
                  :guest (last r))))
   (step "create the gathering"
         (let [{:keys [creator guest]} @context
               gathering (create-gathering {:creator creator
                                            :game-name "Tic Tac Toe"
                                            :others [guest]})]
           (swap! context assoc :gathering gathering)))
   (step "start the game"
         (let [{:keys [gathering creator]} @context
               round (start-round gathering creator)]
           (swap! context assoc :round (round "id"))))
   (Thread/sleep 100) ; give some time for the initial messages
   (step "play one turn"
         (let [{:keys [round creator]} @context]
           (play round creator [0 0] "X--------")))
   (step "play to guest victory"
         (let [{:keys [round creator guest]} @context]
           (play round guest [2 2] "X-------O")
           (play round creator [1 1] "X---X---O")
           (play round guest [2 1] "X---X--OO")
           (play round creator [0 2] "X-X-X--OO")))
   (step "play victory move"
         (let [{:keys [round guest]} @context]
           (play round guest [2 0] "X-X-X-OOO" "WIN")))
   (step "get final game state"
         (let [{:keys [round creator]} @context
               state (s/get
                      (str "/rounds/" round "/state")
                      {"Authorization" (auth-header creator)})]
           (is (= (state "content") "LOSS"))))))