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

(defn wait-for-next-message
  [round player]
  (loop [i 0]
    (let [msgs [(s/get
                 (str "/rounds/" round "/messages")
                 {"Authorization" (auth-header player)})]]
      (is (>= 1 (count msgs)))
      (is (< i 100))
      (if (seq msgs)
        (first msgs)
        (do (Thread/sleep 10)
            (recur (inc i)))))))

(defn play
  "Plays for the player and checks the new state"
  [round player position next-state]
  (consume-all-messages round player)
  (s/post
   (str "/rounds/" round "/messages")
   {"user" (authenticate player)
    "move" position})
  (let [msg (wait-for-next-message round player)]
    (is (= (msg "content") next-state))))

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
         (let [{:keys [round creator guest]} @context]
           (play round creator [0 0] "X--------")))
   (step "play to guest victory"
         (let [{:keys [round creator guest]} @context]
           (play round guest [2 2] "X-------O")
           (play round creator [1 1] "X---X---O")
           (play round guest [2 1] "X---X--OO")
           (play round creator [0 2] "X-X--X--OO")
           (play round guest [2 0] "WIN")))
   (step "get final game state"
         (let [{:keys [round creator]} @context
               state (s/get
                      (str "/rounds/" round "/state")
                      {"Authorization" (auth-header creator)})]
           (is (= (state "content") "LOSS"))))))