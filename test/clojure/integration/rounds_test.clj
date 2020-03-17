(ns integration.rounds-test
  (:require [integration.story :as s :refer [story step]]
            [integration.common :refer [authenticate
                                        auth-header
                                        create-visitors
                                        create-gathering
                                        start-round]]
            [clojure.test :refer (is)]))

(story
 play-fake-game
 (let [context (atom {})]
   (step "create the users"
         (let [[u1 u2 & _r] (create-visitors 5)]
           (swap! context assoc
                  :creator u1
                  :guest u2)))
   (step "find games to play"
         (let [games (s/get "/games")
               one-game (first (filter #(= "Sample" (%1 "name")) games))]
           (is one-game)
           (swap! context assoc
                  :one-game one-game)))
   (step "create the gathering"
         (let [{:keys [creator one-game guest]} @context
               gathering (create-gathering {:creator creator
                                            :game-name (one-game "name")
                                            :others [guest]})]
           (swap! context assoc :gathering gathering)))
   (step "start the game"
         (let [{:keys [gathering creator guest]} @context
               round (start-round gathering creator)]
           (is (= (round "game") "Sample"))
           (is (= (round "players") (map #(get % "uuid") [creator guest])))
           (swap! context assoc :round (round "id"))))
   (step "play one turn"
         (let [{:keys [round creator guest]} @context]
           (s/post
            (str "/rounds/" round "/messages")
            {"user" (authenticate guest)
             "move" "IDLE"})
           (s/post
            (str "/rounds/" round "/messages")
            {"user" (authenticate creator)
             "move" "MOVE 1"})))
   (Thread/sleep 1000) ; wait for messages
   (step "collect messages"
         (let [{:keys [round creator guest]} @context]
           (let [[msg :as c-msgs] (s/get
                                   (str "/rounds/" round "/messages")
                                   {"Authorization" (auth-header creator)})]
             (is (= (count c-msgs) 1))
             (is (= (msg "content") 1)))
           (let [[msg :as g-msgs] (s/get
                                   (str "/rounds/" round "/messages")
                                   {"Authorization" (auth-header guest)})]
             (is (= (count g-msgs) 1))
             (is (= (msg "content") 2)))))
   (step "get game state"
         (let [{:keys [round creator]} @context
               state (s/get
                      (str "/rounds/" round "/state")
                      {"Authorization" (auth-header creator)})]
           (is (= (state "content") 1))))))
