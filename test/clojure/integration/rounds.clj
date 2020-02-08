(ns integration.rounds
  (:require [integration.story :as s :refer [story step]]
            [clojure.test :refer (is)]))

(defn authenticate
  [user]
  {"uuid" (user "uuid")
   "token" (user "token")})

(defn auth-header
  [user]
  (str (user "uuid")
       ":"
       (user "token")))

(story
 play-fake-game
 (let [context (atom {})]
   (step "create the users"
         (let [[u1 u2 & _r] (repeatedly 5 #(s/post "/visitors"))]
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
         (let [{:keys [creator one-game]} @context
               gathering (s/post
                          "/gatherings"
                          {"user" (authenticate creator)
                           "game-id" (one-game "name")})]
           (is gathering)
           (swap! context assoc :gathering gathering)))
   (step "register second user"
         (let [{:keys [gathering guest]} @context]
           (s/post
            (str "/gatherings/" (gathering "id"))
            {"user" (authenticate guest)
             "token" (-> gathering
                         (get "players")
                         (second)
                         (get "token"))})))
   (step "start the game"
         (let [{:keys [gathering creator guest]} @context
               round (s/post
                      (str "/rounds/")
                      {"user" (authenticate creator)
                       "gathering" (gathering "id")})]
           (is round)
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
