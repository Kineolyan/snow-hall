(ns integration.rounds
  (:require [integration.story :as s :refer [story step]]
            [clojure.test :refer (is)]))

(defn authenticate
  [user]
  {"uuid" (user "uuid")
   "token" (user "token")})

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
               one-game (first (filter #(= 2 (%1 "player-count")) games))]
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
           (is (= (round "players") (map #(get % "token") [creator guest])))
           (swap! context assoc :round round)))))
