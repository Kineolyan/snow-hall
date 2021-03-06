(ns integration.gatherings-test
  (:require [integration.story :as s :refer [story step]]
            [clojure.test :refer (is)]))

(story
 create-gathering
 (let [context (atom {})]
   (step "create the users"
         (let [[u1 u2 & _r] (repeatedly 5 #(s/post "/visitors"))]
           (swap! context assoc
                  :creator u1
                  :guest u2)))
   (step "find games to play"
         (let [games (s/get "/games")
               one-game (first (filter #(= 2 (get-in %1 ["player-count" "exact"])) games))]
           (is one-game)
           (swap! context assoc
                  :one-game one-game)))
   (step "create the gathering"
         (let [{:keys [creator one-game]} @context
               gathering (s/post
                          "/gatherings"
                          {"user" {"uuid" (creator "uuid")
                                   "token" (creator "token")}
                           "game-id" (one-game "name")})]
           (is (some? gathering))
           (swap! context assoc :gathering gathering)))
   (step "finds invits"
         (let [{:keys [gathering creator]} @context
               invits (s/get (str "/gatherings/" (gathering "id") "/invits")
                             {"Authorization" (str (creator "uuid") 
                                                   ":" 
                                                   (creator "token"))})]
           (is (= (gathering "players") invits))))
   (step "register another user"
         (let [{:keys [gathering guest]} @context]
           (s/post 
            (str "/gatherings/" (gathering "id"))
            {"user" {"uuid" (guest "uuid")
                     "token" (guest "token")}
             "token" (-> gathering 
                         (get "players") 
                         (second) 
                         (get "token"))})
           ))
   (step "checks for full game"
         (let [{:keys [gathering creator]} @context
               invits (s/get (str "/gatherings/" (gathering "id") "/invits")
                             {"Authorization" (str (creator "uuid") 
                                                   ":" 
                                                   (creator "token"))})]
           (is (every? string? invits))))))