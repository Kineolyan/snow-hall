(ns integration.common
  (:require [integration.story :as s]
            [clojure.test :refer [is]]))

(defn authenticate
  [user]
  {"uuid" (user "uuid")
   "token" (user "token")})

(defn auth-header
  [user]
  (str (user "uuid")
       ":"
       (user "token")))

(defn create-visitors
  "Creates a series of visitors and registers them to the server."
  [n]
  (repeatedly n #(s/post "/visitors")))

(defn create-gathering
  "Creates a new gathering from a creator and registers the other players."
  [{:keys [creator game-name others]}]
  (let [gathering  (s/post
                    "/gatherings"
                    {"user" (authenticate creator)
                     "game-id" game-name})
        _ (is gathering)
        tokens (->> (gathering "players")
                    (map #(get % "token"))
                    (filter identity))]
    (is (= (count tokens) (count others)))
    (doseq [[ guest token] (map vector others tokens)]
      (s/post
       (str "/gatherings/" (gathering "id"))
       {"user" (authenticate guest)
        "token" token}))
    ; returns the created gathering
    gathering))

(defn start-round
  "Starts a round from a gathering."
  [gathering creator]
  {:post [(is %)]}
  (s/post
   (str "/rounds/")
   {"user" (authenticate creator)
    "gathering" (gathering "id")}))

(defn consume-all-messages
  "Consumes all messages for a given player and returns them."
  [round player]
  (s/get
   (str "/rounds/" round "/messages")
   {"Authorization" (auth-header player)}))

(defn wait-for-next-messages
  "Waits for about 1s for n messages to be available for player in round."
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