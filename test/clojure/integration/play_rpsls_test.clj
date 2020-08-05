(ns integration.play-rpsls-test
  (:require [integration.story :as s :refer [story step]]
            [integration.common :refer [authenticate
                                        auth-header
                                        create-visitors
                                        create-gathering
                                        start-round
                                        consume-all-messages
                                        wait-for-next-messages]]
            [clojure.test :refer (is)]))

(defn play
  "Plays a round for Rock-Paper-Scissors."
  [round moves]
  (doseq [[player move] moves]
    (consume-all-messages round player)
    (s/post
     (str "/rounds/" round "/messages")
     {"user" (authenticate player)
      "move" move})))

(defn check
  "Checks the output of a round of Rock-Paper-Scissors."
  [round players result]
  (doseq [player players]
    (let [msgs (wait-for-next-messages round player 1)]
      (is (= (map #(get % "content") msgs) (list result))))))

(defn play-and-check
  "Plays a round for Rock-Paper-Scissors and checks the output."
  [round moves result]
  (play round moves)
  (check round (keys moves) result))

(defn get-state
  [round player]
  (s/get
   (str "/rounds/" round "/state")
   {"Authorization" (auth-header player)}))

(story
 play-rock-paper-scissors-lizard-spock
 (let [context (atom {})]
   (step "create the users"
         (let [[u1 & r] (create-visitors 5)]
           (swap! context assoc
                  :creator u1
                  :guest (last r))))
   (step "create the gathering"
         (let [{:keys [creator guest]} @context
               gathering (create-gathering {:creator creator
                                            :game-name "Rock Paper Scissors Lizard Spock"
                                            :others [guest]})]
           (swap! context assoc :gathering gathering)))
   (step "start the game"
         (let [{:keys [gathering creator guest]} @context
               round (start-round gathering creator)
               round-id (round "id")]
           (swap! context assoc :round round-id)
           (wait-for-next-messages round-id creator 1)
           (wait-for-next-messages round-id guest 1)))
   (step "play one turn"
         (let [{:keys [round creator guest]} @context]
           (play-and-check round {creator "rock" guest "paper"} "0|1;rock|paper")))
   (step "play to guest victory"
         (let [{:keys [round creator guest]} @context]
          (play-and-check round {creator "rock" guest "rock"} "0|1;rock|rock")
           (play-and-check round {creator "spock" guest "rock"} "1|1;spock|rock")
           (play-and-check round {creator "spock" guest "paper"} "1|2;spock|paper")
           (play-and-check round {creator "spock" guest "paper"} "1|3;spock|paper")
           (play-and-check round {creator "spock" guest "paper"} "1|4;spock|paper")))
   (step "play victory move"
         (let [{:keys [round creator guest]} @context]
           (play round {creator "scissors" guest "spock"})))
   (step "get final game state"
         (let [{:keys [round creator guest]} @context
               creator-state (get-state round creator)
               guest-state (get-state round guest)]
           (is (= (creator-state "content") "LOSS BY POINTS"))
           (is (= (guest-state "content") "WIN BY POINTS"))))))

(comment 
  (do (integration.story/clean-server)
      (clojure.test/run-tests 'integration.play-rpsls-test)))