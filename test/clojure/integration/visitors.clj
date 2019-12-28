(ns integration.visitors
  (:require [integration.story :as s]
            [clojure.test :refer (deftest is)]))

(s/story
 register-and-udpate
 (let [context (atom {})]
   (s/step "register a first user"
           (let [user1 (s/post "/visitors")]
             (is (contains? user1 "uuid"))
             (is (contains? user1 "token"))
             (swap! context assoc :user1 user1)))
   (s/step "register a second user"
           (let [user2 (s/post "/visitors" {"nickname" "me"})]
             (is (= (get user2 "nickname") "me"))
             (swap! context assoc :user2 user2)))
   (s/step "list both users"
           (let [visitors (s/get "/visitors")
                 uuids (map #(get % "uuid") visitors)
                 {:keys [user1 user2]} @context]
             (doseq [user [user1 user2]]
               (is (some #{(get user "uuid")} uuids)))))
   (s/step "update nickname of first user"
           (let [{:strs [uuid token]} (:user1 @context)]
             (s/put (str "/visitors/" uuid "/nickname")
                    {"token" token
                     "nickname" "georges"}))
           (let [visitors (s/get "/visitors")
                 u1-id (get-in @context [:user1 "uuid"])
                 u1-again (first (filter #(= (get % "uuid") u1-id) visitors))]
             (is (= (u1-again "nickname") "georges"))))))
