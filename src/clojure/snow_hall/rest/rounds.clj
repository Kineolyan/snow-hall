(ns snow-hall.rest.rounds
  (:require [compojure.core :as http]
            [snow-hall.uuid :refer [->uuid]]
            [snow-hall.games.round :as rounds]
            [snow-hall.rest.gatherings :refer [with-visitor]]))

(defn with-round
  [rounds ruid-getter action]
  (let [ruid (ruid-getter)
        round (get rounds ruid)]
    (if round
      (action round)
      {:status 404
       :body (str "No round " (str ruid))})))

(defn list-round-request
  [rounds _req]
  (let [content (map #(hash-map :id (:ruid %)
                                :players (:players %)
                                :game (:game %))
                     (vals rounds))]
    {:status 200
     :body content}))

(defn start-round-request
  [rounds gatherings _visitors req]
  (let [guid (get-in req [:body "gathering"])
        gathering (get @gatherings guid)
        created-round (rounds/create-round gathering)]
    (dosync
     (alter rounds assoc (:ruid created-round) created-round))
    {:status 200
     :body (-> created-round
               (dissoc :ruid :state :engine)
               (assoc :id (:ruid created-round)))}))

(defn get-state-request
  [rounds visitors ruid req]
  (with-visitor
    @visitors
    req
    (fn [visitor]
      (with-round
        @rounds
        (constantly ruid)
        (fn [round]
          (let [state (rounds/read-last-state round (:uuid visitor))]
            {:status 200
             :body state}))))))

(defn list-messages-request
  [rounds visitors ruid req]
  (with-visitor 
    @visitors
    req
    (fn [visitor]
      (with-round
        @rounds
        (constantly ruid)
        (fn [round]
          (let [messages (rounds/read-messages round (:uuid visitor))]
            {:status 200
             :body messages}))))))

(defn play-request
  [rounds visitors ruid req]
  (let [round (get @rounds ruid)
        move (get-in req [:body "move"])]
    (with-visitor
      @visitors
      req
      #(rounds/play-round round (:uuid %) move))
    {:status 200
     :body "Ok"}))

(defn create-routes
  [{:keys [rounds tab visitors]}]
  [(http/context "/rounds" []
     (http/GET "/" [] (partial list-round-request rounds))
     (http/POST "/" [] (partial start-round-request rounds tab visitors))
     (http/context "/:ruid" [ruid]
       (http/GET "/state" [] (partial get-state-request
                                      rounds
                                      visitors
                                      (->uuid ruid)))
       (http/GET "/messages" [] (partial list-messages-request
                                         rounds
                                         visitors
                                         (->uuid ruid)))
       (http/POST "/messages" [] (partial play-request
                                          rounds
                                          visitors
                                          (->uuid ruid)))))])
