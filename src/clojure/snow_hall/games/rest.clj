(ns snow-hall.games.rest
  (:require [snow-hall.uuid :refer [->uuid]]
            [snow-hall.games.manager :as mgr]
            [snow-hall.games.round :as rounds]
            [compojure.core :as http]))

(defn list-games-request [game-registry _req]
  (let [games (mgr/list-games @game-registry)]
    {:status 200
     :body games}))

(defn list-round-request
  [rounds _req]
  (let [content (map #(hash-map :id (:ruid %)
                                :players (:players %)
                                :game (:game %))
                     (vals rounds))]
    {:status 200
     :body content}))

(defn start-round-request
  [_rounds _gatherings _req]
  {:status 501
   :body "Not implemented"})

(defn get-state-request
  [_rounds _visitors _ruid _uuid _req]
  {:status 501
   :body "Not implemented"})

(defn list-messages-request
  [_rounds _visitors _ruid _uuid _req]
  {:status 501
   :body "Not implemented"})

(defn play-request
  [_rounds _visitors _ruid _uuid _req]
  {:status 501
   :body "Not implemented"})

(defn create-routes
  [{:keys [games rounds gatherings visitors]}]
  [(http/context "/games" []
     (http/GET "/" [] (partial list-games-request games)))
   (http/context "/rounds" []
     (http/GET "/" [] (partial list-round-request rounds))
     (http/POST "/" [] (partial start-round-request rounds gatherings))
     (http/context "/:ruid" [ruid]
       (http/context "/:uuid" [uuid]
         (http/GET "/state" [] (partial get-state-request
                                        rounds
                                        visitors
                                        (->uuid ruid)
                                        (->uuid uuid)))
         (http/GET "/messages" [] (partial list-messages-request
                                           rounds
                                           visitors
                                           (->uuid ruid)
                                           (->uuid uuid)))
         (http/POST "/messages" [] (partial play-request
                                            rounds
                                            visitors
                                            (->uuid ruid)
                                            (->uuid uuid))))))])
