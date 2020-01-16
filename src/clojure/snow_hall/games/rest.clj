(ns snow-hall.games.rest
  (:require [snow-hall.games.manager :as mgr]
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

(defn create-routes
  [{:keys [games rounds]}]
  [(http/context "/games" []
     (http/GET "/" [] (partial list-games-request games)))
   (http/context "/rounds" []
     (http/GET "/" [] (partial list-round-request rounds)))])
