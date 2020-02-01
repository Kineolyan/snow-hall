(ns snow-hall.rest.games
  (:require [compojure.core :as http]
            [snow-hall.games.manager :as mgr]))

(defn list-games-request [game-registry _req]
  (let [games (mgr/list-games @game-registry)]
    {:status 200
     :body games}))

(defn create-routes
  [{:keys [games]}]
  [(http/context "/games" []
     (http/GET "/" [] (partial list-games-request games)))])
