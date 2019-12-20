(ns snow-hall.games.rest
  (:require [snow-hall.games.manager :as mgr]
            [compojure.core :as http]))

(defn list-games-request [game-registry _req]
  (let [games (mgr/list-games game-registry)]
    {:status  200
      :headers {"Content-Type" "application/json"}
      :body games}))

(defn create-routes
  [store]
  [
    (http/GET "/games" [] (partial list-games-request store))])
