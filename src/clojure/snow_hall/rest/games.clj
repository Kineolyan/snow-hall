(ns snow-hall.rest.games
  (:require [compojure.core :as http]
            [snow-hall.games.manager :as mgr]
            [snow-hall.games.game :as game]))

(defn game->json
  "Exports a given game, dropping the unwanted attributes"
  [game]
  {:name (game/get-name game)
   :player-count (game/get-player-range game)})

(defn list-games-request [game-registry _req]
  (let [games (mgr/list-games @game-registry)
        output (map game->json games)]
    {:status 200
     :body output}))

(defn create-routes
  [{:keys [games]}]
  [(http/context "/games" []
     (http/GET "/" [] (partial list-games-request games)))])
