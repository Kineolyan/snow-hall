(ns snow-hall.rest.games
  (:require [compojure.core :as http]
            [snow-hall.games.manager :as mgr]))

(defn game->json 
  "Exports a given game, dropping the unwanted attributes"
  [game]
  (into {} (filter (comp #{:name :player-count} first) game)))

(defn list-games-request [game-registry _req]
  (let [games (mgr/list-games @game-registry)
        output (map game->json games)]
    {:status 200
     :body output}))

(defn create-routes
  [{:keys [games]}]
  [(http/context "/games" []
     (http/GET "/" [] (partial list-games-request games)))])

(comment
  (def some-game {:name "A" :player-count 1 :other "<hidden>"})
  (game->json some-game))
