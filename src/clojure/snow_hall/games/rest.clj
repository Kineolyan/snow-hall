(ns snow-hall.games.rest
  (:require [snow-hall.games.manager :as mgr]
            [compojure.core :as http]
            [clojure.data.json :as json]))

(defn list-games-request [game-registry req]
  (let [games (mgr/list-games game-registry)
        body (json/write-str games)]
    {:status  200
      :headers {"Content-Type" "text/plain"}
      :body body}))

(defn create-routes
  [store]
  [
    (http/GET "/games" [] (partial list-games-request store))])
