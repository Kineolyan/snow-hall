(ns snow-hall.games.rest
  (:require [snow-hall.games.manager :as mgr]
            [compojure.core :as http]))

(defn list-games-request [req]
  {:status  200
    :headers {"Content-Type" "text/plain"}
    :body "No game"})

(def api-routes
  [
    (http/GET "/games" [] list-games-request)])
