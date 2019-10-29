(ns snow-hall.core
  (:require
    [org.httpkit.server :as server]
    [compojure.core :refer :all]
    [compojure.route :as route]
    [ring.middleware.defaults :refer :all]
    [clojure.string :as str]
    [clojure.data.json :as json]
    [snow-hall.games.manager :as game-mgr]
    [snow-hall.games.rest]
    [snow-hall.hall.butler]
    [snow-hall.hall.visitor]
    [snow-hall.hall.rest])
  (:gen-class))

(defn ping-request [req]
  {:status  200
    :headers {"Content-Type" "text/plain"}
    :body "UP"})

(def test-routes
  [
    (GET "/ping" [] ping-request)
    (GET "/wtf" [] ping-request)])

(defn create-game-store
  []
  (let [registry (game-mgr/create-store)]
    (do
      (game-mgr/add-game
        registry
        {
          :name "Code4Life"
          :java "code4life.Referee"
          :player-count 2})
      registry)))

(defn create-visitor-registry
  []
  (ref (snow-hall.hall.visitor/create-registry)))

(defn create-hall-tab
  []
  (ref (snow-hall.hall.butler/create-tab)))

(defn create-app-routes
  []
  (let [game-store (create-game-store)
        visitor-registry (create-visitor-registry)
        hall-tab (create-hall-tab)]
    (apply routes (concat
                    test-routes
                    (snow-hall.games.rest/create-routes game-store)
                    (snow-hall.hall.rest/create-routes visitor-registry hall-tab)))))

(defn -main
  "Starts the Game Server"
  [& args]
  (let [port (Integer/getInteger "PORT" 3000)
        app-routes (create-app-routes)]
    (server/run-server
      (wrap-defaults app-routes site-defaults)
      {:port port})
    (println (str "Running server at http:/127.0.0.1:" port "/"))))
