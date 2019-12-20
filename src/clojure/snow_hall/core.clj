(ns snow-hall.core
  (:require
    [org.httpkit.server :as server]
    [compojure.core :as cmpj]
    [ring.middleware.defaults :as ring-defaults]
    [ring.middleware.json :as json]
    [snow-hall.games.manager :as game-mgr]
    [snow-hall.games.rest]
    [snow-hall.hall.butler]
    [snow-hall.hall.visitor]
    [snow-hall.hall.rest])
  (:gen-class))

(defn ping-request [_req]
  {:status  200
    :headers {"Content-Type" "text/plain"}
    :body "UP"})

(def basic-routes
  [
    (cmpj/GET "/ping" [] ping-request)])

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
    (apply cmpj/routes (concat
                    basic-routes
                    (snow-hall.games.rest/create-routes game-store)
                    (snow-hall.hall.rest/create-routes visitor-registry hall-tab)))))
(prn (create-app-routes))

(def app-site-config
  (update-in ring-defaults/site-defaults [:security] dissoc :anti-forgery))

(defn -main
  "Starts the Game Server"
  [& _args]
  (let [port (Integer/getInteger "PORT" 3000)
        app-routes (create-app-routes)]
    (server/run-server
     (-> app-routes
         (json/wrap-json-body)
         (json/wrap-json-response {:keywords? true :bigdecimals? true})
         (ring-defaults/wrap-defaults app-site-config))
     {:port port})
    (println (str "Running server at http:/127.0.0.1:" port "/"))))
