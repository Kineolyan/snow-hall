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

; (defn create-game-store
;   []
;   (-> (game-mgr/create-store)
;       (game-mgr/add-game {:name "Code4Life"
;                           :java "code4life.Referee"
;                           :player-count 2})))
(defn create-game-store
  []
  (let [registry (game-mgr/create-store)]
    (game-mgr/add-game
     registry
     {:name "Code4Life"
      :java "code4life.Referee"
      :player-count 2})
    registry))

(defn create-visitor-registry
  []
  (ref (snow-hall.hall.visitor/create-registry)
       :validator snow-hall.hall.visitor/validate-fn))

(defn create-hall-tab
  []
  (ref (snow-hall.hall.butler/create-tab)
       :validator snow-hall.hall.butler/validate-fn))

(defn create-app-routes
  []
  (let [context {:games  (create-game-store)
                 :visitors (create-visitor-registry)
                 :tab (create-hall-tab)}]
    (apply cmpj/routes (concat
                        basic-routes
                        (snow-hall.games.rest/create-routes context)
                        (snow-hall.hall.rest/create-routes context)))))

(def app-site-config
  (update-in ring-defaults/site-defaults [:security] dissoc :anti-forgery))

(defn create-handler
  [app-routes]
  (-> app-routes
      (json/wrap-json-body)
      (json/wrap-json-response {:keywords? true :bigdecimals? true})
      (ring-defaults/wrap-defaults app-site-config)))

(defn start-server
  [port]
  (-> (create-app-routes)
      (create-handler)
      (server/run-server {:port port})))

(defn -main
  "Starts the Game Server"
  [& _args]
  (let [port (Integer/getInteger "PORT" 3000)]
    (start-server port)
    (println (str "Running server at http:/127.0.0.1:" port "/"))))
