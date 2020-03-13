(ns snow-hall.core
  (:require
   [org.httpkit.server :as server]
   [compojure.core :as cmpj]
   [ring.middleware.defaults :as ring-defaults]
   [ring.middleware.json :as json]
   [ring.middleware.reload :as reload]
   [snow-hall.games.manager :as game-mgr]
   [snow-hall.games.round]
   [snow-hall.games.library.sample]
   [snow-hall.games.library.tic-tac-toe]
   [snow-hall.hall.butler]
   [snow-hall.hall.visitor]
   [snow-hall.rest.games]
   [snow-hall.rest.gatherings]
   [snow-hall.rest.rounds]
   [snow-hall.rest.visitors])
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
  (-> (game-mgr/create-store)
      (game-mgr/add-game
       {:name "Code4Life"
        :java "code4life.Referee"
        :player-count 2})
      (game-mgr/add-game snow-hall.games.library.sample/game-definition)
      (game-mgr/add-game snow-hall.games.library.tic-tac-toe/game-definition)
      (ref :validator snow-hall.games.manager/validate-fn)))

(defn create-visitor-registry
  []
  (ref (snow-hall.hall.visitor/create-registry)
       :validator snow-hall.hall.visitor/validate-fn))

(defn create-hall-tab
  []
  (ref (snow-hall.hall.butler/create-tab)
       :validator snow-hall.hall.butler/validate-fn))

(defn create-round-registry
  []
  (ref (snow-hall.games.round/create-store)
       :validator snow-hall.games.round/validate-fn))

(defn create-context
  []
  {:games  (create-game-store)
   :visitors (create-visitor-registry)
   :tab (create-hall-tab)
   :rounds (create-round-registry)})

(defn create-app-routes
  [context] 
  (apply cmpj/routes (concat
                      basic-routes
                      (snow-hall.rest.games/create-routes context)
                      (snow-hall.rest.gatherings/create-routes context)
                      (snow-hall.rest.rounds/create-routes context)
                      (snow-hall.rest.visitors/create-routes context))))

(def app-site-config
  (update-in ring-defaults/site-defaults [:security] dissoc :anti-forgery))

(defn create-handler
  [app-routes]
  (-> app-routes
      (json/wrap-json-body)
      (json/wrap-json-response {:keywords? true :bigdecimals? true})
      (ring-defaults/wrap-defaults app-site-config)))

(def dev-context nil)
(defn- save-context
  [ctx]
  (alter-var-root #'dev-context (constantly ctx)))
(defn start-server
  [port dev]
  (-> (create-context)
      (#(do 
          (when dev (save-context %))
          %))
      create-app-routes
      create-handler
      (#(if dev (reload/wrap-reload %) %))
      (server/run-server {:port port})
      (#(fn [] 
          (when dev (save-context nil))
          (shutdown-agents)
          (%)))))

(defn -main
  "Starts the Game Server"
  [& _args]
  (let [port (Integer/getInteger "PORT" 3000)]
    (start-server port false)
    (println (str "Running server at http:/127.0.0.1:" port "/"))))
