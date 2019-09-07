(ns snow-hall.core
  (:require
    [org.httpkit.server :as server]
    [compojure.core :refer :all]
    [compojure.route :as route]
    [ring.middleware.defaults :refer :all]
    [clojure.string :as str]
    [clojure.data.json :as json]
    [snow-hall.games.rest :as games])
  (:gen-class))

(defn ping-request [req]
  {:status  200
    :headers {"Content-Type" "text/plain"}
    :body "UP"})

(def test-routes
  [
    (GET "/ping" [] ping-request)
    (GET "/wtf" [] ping-request)])

(def app-routes
  (apply routes (concat
                  test-routes
                  games/api-routes)))

(defn -main
  "Starts the Game Server"
  [& args]
  (let [port (Integer/getInteger "PORT" 3000)]
    (server/run-server (wrap-defaults #'app-routes site-defaults) {:port port})
    (println (str "Running server at http:/127.0.0.1:" port "/"))))
