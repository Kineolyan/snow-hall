(ns snow-hall.hall.rest
  (:require [snow-hall.hall.butler :as butler]
            [snow-hall.hall.visitor :as visitor]
            [compojure.core :as http]
            [clojure.data.json :as json]))

(defn list-users-request [registry req]
  (let [visitors (vals @registry)
        cleansed-visitors (map #(dissoc :token visitors))]
    {:status  200
      :headers {"Content-Type" "text/plain"}
      :body (json/write-str visitors)}))

(defn create-routes
  [visitors tab]
  [
    (http/GET "/visitors" [] (partial list-users-request visitors))])