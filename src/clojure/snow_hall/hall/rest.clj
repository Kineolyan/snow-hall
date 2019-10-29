(ns snow-hall.hall.rest
  (:require [snow-hall.hall.butler :as butler]
            [snow-hall.hall.visitor :as visitor]
            [compojure.core :as http]
            [clojure.data.json :as json]))

(defn list-users-request
  [registry req]
  (let [visitors (vals @registry)
        cleansed-visitors (map #(dissoc :token visitors))]
    {
      :status  200
      :headers {"Content-Type" "application/json"}
      :body (json/write-str visitors)}))

(defn register-visitor-request
  [registry req]
  (let [visitor (visitor/create-new-user)]
        ; at this point, we could also read the request to get user info
        ; nickname, ...
    (dosync
      (alter registry visitor/register-user visitor))
    {
      :status 200
      :headers {"Content-Type" "application/json"}
      :body (json/write-str visitor)}))

(defn update-nickname-request
  [registry req]
  {
    :status 500
    :headers {"Content-Type" "application/json"}
    :body (json/write-str "Not implemented")})

(defn create-routes
  [visitors tab]
  [
    (http/GET "/visitors" [] (partial list-users-request visitors))
    (http/POST "/visitors" [] (partial register-visitor-request visitors))
    (http/PUT "/visitors/uuid/nickname" [] (partial update-nickname-request visitors))])
