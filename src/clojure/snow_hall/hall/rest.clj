(ns snow-hall.hall.rest
  (:require [snow-hall.hall.butler :as butler]
            [snow-hall.hall.visitor :as visitor]
            [compojure.core :as http]))

(defn list-users-request
  [registry _req]
  (let [visitors (vals @registry)
        cleansed-visitors (map (partial dissoc :token) visitors)]
    {:status  200
     :headers {"Content-Type" "application/json"}
     :body cleansed-visitors}))

(defn register-visitor-request
  [registry _req]
  (let [visitor (visitor/create)]
        ; at this point, we could also read the request to get user info
        ; nickname, ...
    (dosync
      (alter registry visitor/register visitor))
    {
      :status 200
      :headers {"Content-Type" "application/json"}
      :body visitor}))

(defn update-nickname-request
  [registry uuid req]
  (let [{:keys [token nickname]} (:body req)]
    (dosync
     (alter 
      registry 
      (fn [r] (visitor/edit
               r 
               uuid 
               token 
               #(assoc % :nickname nickname)))))
    {:status 500
     :headers {"Content-Type" "application/json"}
     :body "Not implemented"}))

(defn create-routes
  [visitors _tab]
  [
    (http/GET "/visitors" [] (partial list-users-request visitors))
    (http/POST "/visitors" [] (partial register-visitor-request visitors))
    (http/PUT "/visitors/:uuid/nickname" [uuid] (partial update-nickname-request visitors uuid))])
