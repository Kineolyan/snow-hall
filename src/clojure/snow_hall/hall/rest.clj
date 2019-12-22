(ns snow-hall.hall.rest
  (:require ;[snow-hall.hall.butler :as butler]
            [snow-hall.hall.visitor :as visitor]
            [compojure.core :as http]))

(defn list-users-request
  [registry _req]
  (let [visitors (vals @registry)
        cleansed-visitors (map #(dissoc % :token) visitors)]
    {:status  200
     :headers {"Content-Type" "application/json"}
     :body cleansed-visitors}))

(defn create-visitor
  [{:strs [nickname]}]
  (-> (visitor/create)
      (#(if nickname (visitor/set-nickname %1 nickname) %1))))

(defn register-visitor-request
  [registry req]
  (let [visitor (create-visitor (:body req))]
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
  (let [{:strs [token nickname]} (:body req)]
    (dosync
     (alter
      registry
      visitor/edit
      uuid
      token
      #(visitor/set-nickname % nickname)))
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body {:uuid uuid :nickname nickname}}))

(defn create-routes
  [visitors _tab]
  [
    (http/GET "/visitors" [] (partial list-users-request visitors))
    (http/POST "/visitors" [] (partial register-visitor-request visitors))
    (http/PUT "/visitors/:uuid/nickname" [uuid] (partial update-nickname-request visitors uuid))])
