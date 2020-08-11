(ns snow-hall.rest.gatherings
  (:require [compojure.core :as http]
            [clojure.string :as str]
            [snow-hall.uuid :refer [->uuid]]
            [snow-hall.hall.butler :as butler]
            [snow-hall.hall.visitor :as visitor]
            [snow-hall.rest.core :refer [resolved rejected with]]))

(defn format-gathering
  [gathering]
  (-> gathering
      (update-in [:players] (partial map #(if (contains? %1 :token) nil %1)))))

(defn extract-user-info
  "Extracts the user info from headers or the payload."
  [req]
  (let [{:strs [uuid token]} (get-in req [:body "user"])
        header (get-in req [:headers "authorization"])]
    (cond
      (and uuid token) {:uuid uuid :token token}
      header (let [[uuid token] (str/split header #":")]
               {:uuid uuid :token token})
      :else nil)))

(defn with-visitor
  [visitors req & _]
  (let [{:keys [uuid token]} (extract-user-info req)
        v (visitors uuid)]
    (cond
      (nil? v) (rejected {:status 401})
      (visitor/validate-token v token) (resolved v)
      :else (rejected {:status 403}))))

; TODO edit to have a getter instead of always reading it from the req
(defn with-game
  [games req & _]
  (if-let [game (get games (get-in req [:body "game-id"]))]
    (resolved game)
    (rejected {:status 404})))

(defn with-gathering
  [tab guid-getter & _]
  (if-let [gathering (get tab (guid-getter))]
    (resolved gathering)
    (rejected {:status 404})))

(defn with-options
  [req & _] 
  (resolved (or (get-in req [:body "options"]) {})))

(defn list-gathering-request
  [{:keys [tab]} _req]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (map format-gathering (vals @tab))})

(defn do-create-gathering
  [tab visitor game options]
  (let [new-gathering (butler/create-gathering
                       {:tab @tab
                        :user visitor
                        :game game
                        :user-options options})]
    (alter tab butler/register-gathering new-gathering)
    (println (str "created: " new-gathering))
    {:status 200
     :body new-gathering}))

(defn create-gathering-request
  [{:keys [visitors tab games]} req]
  (with
   {:visitor (partial with-visitor @visitors req)
    :game (partial with-game @games req)
    :options (partial with-options req)}
   (fn [{:keys [visitor game options]}]
     (dosync 
      (do-create-gathering tab visitor game options)))))

(defn get-invit-list
  [gathering visitor]
  (let [players (:players gathering)
        owner-id (first players)
        visitor-id (:uuid visitor)]
    (if (= owner-id visitor-id)
      {:status 200
       :body players}
      {:status 403})))

(defn list-invit-request
  [{:keys [visitors tab]} guid req]
  (with
   {:visitor (partial with-visitor @visitors req)
    :gathering (partial with-gathering @tab (constantly guid))}
   (fn [{:keys [visitor gathering]}]
     (get-invit-list gathering visitor))))

(defn do-join-gathering
  [tab gathering visitor token]
  (alter tab #(butler/join-gathering {:tab %
                                      :user visitor
                                      :gathering-id (:id gathering)
                                      :token token}))
  {:status 200})

(defn join-gathering-request
  [{:keys [visitors tab]} guid req]
  (with
   {:visitor (partial with-visitor @visitors req)
    :gathering (partial with-gathering @tab (constantly guid))}
   (fn [{:keys [visitor gathering]}]
     (let [token (->uuid (get-in req [:body "token"]))]
       (dosync 
        (do-join-gathering tab gathering visitor token))))))

(defn create-routes
  [context]
  [(http/context "/gatherings" []
     (http/GET "/" [] (partial list-gathering-request context))
     (http/POST "/" [] (partial create-gathering-request context))
     (http/context "/:guid" [guid]
       (http/GET "/invits" [] (partial list-invit-request context guid))
       (http/POST "/" [] (partial join-gathering-request context guid))))])
