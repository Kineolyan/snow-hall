(ns snow-hall.rest.gatherings
  (:require [snow-hall.hall.butler :as butler]
            [snow-hall.hall.visitor :as visitor]
            [compojure.core :as http]
            [clojure.string :as str]
            [snow-hall.uuid :refer [->uuid]]))

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
  [visitors req action]
  (let [{:keys [uuid token]} (extract-user-info req)
        v (visitors uuid)]
    (cond
      (nil? v) {:status 401}
      (visitor/validate-token v token) (action v)
      :else {:status 403})))

; TODO edit to have a getter instead of always reading it from the req
(defn with-game
  [req games action]
  (if-let [game (get games (get-in req [:body "game-id"]))]
    (action game)
    {:status 404}))

(defn with-gathering
  [tab guid-getter action]
  (if-let [gathering (get tab (guid-getter))]
    (action gathering)
    {:status 404}))

(defn list-gathering-request
  [tab _req]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (map format-gathering (vals @tab))})

(defn do-create-gathering
  [tab visitor game]
  (let [new-gathering (butler/create-gathering
                       {:tab @tab
                        :user visitor
                        :game game})]
    (alter tab butler/register-gathering new-gathering)
    {:status 200
     :body new-gathering}))

(defn create-gathering-request
  [visitors tab games req]
  (dosync
   (with-visitor
     @visitors
     req
     (fn [visitor]
       (with-game
         req
         @games
         (fn [game] (do-create-gathering tab visitor game)))))))

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
  [visitors tab guid req]
  (with-visitor
    @visitors
    req
    (fn [visitor]
      (with-gathering
        @tab
        (constantly guid)
        (fn [gathering] (get-invit-list gathering visitor))))))

(defn do-join-gathering
  [tab gathering visitor token]
  (alter tab #(butler/join-gathering {:tab %
                                      :user visitor
                                      :gathering-id (:id gathering)
                                      :token token}))
  {:status 200})

(defn join-gathering-request
  [visitors tab guid req]
  (let [token (->uuid (get-in req [:body "token"]))]
    (dosync
     (with-visitor
       @visitors
       req
       (fn [visitor]
         (with-gathering
           @tab
           (constantly guid)
           (fn [gathering]
             (do-join-gathering tab gathering visitor token))))))))

(defn create-routes
  [{:keys [visitors tab games]}]
  [(http/context "/gatherings" []
     (http/GET "/" [] (partial list-gathering-request tab))
     (http/POST "/" [] (partial create-gathering-request visitors tab games))
     (http/context "/:guid" [guid]
       (http/GET "/invits" [] (partial list-invit-request visitors tab guid))
       (http/POST "/" [] (partial join-gathering-request visitors tab guid))))])
