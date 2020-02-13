(ns snow-hall.rest.rounds
  (:require [compojure.core :as http]
            [snow-hall.uuid :refer [->uuid]]
            [snow-hall.games.round :as rounds]
            [snow-hall.rest.core :refer [checked-with with resolved rejected]]
            [snow-hall.rest.gatherings :refer [with-visitor]]))

(defn with-round
  [rounds ruid-getter]
  (let [ruid (ruid-getter)
        round (get rounds ruid)]
    (if round
      (resolved round)
      (rejected {:status 404
                 :body (str "No round " (str ruid))}))))

(defn with-gathering
  [gatherings guid-getter _]
  (let [guid (guid-getter)
        gathering (get gatherings guid)]
    (if guid
      (resolved gathering)
      (rejected {:status 404
                 :body (str "No gathering " (str guid))}))))

(defn with-game
  [games name-getter]
  (let [name (name-getter)
        game (get games name)]
    (if game
      (resolved game)
      (rejected {:status 404
                 :body (str "No game " (str name))}))))

(defn int-with-visitor
  [visitors req _]
  (let [result (with-visitor
                 visitors
                 req
                 (fn [visitor] visitor))]
    (if (contains? result :status)
      (rejected result)
      (resolved result))))

(defn full-gathering?
  [gathering]
  (when (some :token (:players gathering))
    (rejected {:status 400
               :body "Gathering not complete"})))

(defn list-round-request
  [{:keys [rounds]} _req]
  (let [content (map #(hash-map :id (:ruid %)
                                :players (:players %)
                                :game (:game %))
                     (vals rounds))]
    {:status 200
     :body content}))

(defn start-round-request
  [{:keys [rounds tab visitors games]} req]
  (checked-with
   [
    [:gathering (partial with-gathering
                         @tab
                         (constantly (get-in req [:body "gathering"])))]
    [:game #(with-game @games (constantly (get-in % [:gathering :game])))]
    [:visitor (partial int-with-visitor @visitors req)]]
   [#(full-gathering? (:gathering %))]
   (fn [{:keys [gathering game visitor]}]
     (println (str "playing " gathering))
     (if (= ((comp first :players) gathering) (:uuid visitor))
       (let [created-round (rounds/create-round gathering game)]
         (dosync
          (alter rounds assoc (:ruid created-round) created-round))
         {:status 200
          :body (-> created-round
                    (dissoc :ruid :state :engine)
                    (assoc :id (:ruid created-round)))})
       {:status 403
        :body "Not the creator"}))))

(defn get-state-request
  [{:keys [rounds visitors]} ruid req]
  (with
   {:visitor (partial int-with-visitor @visitors req)
    :round (partial with-round @rounds (constantly ruid))}
   (fn [{:keys [visitor round]}]
     (let [state (rounds/read-last-state round (:uuid visitor))]
       {:status 200
        :body state}))))

(defn list-messages-request
  [{:keys [rounds visitors]} ruid req]
  (with
   {:visitor (partial int-with-visitor @visitors req)
    :round (partial with-round @rounds (constantly ruid))}
   (fn [{:keys [visitor round]}]
     (let [messages (rounds/read-messages round (:uuid visitor))]
       {:status 200
        :body messages}))))

(defn play-request
  [{:keys [rounds visitors]} ruid req]
  (with
   {:visitor (partial int-with-visitor @visitors req)
    :round (partial with-round @rounds (constantly ruid))}
   (fn [{:keys [visitor round]}]
     (let [move (get-in req [:body "move"])]
       (rounds/play-round round (:uuid visitor) move)
       {:status 200
        :body "Ok"}))))

(defn create-routes
  [context]
  [(http/context "/rounds" []
     (http/GET "/" [] (partial list-round-request context))
     (http/POST "/" [] (partial start-round-request context))
     (http/context "/:ruid" [ruid]
       (http/GET "/state" [] (partial get-state-request
                                      context
                                      (->uuid ruid)))
       (http/GET "/messages" [] (partial list-messages-request
                                         context
                                         (->uuid ruid)))
       (http/POST "/messages" [] (partial play-request
                                          context
                                          (->uuid ruid)))))])
