(ns integration.story
  (:require [clojure.string :as str]
            [org.httpkit.client :as http]
            [clojure.test :refer [deftest]]
            [clojure.data.json :as json]
            [snow-hall.core :refer [start-server]])
  (:refer-clojure :exclude [get]))

(def server (atom nil))
(def port 4321)
(def base-url (str "http://localhost:" port))

(defn ensure-server 
  []
  (swap! server #(if (nil? %1) (start-server port false) %1)))

(defmacro story
  [name & body]
  `(deftest ~(with-meta name {:integration true}) []
     (require 'integration.story)
     (integration.story/ensure-server)
     ~@body)) 

(defmacro step
  [title & operation]
  `(do
     (println (str " > " ~title))
     ~@operation))

(defn- get-content-type
  [value]
  (first (str/split value #";")))

(defn- handle
  [response]
  (let [{:keys [status body error headers]} @response]
    (when error
      (throw (AssertionError. (str "Failed, exception: " error))))
    (when (>= status 400) 
      (throw (AssertionError. (str "Failed [" status "]: " body))))
    (if body 
      (case (get-content-type (:content-type headers))
        "application/json" (json/read-str body)
        "application/octet-stream" (slurp body)) 
      nil)))

(defn get
  ([url]
   (handle (http/get (str base-url url))))
  ([url headers]
   (handle (http/get (str base-url url) {:headers headers}))))

(defn- request-with-body
  [method url content]
  (let [options {:headers {"Content-Type" "application/json"}
                 :body (json/write-str content)}]
    (handle (method (str base-url url) options))))

(defn post
  ([url]
   (handle (http/post (str base-url url))))
  ([url content]
   (request-with-body http/post url content)))

(defn put
  ([url]
   (handle (http/put (str base-url url))))
  ([url content]
   (request-with-body http/put url content)))
