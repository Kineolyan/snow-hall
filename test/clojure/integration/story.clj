(ns integration.story
  (:require [org.httpkit.client :as http]
            [clojure.test :refer [deftest testing]]
            [clojure.data.json :as json]))

(defmacro story
  [name & body]
  `(deftest ~name ~@body)) 

(defn step
  [title operation]
  (print (str " > " title))
  (testing title operation))

(def port 3000)
(def base-url (str "http://localhost:" port))

(defn- handle
  [response]
  (let [{:keys [status headers body error] :as resp} @response]
    (when error
      (throw (AssertionError. (str "Failed, exception: " error))))
    (when (>= status 400) 
      (throw (AssertionError. (str "Failed [" status "]: " body))))
    (if body
      (json/read-str body)
      nil)))

(defn get
  [url]
  (handle (http/get (str base-url url))))

(defn post
  ([url]
   (handle (http/post (str base-url url))))
  ([url content]
   (let [body (json/write-str content)]
     (handle (http/post (str base-url url) {:body body})))))

(defn put
  ([url]
   (handle (http/put (str base-url url))))
  ([url content]
   (let [body (json/write-str content)]
     (handle (http/put (str base-url url) {:body body})))))
