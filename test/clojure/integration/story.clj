(ns integration.story
  (:require [org.httpkit.client :as http]
            [clojure.test :refer [deftest]]
            [clojure.data.json :as json]))

(defmacro story
  [name & body]
  `(deftest ~(with-meta name {:integration true}) [] ~@body)) 

(defmacro step
  [title & operation]
  `(do
     (println (str " > " ~title))
     ~@operation))

(def port 3000)
(def base-url (str "http://localhost:" port))

(defn- handle
  [response]
  (let [{:keys [status body error]} @response]
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
