(ns user
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.repl :refer :all]
            [clojure.test :as test]
            [clojure.tools.namespace.repl :as tnr]
            [snow-hall.core]))

(defn run-my-tests
  ([] (run-my-tests false))
  ([all]
   (let [expr (if all  
                #"snow-hall\..+-test|integration\..+"  
                #"snow-hall\..+-test")
         namespaces (->> (all-ns) 
                         (map ns-name) 
                         (map str) 
                         (filter (partial re-matches expr)) (map symbol))]
     (apply test/run-tests namespaces))))

(def ze-server (atom nil))
(defn run-project
  []
  (swap! ze-server
         (fn [stop-server]
           (when stop-server (stop-server))
           (snow-hall.core/start-server 3003 true))))