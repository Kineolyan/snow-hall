(ns snow-hall.validate
  (:require [clojure.spec.alpha :as s]))

(defn create-validation 
  [spec]
  (fn [value] 
    (let [result (s/valid? spec value)]
      (when-not result (s/explain spec value))
      value)))