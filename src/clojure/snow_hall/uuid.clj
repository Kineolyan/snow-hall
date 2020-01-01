(ns snow-hall.uuid
  (:require [medley.core :as m])
  (:refer-clojure :exclude [uuid?]))

(def random-uuid m/random-uuid)
(def uuid? m/uuid?)

(defn ->uuid
  [value]
  (cond
    (uuid? value) value
    (string? value) (m/uuid value)
    :else (throw (IllegalArgumentException. (str "Cannot create uuid from " value)))))