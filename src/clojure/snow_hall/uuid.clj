(ns snow-hall.uuid
  (:require [medley.core :as m]))

(def random-uuid m/random-uuid)

(defn ->uuid
  [value]
  (cond
    (m/uuid? value) value
    (string? value) (m/uuid value)
    :else (throw (IllegalArgumentException. (str "Cannot create uuid from " value)))))