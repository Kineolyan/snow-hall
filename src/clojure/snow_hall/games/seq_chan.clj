(ns snow-hall.games.seq-chan
  (:require [clojure.core.async :refer [chan]]
            [clojure.core.async.impl.protocols :as protocols]
            [clojure.test :refer [is]]))

(defn create-cell
  [owners]
  {:pre [(coll? owners) (seq owners)]}
  (ref {:nexts (rest owners)
        :owner (first owners)}))

(defn fill-cell!
  [cell k v]
  {:pre [(some? v)]}
  (dosync 
   (let [{:keys [owner content]} @cell]
     (when (and (nil? content) (= owner k))
       (alter cell assoc :content v)))))

(defn reset-cell
  [state]
  (dissoc state :content))

(defn move-to-next
  [state]
  (let [{[next-owner & remaining] :nexts
         current-owner :owner} state]
    (assoc state
           :owner next-owner
           :nexts (concat remaining (list current-owner)))))

(defn reset-to-next
  [state]
  (-> state reset-cell move-to-next))

(defn empty-cell!
  [cell k]
  (dosync
   (let [{:keys [owner content]} @cell]
     (when (= owner k)
       (alter cell reset-to-next)
       content))))

(deftype SequentialBuffer [cell k]
  protocols/Buffer
  (full?
    [this]
    (let [{:keys [owner content]} @cell]
      (or (not= owner k)
          (some? content))))
  (remove!
    [this]
    (empty-cell! cell k))
  (add!*
    [this v]
    (fill-cell! cell k v)
    this)
  (close-buf! [this])
  clojure.lang.IDeref
  (deref
    [this]
    nil))

(defn create-buffer
  [])

(defn make-chan
  [n]
  (repeat n nil))