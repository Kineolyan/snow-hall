(ns snow-hall.games.seq-chan
  (:require [clojure.core.async :refer [chan]]
            [clojure.core.async.impl.protocols :as protocols]))

(defn create-cell
  [owners]
  {:pre (seq owners)}
  (ref {:seq (cycle owners)
        :_n (count owners)
        :owner (first owners)}))

(defn fill-cell!
  [cell k v]
  (dosync 
   (let [{:keys [owner content]} @cell]
     (when (and (nil? content) (= owner k))
       (alter cell assoc :content v)))))

(defn reset-cell
  [state]
  (dissoc state :content))

(defn move-to-next
  [state]
  (let [[owner & remaining] (:seq state)]
    (assoc state
           :owner owner
           :seq remaining)))

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