(ns snow-hall.games.seq-chan
  (:require [clojure.core.async :refer [chan]]
            [clojure.core.async.impl.protocols :as protocols]))

(defn create-cell
  [owners]
  {:pre [(coll? owners) (seq owners)]}
  (ref {:nexts (rest owners)
        :owner (first owners)}))

(defn fill-cell!
  "Fills the current cell with a value, provided the key k is the current owner."
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

(defn consume-cell!
  "Consumes the content of the cell, returning the value and moving to the next owner.
  This is conditioned by k being the current owner."
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
    (consume-cell! cell k))
  (add!*
    [this v]
    (fill-cell! cell k v)
    this)
  (close-buf! [this])
  clojure.lang.Counted
  (count 
   [this]
   (if (protocols/full? this) 1 0))
  clojure.lang.IDeref
  (deref
    [this]
    nil))

(defn make-chans
  [owners]
  (let [cell (create-cell owners)
        buffers (map #(SequentialBuffer. cell %) owners)]
    (map chan buffers)))