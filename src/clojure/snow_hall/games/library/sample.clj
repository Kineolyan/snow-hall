(ns snow-hall.games.library.sample
  (:require [clojure.core.async :as async :refer [chan go <! >! close!]]
            [snow-hall.games.game :refer [GameFactory RoundEngine]]))

(defn create-io
  []
  {:in (chan 1) :out (chan 1)})

(defrecord SampleRound [ios stop]
  RoundEngine
  (ios [e] ios)
  (stop [e] (compare-and-set! stop false true)))

(def end-message "-THE END-")

(defn- start
  [{:keys [ios stop]}]
  (let [[io1 io2] ios]
    (go (do
          (while (not @stop)
            (let [m1 (<! (:in io1))
                  m2 (<! (:in io2))]
              (println (str m1 " - " m2))
              (>! (:out io1) 1)
              (>! (:out io2) 2)))
          (doseq [io [io1 io2]]
            (>! (:out io) end-message)
            (close! (:out io))
            (close! (:in io)))))))

(defn- create
  []
  (SampleRound.
   (repeatedly 2 create-io)
   (atom false)))

(def SampleGame
  (reify
    GameFactory
    (create-engine [f]
      (let [round (create)]
        (start round)
        round))))
