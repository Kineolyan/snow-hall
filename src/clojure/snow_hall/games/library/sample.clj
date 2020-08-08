(ns snow-hall.games.library.sample
  (:require [clojure.core.async :as async :refer [chan go <! >! close!]]
            [snow-hall.games.game :as game]))

(defn create-io
  []
  {:in (chan 1) :out (chan 1)})

(defrecord SampleRound [ios stop]
  game/RoundEngine
  (ios [e] ios)
  (stop [e] (compare-and-set! stop false true)))

(def end-message "-THE END-")

(defn- create
  []
  (SampleRound.
   (repeatedly 2 create-io)
   (atom false)))

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

(defn create-and-start
  []
  (let [round (create)]
    (start round)
    round))

(def game-definition
  (reify
    game/Game
    (get-specs [this] {:name "Sample"
                      :player-count {:exact 2}})
    (read-options [this options] {})
    (get-player-count [this option] 2)
    (create-engine [this options] (create-and-start))))
