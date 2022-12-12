(ns aoc.space-2d
  (:require
   [clojure.core :as core]
   [aoc.algo :as algo]))

(defn + [[ax ay] [bx by]]
  [(core/+ ax bx) (core/+ ay by)])

(defn - [[ax ay] [bx by]]
  [(core/- ax bx) (core/- ay by)])

(def up-right-down-left [[0 1] [1 0] [0 -1] [-1 0]])

(defn count-min-steps [&{:keys [start directions allowed-step? stop?]}]
  (algo/count-min-steps :start start
                        :stop? stop?
                        :next-positions (fn [pos]
                                          (->> (map #(+ pos %) directions)
                                               (filter #(allowed-step? pos %))))))
