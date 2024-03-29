(ns aoc-2017.day11
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.math.numeric-tower :refer [abs]]
   [clojure.string :refer [split]]))

(defn puzzle-input [stream]
  (->> (first (line-seq stream))
       (re-seq #"[a-z]+")
       (map keyword)))

;; part 1

(def base
  {:n [0 1]
   :s [0 -1]
   :ne [1 0]
   :se [1 -1]
   :nw [-1 1]
   :sw [-1 0]})

(defn coordinates [path]
  (->> path
       (map base)
       (reduce add)))

(defn same-sign? [x y]
  (or (and (pos? x) (pos? y))
      (and (neg? x) (neg? y))))

(defn dist [[x y]]
  (if (same-sign? x y)
    (s2/manatthan-dist [x y])
    (max (abs x) (abs y))))

(defpart part1 [input]
  (dist (coordinates input)))

;; part 2

(defn coordinates-along [path]
  (->> path
       (map base)
       (reductions add)))

(defpart part2 [input]
  (->> input
       coordinates-along
       (map dist)
       (apply max)))

;; tests
