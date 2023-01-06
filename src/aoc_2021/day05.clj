(ns aoc-2021.day05
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (map parse-points)))

;; part 1

(defn horiz-or-vert? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

(defn count-dangerous-spots [lines]
  (->> (mapcat s2/segment-points lines)
       frequencies
       vals
       (filter #(> % 1))
       count))

(defpart part1 [input]
  (->> input
       (filter horiz-or-vert?)
       count-dangerous-spots))

;; part 2

(defpart part2 [input]
  (count-dangerous-spots input))

;; tests

(deftest part1-test (part-test part1 5))

(deftest part2-test (part-test part2 12))
