(ns aoc-2018.day01
  (:require [aoc.core :refer :all]))

(puzzle-input-parse-lines parse-int)

;; part 1

(defpart part1 [input]
  (reduce + input))

;; part 2

(defpart part2 [input]
  (loop [freq 0
         [delta & others] (cycle input)
         history #{}]
    (if (contains? history freq)
      freq
      (recur (+ freq delta) others (conj history freq)))))
