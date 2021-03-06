(ns aoc-2015.day01
  (:require [aoc.core :refer :all]))

(puzzle-input-string)

(defpart part1 [input]
  (reduce + (map {\( 1, \) -1} input)))

(defpart part2 [input]
  (loop [floor 0
         pos 0
         input (map {\( 1, \) -1} input)]
    (if (= -1 floor)
      pos
      (recur (+ floor (first input))
             (inc pos)
             (rest input)))))


