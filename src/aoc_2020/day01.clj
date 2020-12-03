(ns aoc-2020.day01
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(puzzle-input-parse-lines parse-int)

;; part 1

(defpart part1 [input]
  (first
   (for [x input
         y input
         :let [s (+ x y)]
         :when (= s 2020)]
     (* x y))))

;; part 2

(defpart part2 [input]
  (first
   (for [x input
         y input
         z input
         :let [s (+ x y z)]
         :when (= s 2020)]
     (* x y z))))
