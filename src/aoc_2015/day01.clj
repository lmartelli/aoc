(ns aoc-2015.day01
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-string stream))

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

;; Tests

(deftest part1-test
  (are [input expected] (= expected (part1 input))
    "(())" 0
    "()()" 0
    "))(((((" 3
    "())" -1
    "))(" -1
    ")))" -3
    ")())())" -3))

(deftest part2-test
  (are [input expected] (= expected (part2 input))
    ")" 1
    "()())" 5))

