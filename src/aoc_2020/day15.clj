(ns aoc-2020.day15
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(def-input-parser [[line]]
  (parse-ints line))

;; part 1

(defn nth-spoken-number [starting-numbers n]
  (let [last-spoken-turns (int-array  (inc (apply max n starting-numbers)) -1)]
    (dorun (map-indexed #(aset-int last-spoken-turns %2 (inc %1)) (butlast starting-numbers)))
    (loop [turn (count starting-numbers)
           last-spoken (int (last starting-numbers))]
      (if (= turn n)
        last-spoken
        (let [last-spoken-turn (aget last-spoken-turns last-spoken)
              spoken (if (= -1 last-spoken-turn) 0 (- turn last-spoken-turn))]
          (aset-int last-spoken-turns last-spoken turn)
          (recur
            (inc turn)
            spoken))))))

(defpart part1 [starting-numbers]
  (nth-spoken-number starting-numbers 2020))

;; part 2

(defpart part2 [starting-numbers]
  (nth-spoken-number starting-numbers 30000000))

;; tests

(deftest spoken-numbers-test
  (are [starting-numbers expected] (starts-with? (spoken-numbers starting-numbers) expected)
    [0 3 6] [0 3 6]
    [0 3 6] [0 3 6 0 3 3 1 0 4 0]))

(deftest part1-test
  (test-with-lines
    part1
    ["0,3,6"] 436
    ["1,3,2"] 1
    ["2,1,3"] 10
    ["1,2,3"] 27
    ["2,3,1"] 78
    ["3,2,1"] 438
    ["3,1,2"] 1836))
