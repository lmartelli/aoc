(ns aoc-2017.day01
  (:require [aoc.core :refer :all]
            [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-string stream))

;; part 1

(defn with-next [input]
  (partition 2 1 input input))

(defn sum-matching [input f-pair]
  (->> input 
       digit-seq
       f-pair
       (filter #(apply = %))
       (map first)
       (reduce +)))

(defpart part1 [input]
  (sum-matching input with-next))


;; part 2

(defn with-halfway [input]
  (map #(vector %1 (nth input (mod (+ %2 (/ (count input) 2)) (count input))))
       input (range)))

(defpart part2 [input]
  (sum-matching input with-halfway))

;; tests

(deftest part1-test
  (are [input expected] (= expected (part1 input))
    "1122" 3
    "1111" 4
    "1234" 0
    "91212129" 9))

(deftest part2-test
  (are [input expected] (= expected (part2 input))
    "1212" 6
    "1221" 0
    "123425" 4
    "123123" 12
    "12131415" 4))
