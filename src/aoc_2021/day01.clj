(ns aoc-2021.day01
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines stream parse-int))

;; part 1

(defn count-increases [input]
  (->> (map #(if (> %2 %1) 1 0) input (rest input))
       (apply +)))

(defpart part1 [input]
  (count-increases input))

;; part 2

(defn count-3-windows-increases [input]
  (->> (partition 3 1 input)
       (map #(apply + %1))
       count-increases))

(defpart part2 [input]
  (count-3-windows-increases input))

;; tests

(deftest count-increases-test
  (are [input increases] (= increases (count-increases input))
    [] 0
    [0] 0
    [0 1] 1
    [1 0] 0
    [0 1 2 3] 3
    [3 2 1 0] 0
    [0 2 1 3] 2))

(deftest count-3-windows-increases-test
  (are [input increases] (= increases (count-3-windows-increases input))
    [199 200 208 210 200 207 240 269 260 263] 5))
