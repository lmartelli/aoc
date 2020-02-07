(ns aoc-2017.day01
  (:require [aoc.core :refer :all]
            [clojure.test :refer :all]))

(puzzle-input-string)

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
