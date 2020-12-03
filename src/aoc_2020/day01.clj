(ns aoc-2020.day01
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]
   [clojure.math.combinatorics :refer [combinations]]))

(puzzle-input-parse-lines parse-int)

;; part 1

(defn solve [input n]
  (->> (combinations input n)
       (filter #(= 2020 (apply + %)))
       (map #(apply * %))
       first))

(defn solve2 [input]
  (for [x input y input z input :when (= 2020 (+ x y z))] (* x y z)))

(defpart part1 [input]
  (solve input 2))

;; part 2

(defpart part2 [input]
  (solve input 3))
