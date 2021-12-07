(ns aoc-2020.day01
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]
   [clojure.math.combinatorics :refer [combinations]]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines stream parse-int))

;; part 1

(defn solve [input n]
  (->> (combinations input n)
       (filter #(= 2020 (apply + %)))
       first
       (apply *)))

(defpart part1 [input]
  (solve input 2))

;; part 2

(defpart part2 [input]
  (solve input 3))

;; tests

(def input [1721 979 366 299 675 1456])

(deftest part1-test
  (is (= 514579 (part1 input))))

(deftest part2-test
  (is (= 241861950 (part2 input))))
