(ns aoc-2020.day06
  (:require
   [aoc.core :refer :all]
   [clojure.set :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (puzzle-input-lines stream)
       (merge-lines "" nil conj)))

;; part 1

(defn count-distinct-answers [answers]
  (count (reduce #(into %1 %2) #{} answers)))

(defpart part1 [input]
  (->> (map count-distinct-answers input)
       (reduce +)))

;; part 2

(defn count-all-answered [answers]
  (->> (map set answers)
       (apply intersection)
       count))

(defpart part2 [input]
  (->> (map count-all-answered input)
       (reduce +)))
