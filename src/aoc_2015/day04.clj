(ns aoc-2015.day04
  (:require [aoc.core :refer :all]
            [aoc.md5 :refer :all]
            [clojure.string :refer [split]]))

(puzzle-input-string)

;; part 1

(defn find-salt [key regex]
  (->>  (map vector (range) (md5-seq key))
        (find-first #(re-find regex (second %)))
        first))

(defpart part1 [input]
  (find-salt input #"^0{5}"))

;; part 2

(defpart part2 [input]
  (find-salt input #"^0{6}"))
