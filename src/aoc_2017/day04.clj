(ns aoc-2017.day04
  (:require [aoc.core :refer :all]
            [clojure.string :refer [split]]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.test :refer :all]))

(puzzle-input-parse-lines #(split % #"\s+"))

;; part 1
(defn valid? [words]
  (->> words
       frequencies
       vals
       (some #(> % 1))
       not))

(defpart part1 [input]
  (->> input
       (filter valid?)
       count))

;; part 2

(defn anagram? [& rest]
  (->> rest
       (map frequencies)
       (apply =)))

(defn valid2? [words]
  (->> (combinations words 2)
       (filter #(apply anagram? %))
       empty?))

(defpart part2 [input]
    (->> input
       (filter valid2?)
       count))

;; tests
