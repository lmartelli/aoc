(ns aoc-2015.day05
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (line-seq stream))

;; part 1

(defn has-3-vowels? [s]
  (->> s
       frequencies
       (filter #(contains? (into #{} "aeiou") (first %)))
       (map second)
       (reduce +)
       (<= 3)))

(defn has-consecutive? [s]
  (re-find #"(.)\1" s))

(defn contains-forbidden? [s]
  (re-find #"ab|cd|pq|xy" s))

(defn nice? [s]
  (and (has-3-vowels? s)
       (has-consecutive? s)
       (not (contains-forbidden? s))))

(defn count-nice [coll nice-predicate]
    (->> coll
       (filter nice-predicate)
       count))

(defpart part1 [input]
  (count-nice input nice?))

;; part 2

(defn nice2? [s]
  (and (re-find #"(..).*\1" s)
       (re-find #"(.).\1" s)))

(defpart part2 [input]
  (count-nice input nice2?))

;; Tests

(deftest nice?-test
  (are [s] (nice? s)
    "ugknbfddgicrmopn"
    "aaa")
  (are [s] (not (nice? s))
    "jchzalrnumimnmhp"
    "haegwjzuvuyypxyu"
    "dvszwmarrgswjxmb"))

(deftest nice2?-test
  (are [s] (nice2? s)
    "qjhvhtzxzqqjkmpb"
    "xxyxx")
  (are [s] (not (nice2? s))
    "uurcxstgmygtbstg"
    "ieodomkazucvgmuy"))
