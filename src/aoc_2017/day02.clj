(ns aoc-2017.day02
  (:require [aoc.core :refer :all]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (map #(map parse-int (str/split % #"\s+")))))

;; part 1

(defn diff-min-max [s]
  (- (apply max s)
     (apply min s)))

(defn checksum [rows f-row]
  (->> rows
       (map f-row)
       (reduce +)))

(defpart part1 [input]
  (checksum input diff-min-max))

;; part 2

(defn find-div [row]
  (->> (for [a row, b row,
             :when (not= a b)]
         [a b])
       (filter #(zero? (apply rem %)))
       first
       (apply quot)))

(defpart part2 [input]
  (checksum input find-div))

;; tests

(deftest part1-test (part-test part1 18))

(deftest part1-test (part-test part2 "2" 9))
