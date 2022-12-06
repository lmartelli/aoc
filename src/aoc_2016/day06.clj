(ns aoc-2016.day06
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (line-seq stream))

;; part 1

(defn unjam [messages freq-unjam-fn]
  (->> messages
       (apply map vector)
       (map frequencies)
       (map freq-unjam-fn)
       (map first)
       (apply str)))

(defpart part1 [input]
  (unjam input #(apply max-key val %)))

;; part 2

(defpart part2 [input]
  (unjam input #(apply min-key val %)))

;; tests

(deftest part1-test (part-test part1 "easter"))

(deftest part2-test (part-test part2 "advent"))
