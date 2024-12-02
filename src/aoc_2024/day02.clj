(ns aoc-2024.day02
  (:require
   [aoc.core :refer :all]
   [clojure.math.combinatorics :as combo]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines stream parse-ints))

;; part 1

(defn safe? [report]
  (let [deltas (->> report
                    (partition 2 1)
                    (map #(apply - %)))]
    (and (apply = (map signum deltas))
         (every? #(<= 1 % 3) (map abs deltas)))))

(defpart part1 [reports]
  (->> reports
       (filter safe?)
       count))

;; part 2

(defn safe-with-dampener? [report]
  (or (safe? report)
      (some safe? (map #(remove-index report %) (range (count report))))))

(defpart part2 [reports]
  (->> reports
       (filter safe-with-dampener?)
       count))

;; tests

(deftest part1-test (part-test part1 2))

(deftest safe?-test
  (are [report safe] (= safe (safe? report))
    [7 6 4 2 1] true
    [1 2 7 8 9] false
    [9 7 6 2 1] false
    [1 3 2 4 5] false
    [8 6 4 4 1] false
    [1 3 6 7 9] true))

(deftest part2-test (part-test part2 nil))

(deftest safe-with-dampener?-test
  (are [report safe] (= safe (true? (safe-with-dampener? report)))
    [7 6 4 2 1] true
    [1 2 7 8 9] false
    [9 7 6 2 1] false
    [1 3 2 4 5] true
    [8 6 4 4 1] true
    [1 3 6 7 9] true))

