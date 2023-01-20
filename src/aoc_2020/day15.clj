(ns aoc-2020.day15
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(def-input-parser [[line]]
  (parse-ints line))

;; part 1

(defn turn [{:keys [turn last-spoken last-spoken-indices]}]
  (let [spoken (- turn (get last-spoken-indices last-spoken turn))]
    {:turn (inc turn)
     :last-spoken spoken
     :last-spoken-indices (assoc last-spoken-indices last-spoken turn)}))

(defn state-seq [starting-numbers]
  (iterate
    turn
    {:turn (dec (count starting-numbers))
     :last-spoken (last starting-numbers)
     :last-spoken-indices (zipmap (butlast starting-numbers) (range))}))

(defn spoken-numbers [starting-numbers]
  (concat
    starting-numbers
    (->> (state-seq starting-numbers)
         (map :last-spoken)
         (drop 1))))

(defpart part1 [starting-numbers]
  (nth (spoken-numbers starting-numbers) (dec 2020)))

;; part 2

(defpart part2 [starting-numbers]
  (nth (spoken-numbers starting-numbers) (dec 30000000)))

;; tests

(deftest spoken-numbers-test
  (are [starting-numbers expected] (starts-with? (spoken-numbers starting-numbers) expected)
    [0 3 6] [0 3 6]
    [0 3 6] [0 3 6 0 3 3 1 0 4 0]))

(deftest part1-test
  (test-with-lines
    part1
    ["0,3,6"] 436
    ["1,3,2"] 1
    ["2,1,3"] 10
    ["1,2,3"] 27
    ["2,3,1"] 78
    ["3,2,1"] 438
    ["3,1,2"] 1836))

(deftest part2-test)
