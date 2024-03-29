(ns aoc-2021.day07
  (:require
   [aoc.core :refer :all]
   [clojure.math.numeric-tower :refer [abs]]
   [clojure.test :refer :all]))

(def-input-parser [[line]]
  (parse-ints line))

;; part 1

(defn required-fuel [positions target f]
  (->> (map #(f (abs (- % target))) positions)
       (reduce + 0)))

(defn find-min-fuel-consumption [positions f]
  (->> (map #(required-fuel positions % f) (range-inc (apply max positions)))
       (apply min)))

(defpart part1 [input]
  (find-min-fuel-consumption input identity))

;; part 2

(defpart part2 [input]
  (find-min-fuel-consumption input #(/ (* % (inc %)) 2)))

;; tests

(def data (parse-input-lines ["16,1,2,0,4,2,7,1,2,14"]))

(deftest part1-test
  (is (= 37 (part1 data))))

(deftest part2-test
  (is (= 168 (part2 data))))
