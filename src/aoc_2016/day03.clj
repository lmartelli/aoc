(ns aoc-2016.day03
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-split-lines stream #"\s+" #(mapv parse-int (rest %))))

;; part 1

(defn valid-triangle? [lengths]
  (->> (range 3)
       (map #(> (apply + (remove-index lengths %)) (get lengths %)))
       (every? true?)))

(defpart part1 [input]
  (->> input
       (filter valid-triangle?)
       count))

;; part 2

(defn transpose [rows]
  (->> rows
       (partition 3)
       (map #(apply map vector %))
       (apply concat)))

(defpart part2 [input] 
  (->> input
       transpose
       (filter valid-triangle?)
       count))

;; tests

(deftest valid-triangle?-test
  (are [a b c] (valid-triangle? [a b c])
    10 10 15
    10 15 10
    15 10 10
    1 1 1)
  (are [a b c] (not (valid-triangle? [a b c]))
    10 10 20
    10 20 10
    20 10 10))
