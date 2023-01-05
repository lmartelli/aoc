(ns aoc-2019.day01
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (map parse-int)))

;; part 1

(defn get-fuel [mass]
  (max 0
       (-> mass
           (quot 3)
           (- 2))))

(defn get-total-fuel [masses f]
  (reduce + (map f masses)))

(defpart part1 [input]
  (get-total-fuel input get-fuel))

;; part 2

(defn get-fuel2 [mass]
  (if (<= mass 0)
    0
    (let [fuel (get-fuel mass)]
      (+ fuel (get-fuel2 fuel)))))

(defpart part2 [input]
  (get-total-fuel input get-fuel2))

;; tests

(deftest get-fuel-test
  (are [mass fuel] (= fuel (get-fuel mass))
    2 0
    12 2
    14 2
    1969 654
    100756 33583))

(deftest get-fuel-test2
  (are [mass fuel] (= fuel (get-fuel2 mass))
    14 2
    1969 966
    100756 50346))

(deftest part1-test (part-test part1 34241))

(deftest part2-test (part-test part2 51316))
