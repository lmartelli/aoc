(ns aoc-2015.day10
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (-> stream
      line-seq
      first))

;; part 1

(defn look-and-say [s]
  (->> (partition-by identity s)
       (mapcat (juxt count first))))

(defn look-and-say-seq [start]
  (iterate look-and-say start))

(defn look-and-say-nth-length [seed n]
  (-> seed
      digit-seq
      look-and-say-seq
      (nth 40)
      count))

(defpart part1 [input]
  (look-and-say-nth-length input 40))

;; part 2

(defpart part2 [input]
  (look-and-say-nth-length input 50))

;; tests

(deftest look-and-say-test
  (are [digits expected] (= expected (look-and-say digits))
    [1] [1 1]
    [1 1] [2 1]
    [2 1] [1 2 1 1]
    [1 2 1 1] [1 1 1 2 2 1]
    [1 1 1 2 2 1] [3 1 2 2 1 1]))

(deftest part1-test (is (= 329356 (part1 "3113322113"))))

(deftest part2-test (is (= 4666278 (part2 "3113322113"))))
