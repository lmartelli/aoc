(ns aoc-2022.day01
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (puzzle-input-lines stream)
       (split-seq empty?)
       (map #(map parse-int %))))

;; part 1

(defpart part1 [input]
  (->> (map #(reduce + %) input)
       (apply max)))

;; part 2

(defpart part2 [input]
  (->> (map #(reduce + %) input)
       (sort >)
       (take 3)
       (reduce +)))

;; tests

(def test-data (puzzle-input (test-input *ns*)))

(deftest part1-test
  (is (= 24000 (part1 test-data))))

(deftest part2-test
  (is (= 45000 (part2 test-data))))
