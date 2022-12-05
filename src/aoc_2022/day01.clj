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


(deftest part1-test (part-test part1 24000))

(deftest part2-test (part-test part2 45000))
