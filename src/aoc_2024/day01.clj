(ns aoc-2024.day01
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines
    stream
    parse-ints
    #(apply mapv vector %)))

;; part 1

(defpart part1 [input]
  (->> input
       (map sort)
       (apply map (comp abs -))
       (reduce +)))

;; part 2

(defpart part2 [[left right]]
  (let [right-freqs (frequencies right)]
    (->> left
         (map (juxt identity #(right-freqs % 0)))
         (map #(apply * %))
         (reduce +))))

;; tests


(deftest part1-test (part-test part1 11))


(deftest part2-test (part-test part2 31))
