(ns aoc-2022.day04
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]
   [clojure.string :refer [split]]))

(defn puzzle-input [stream]
  (puzzle-input-split-lines stream #","))

;; part 1

(defn parse-range [range-spec]
  (->> (split range-spec #"-")
       (mapv parse-int)))

(defn range-contains-other? [[min1 max1] [min2 max2]]
  (or (<= min2 min1 max1 max2)
      (<= min1 min2 max2 max1)))

(defpart part1 [input]
  (->> input
       (map #(map parse-range %))
       (filter #(apply range-contains-other? %))
       count))

;; part 2

(defn range-overlap? [[min1 max1] [min2 max2]]
  (not
    (or (> min1 max2)
        (> min2 max1))))

(defpart part2 [input]
    (->> input
         (map #(map parse-range %))
         (filter #(apply range-overlap? %))
         count))

;; tests

(def test-data (puzzle-input (test-input *ns*)))

(deftest parse-range-test
  (are [range-spec expected] (= expected (parse-range range-spec))
    "1-2" [1 2]
    "123-456" [123 456]))

(deftest range-contains-other?-test
  (are [r1 r2 expected] (and (= expected (range-contains-other? r1 r2))
                             (= expected (range-contains-other? r2 r1))
                             )
    [2 4] [5 7] false
    [2 4] [4 7] false
    [2 4] [3 7] false
    [2 4] [2 7] true
    [2 4] [1 7] true
    [2 4] [1 5] true
    [2 4] [1 4] true
    [2 4] [1 3] false
    [2 4] [1 2] false
    [2 4] [1 1] false
    ))

(deftest range-overlap?-test
  (are [r1 r2 expected] (and (= expected (range-overlap? r1 r2))
                             (= expected (range-overlap? r2 r1))
                             )
    [2 4] [5 7] false
    [2 4] [4 7] true
    [2 4] [3 7] true
    [2 4] [2 7] true
    [2 4] [1 7] true
    [2 4] [1 5] true
    [2 4] [1 4] true
    [2 4] [1 3] true
    [2 4] [1 2] true
    [2 4] [1 1] false
    ))

(deftest part1-test
  (is (= 2 (part1 test-data))))

(deftest part2-test
  (is (= 4 (part2 test-data))))
