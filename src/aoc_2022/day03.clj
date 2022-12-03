(ns aoc-2022.day03
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]
   [clojure.set :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-lines stream))

;; part 1

(defn find-common-item [compartments]
  (->> (map set compartments)
       (apply intersection)
       first))

(defn item-priority [item]
  (cond
    (<= (int \a) (int item) (int \z)) (inc (- (int item) (int \a)))
    (<= (int \A) (int item) (int \Z)) (+ 27 (- (int item) (int \A)))))

(defpart part1 [input]
  (->> input
       (map #(split-at (/ (count %) 2) %))
       (map find-common-item)
       (map item-priority)
       (reduce +)))

;; part 2

(defpart part2 [input]
  (->> input
       (partition 3)
       (map (comp item-priority find-common-item))
       (reduce +)))

;; tests

(def test-data (puzzle-input (test-input *ns*)))

(deftest part1-test
  (is (= 157 (part1 test-data))))

(deftest part2-test
  (is (= 70 (part2 test-data))))
