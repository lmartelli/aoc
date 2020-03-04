(ns aoc-2017.day15
  (:require
   [aoc.core :refer :all]))

(puzzle-input-parse-lines
 #(->> (re-find #"(\d+)" %)
       first
       parse-int))

;; part 1

(defn generated-values [first-value multiplier]
  (iterate #(rem (* % multiplier) 2147483647) first-value))

(defn gen-a [first-value]
  (generated-values first-value 16807))

(defn gen-b [first-value]
  (generated-values first-value 48271))

(defn low-word [x]
  (bit-and x (dec (reduce * (repeat 16 2)))))

(defn low-word [x]
  (bit-and x 65535))

(defn compare [a b]
  (= (low-word a) (low-word b)))

(defn judge [n seq-a seq-b]
  (->> (map compare seq-a seq-b)
       (take n)
       (filter true?)
       count))

(defpart part1 [input]
  (judge 40000000 (gen-a (first input)) (gen-b (second input))))

;; part 2

(defn filter-multiples [s n]
  (filter #(zero? (mod % n)) s))

(defpart part2 [input]
  (judge 5000000
         (filter-multiples (gen-a (first input)) 4)
         (filter-multiples (gen-b (second input)) 8)))

;; tests
