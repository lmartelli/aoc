(ns aoc-2015.day10
  (:require
   [aoc.core :refer :all]))

(puzzle-input-string)

;; part 1

(defn look-and-say [s]
  (->> (re-seq #"(.)\1*" s)
       (map first)
       (reduce
        (fn [say look]
          (conj say (count look) (first look)))
        [])
       (apply str)))

(defn look-and-say-seq [start]
  (iterate look-and-say start))

(defpart part1 [input]
  (-> (look-and-say-seq input)
      (nth 40)
      count))
;; part 2

(defpart part2 [input]
    (-> (look-and-say-seq input)
      (nth 50)
      count))

;; tests
