(ns aoc-2015.day08
  (:require
   [aoc.core :refer :all]))

(puzzle-input-lines)

;; part 1

(defn raw-size [line]
  (count line))

(defn total-size [lines count-fn]
  (->> lines
       (map count-fn)
       (reduce +)))

(defn delta-size [line]
  (+ 2
     (count (re-seq #"\\[^x]" line))
     (* 3 (count (re-seq #"\\x[0-9a-f]{2}" line)))))

(defpart part1 [input]
  (total-size input delta-size))

;; part 2

(defn encode-delta-size [line]
  (+ 2
   (count (re-seq #"[\"\\]" line))))

(defpart part2 [input]
  (total-size input encode-delta-size))

;; tests
