(ns aoc-2021.day05
  (:require
   [aoc.core :refer :all :exclude [range-inc]]
   [clojure.string :refer [split]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-int-array
    stream
    #(->> (split % #"[^\d]+")
          (map parse-int)
          (partition 2)
          (mapv vec))))

;; part 1

(defn horiz-or-vert? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

(defn filter-horiz-or-vert-lines [lines]
  (filter horiz-or-vert? lines))

(defn range-inc [from to]
  (if (> to from)
    (range from (inc to))
    (take (inc (- from to)) (iterate dec from))))

(defn draw-line [[[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2) (map #(vector x1 %) (range-inc y1 y2))
    (= y1 y2) (map #(vector % y1) (range-inc x1 x2))
    :else (map vector (range-inc x1 x2) (range-inc y1 y2))))

(defn count-dangerous-spots [lines]
  (->> (mapcat draw-line lines)
       frequencies
       vals
       (filter #(> % 1))
       count))

(defpart part1 [input]
  (->> input
       filter-horiz-or-vert-lines
       count-dangerous-spots))

;; part 2

(defpart part2 [input]
  (count-dangerous-spots input))

;; tests

(def test-data (puzzle-input (test-input *ns*)))

(deftest part1-test
  (is (= 5 (part1 test-data))))

(deftest part1-test
  (is (= 12 (part2 test-data))))
