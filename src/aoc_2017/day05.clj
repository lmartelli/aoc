(ns aoc-2017.day05
  (:require [aoc.core :refer :all]))

(puzzle-input-parse-lines parse-int)

;; part 1

(defn not-escaped? [pos maze]
  (get maze pos nil))

(defn do-jump [[pos maze] f]
  (if-let [offset (get maze pos nil)]
    [(+ pos offset) (update maze pos f)]
    [pos maze]))

(defn count-steps-to-exit [maze f-update]
  (->> (iterate (fn [x] (do-jump x f-update)) [0 maze])
       (map first)
       (take-while #(not-escaped? % maze))
       count))

(defpart part1 [input]
  (count-steps-to-exit input inc))

;; part 2

(defpart part2 [input]
  (count-steps-to-exit
   input
   (fn [x] (if (>= x 3) (dec x) (inc x)))))

;; tests
