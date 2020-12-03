(ns aoc-2020.day02
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(puzzle-input-parse-lines
 (fn [line]
   (let [[_ low high char password] (re-matches #"(\d+)-(\d+) (.): ([a-z]+)" line)]
     (vector (parse-int low) (parse-int high) (first char) password))))

(defn count-valid-passwords [valid? input]
  (->> input
       (filter #(apply valid? %))
       count))

;; part 1

(defpart part1 [input]
  (count-valid-passwords
   (fn [low high char password]
     (let [c (count (filter #{char} password))]
       (<= low c high)))
   input))

;; part 2

(defpart part2 [input]
  (count-valid-passwords
   (fn [pos1 pos2 char password]
     (->> (map #(= char (nth password (dec %))) [pos1 pos2])
          (filter true?)
          count
          (= 1)))
   input))
