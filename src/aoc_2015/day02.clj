(ns aoc-2015.day02
  (:require [aoc.core :refer :all]
            [clojure.string :refer [split]]))

(puzzle-input-parse-lines #(map parse-int (split % #"x")))

;; part 1

(defn surface [[w h l]]
  (let [s1 (* w h)
        s2 (* w l)
        s3 (* h l)]
    (+ (* 2 (+ s1 s2 s3))
       (min s1 s2 s3))))

(defpart part1 [input]
  (reduce + (map surface input)))

;; part 2

(defn ribbon-length [[w h l]]
  (let [p1 (+ w h)
        p2 (+ w l)
        p3 (+ h l)]
    (+ (* 2 (min p1 p2 p3)) (* w h l))))

(defpart part2 [input]
  (reduce + (map ribbon-length input)))
