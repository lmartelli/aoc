(ns aoc-2022.day09
  (:require
   [aoc.core :refer :all]
   [clojure.math.numeric-tower :as math]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (split-lines #" " #(vector %1 (parse-int %2)))))

;; part 1

(def directions { "R" [1 0], "L" [-1 0], "U" [0 1], "D" [0 -1]})

(defn touching? [[ax ay] [bx by]]
  (and (<= -1 (- ax bx) 1)
       (<= -1 (- ay by) 1)))

(defn follow-move [from to]
  (if-not (touching? from to)
    (map signum (sub to from))))

(defn step [[k1 k2 :as knots] v]
  (if (not v)
    knots
    (let [new-k1 (add k1 v)]
      (if-not k2
        (list new-k1)
        (cons new-k1 (step (rest knots) (follow-move k2 new-k1)))))))

(defn explode-motion [[dir steps]]
  (repeat steps (directions dir)))

(defn count-visited [nb-knots motions]
  (->> (reductions step
                   (repeat nb-knots [0 0])
                   (mapcat explode-motion motions))
       (map last)
       (into #{})
       count))

;; part 2

(defpart part1 [motions]
  (count-visited 2 motions))

(defpart part2 [motions]
  (count-visited 10 motions))

;; tests

(deftest follow-move-test
  (are [head] (nil? (follow-move [0 0] head))
    [0 0] [0 1] [0 -1] [1 0] [-1 0] [1 1] [1 -1] [-1 1] [-1 -1])
  (are [head new-tail] (= new-tail (follow-move [0 0] head))
    [0 2] [0 1]
    [2 0] [1 0]
    [0 -2] [0 -1]
    [-2 0] [-1 0]
    [2 1] [1 1]
    [-2 1] [-1 1]
    [2 2] [1 1]
    [1 2] [1 1]))

(deftest part1-test
  (part-test part1 13)
  (are [motions expected] (= expected (part1 motions))
    [] 1
    [["R" 1]] 1
    [["R" 2]] 2
    [["R" 3]] 3
    [["R" 2] ["U" 1]] 2
    [["R" 2] ["U" 1]] 2
    [["R" 2] ["U" 2]] 3))

(deftest part2-test (part-test part2 "2" 36))
