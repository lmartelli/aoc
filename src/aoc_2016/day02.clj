(ns aoc-2016.day02
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (line-seq stream))

;; part 1

(def initial-pos [1 1])

(def keyboard1
  (array-2d-to-map
   [[1 2 3]
    [4 5 6]
    [7 8 9]]))

(def directions {\U [0 -1], \D [0 1], \R [1 0], \L [-1 0]})

(defn make-moves [keyboard pos moves]
  (->> moves
       (map directions)
       (reduce
        (fn [pos move]
          (let [new-pos (s2/+ pos move)]
            (if (contains? keyboard new-pos)
              new-pos
              pos)))
        pos)))

(defn get-code [initial-pos moves keyboard]
  (->> moves
       (reductions (partial make-moves keyboard) initial-pos)
       rest
       (map keyboard)
       (apply str)))

(defpart part1 [input]
  (get-code [1 1] input keyboard1))

;; part 2

(def keyboard2
  (zipmap
   [[2 0]
    [1 1] [2 1] [3 1]
    [0 2] [1 2] [2 2] [3 2] [4 2]
    [1 3] [2 3] [3 3]
    [2 4]]
   "123456789ABCD"))

(defpart part2 [input] 
  (get-code [0 2] input keyboard2))

;; tests

(deftest part1-test (part-test part1 "1985"))

(deftest part2-test (part-test part2 "5DB3"))
