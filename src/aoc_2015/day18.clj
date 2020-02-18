(ns aoc-2015.day18
  (:require
   [aoc.core :refer :all]))

(puzzle-input-lines array-2d-to-map)

;; part 1

(defn alive? [state]
  (= \# state))

(defn dead? [state]
  (not (alive? state)))

(defn neighbours [m coord]
  (map
   #(m (add coord %))
   [[0 1] [0 -1] [1 0] [-1 0] [1 1] [-1 -1] [1 -1] [-1 1]]))

(defn alive-neighbours [m coord])

(defn next-state [m coord]
  (cond
    (alive? (m coord)) nil
    
    ))

(defn game-of-life-step [m]
  (into
   {}
   (map (fn [coord state] ) m)))

(defpart part1 [input]
  nil)

;; part 2

(defpart part2 [input]
  nil)

;; tests
