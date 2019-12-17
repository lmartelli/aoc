(ns aoc-2019.day13
  (:require
   [clojure.string :refer [split]]
   [aoc.core :refer :all]
   [aoc-2019.day05 :refer [debug set-debug]]
   [aoc-2019.day07 :refer [terminated?]]
   [aoc-2019.day09 :refer [run-instr run run-prog]]
   [clojure.test :refer :all]))

(def puzzle-input (puzzle-input-int-array))

;; part 1

(def tile-types { 0 :empty, 1 :wall, 2 :block, 3 :paddle, 4 :ball})

(defn draw-tile [screen [x y tile]]
  (assoc screen [x y] (tile-types tile)))

(->> (run-prog puzzle-input)
     :out
     (partition 3)
     (reduce draw-tile {})
     vals
     frequencies
     :block)

;; part 2

;; tests
