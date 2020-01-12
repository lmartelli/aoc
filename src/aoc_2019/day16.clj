(ns aoc-2019.day16
  (:require
   [aoc.core :refer :all]
   [clojure.math.numeric-tower :refer [abs]]
   [clojure.test :refer :all]))

(def puzzle-input (puzzle-input-parse digit-seq))

(def base-pattern [0 1 0 -1])

;; part 1

(defn pattern [pos]
  (->> base-pattern
       (mapcat #(repeat pos %))
       cycle
       rest))

(defn apply-pattern [digits pos]
  (-> (reduce + (map * digits (pattern pos)))
      abs
      (mod 10)))

(defn phase [digits]
  (map
   #(apply-pattern digits (inc %))
   (range (count digits))))

(defn phases [digits n]
  (reduce (fn [input _] (phase input)) digits (range n)))

(defpart part1 [input]
  (->> (phases input 100)
       (take 8)
       (apply str)))

;; part 2

(defn part2 [input]
  )

;; tests
