(ns aoc-2017.day23
  (:require
   [aoc.core :refer :all]
   [aoc.cpu :refer :all]
   [aoc-2017.instr :refer :all]))

;; Use aoc.cpu/puzzle-input

;; part 1

(defpart part1 [prog]
  (->> (trace-instr {:ip 0} prog basic-instr-set)
       (filter (fn [[instr]] (= :mul instr)))
       count))

;; part 2

(defpart part2 [prog]
  (-> (run-prog {:a 1} prog basic-instr-set)
      :a))

;; tests
