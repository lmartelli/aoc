(ns aoc-2016.day12
  (:require
   [aoc.core :refer :all]
   [aoc.cpu :refer :all]
   [aoc-2016.assembunny :as assembunny]
   [clojure.test :refer :all]))

;; Use aoc.cpu/puzzle-input

;; part 1

(defpart part1 [prog]
  (-> (run-prog {} prog assembunny/instruction-set)
      :a))

;; part 2

(defpart part2 [prog]
  (-> (run-prog {:c 1} prog assembunny/instruction-set)
      :a))

;; tests

(deftest part1-test (part-test part1 42))
