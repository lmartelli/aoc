(ns aoc-2016.day12
  (:require
   [aoc.core :refer :all]
   [aoc-2016.assembunny :refer :all]
   [clojure.test :refer :all]))

;; Use aoc-2016.assembunny/puzzle-input

;; part 1

(def init-registers (constantly {:a 0 :b 0 :c 0 :d 0 :ip 0}))

(defpart part1 [input]
  (-> (run-prog input (init-registers))
      :a))

;; part 2

(defpart part2 [input]
  (-> (run-prog input
                (-> (init-registers) (assoc :c 1)))
      :a))

;; tests

(deftest part1-test (part-test part1 42))
