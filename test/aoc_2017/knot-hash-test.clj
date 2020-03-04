(ns aoc-2017.knot-hash-test
  (:require
   [aoc-2017.knot-hash :refer :all]
   [clojure.test :refer :all]))

(deftest sub-list-test
  (are [v pos len expected] (= expected (sub-list v pos len))
    [0 1 2 3 4] 0 5 [0 1 2 3 4]
    [0 1 2 3 4] 1 5 [1 2 3 4 0]))

(deftest replace-test
  (are [v pos other expected] (= expected (replace v pos other))
    [0 1 2 3 4] 0 [5 6 7] [5 6 7 3 4]
    [0 1 2 3 4] 4 [5 6 7] [6 7 2 3 5]))

(deftest knot-hash-test
  (are [input hash] (= hash (hex (knot-hash input)))
    "" "a2582a3a0e66e6e86e3812dcb672a272"
    "AoC 2017" "33efeb34ea91902bb2f59c9920caa6cd"
    "1,2,3" "3efbe78a8d82f29979031a4aa0b16a9d"
    "1,2,4" "63960835bcdc130f0b66d7ff4f6a5a8e"))
