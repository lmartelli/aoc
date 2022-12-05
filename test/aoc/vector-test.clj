(ns aoc.vector-test
  (:require
   [aoc.vector :refer :all]
   [clojure.test :refer :all]))

(deftest add-test
  (are [in expected] (= expected (apply add in))
       [[-1 3] [2 -5]] [1 -2]
       [[-1 3] [0 0]]  [-1 3]
       [[1 2] [3 4] [5 6]]  [9 12]))

(deftest mult-test
  (are [v n expected] (= expected (mult v n))
    [0 1 2 3 -4]  0 [0 0 0 0 0]
    [0 1 2 3 -4]  1 [0 1 2 3 -4]
    [0 1 2 3 -4]  3 [0 3 6 9 -12]
    [0 1 2 3 -4] -1 [0 -1 -2 -3 4]))

(deftest shift-right-test
  (are [v] (= v (shift-right v 0) (shift-right v (count v)))
    [] [0] [0 1] [0 1 2 3])
  (are [v expected] (= expected (shift-right v 1) (shift-right v (+ 1 (count v))))
    [0] [0]
    [0 1] [1 0]
    [0 1 2 3] [3 0 1 2])
  (are [v expected] (= expected (shift-right v 2) (shift-right v (+ 2 (count v))))
    [0] [0]
    [0 1 2 3] [2 3 0 1]))
