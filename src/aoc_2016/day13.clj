(ns aoc-2016.day13
  (:require
   [aoc.core :refer :all]
   [aoc.algo :as algo]
   [aoc.space-2d :as space-2d]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (-> (puzzle-input-string stream) parse-int))

;; part 1

(defn count-bits [n]
  (loop [n n
         res 0]
    (if (zero? n)
      res
      (recur (bit-shift-right n 1) (+ res (bit-and n 1))))))

(defn is-open-space? [[x y] favorite-num]
  (->> (+ (* x x) (* 3 x) (* 2 x y) y (* y y))
       (+ favorite-num)
       count-bits
       even?))

(defn pos-or-zero? [n] (>= n 0))

(defn allowed-neighbour? [pos favorite-num]
  (and (every? pos-or-zero? pos)
       (is-open-space? pos favorite-num)))

(defn explore [from stop? favorite-num]
  (algo/explore :start from
                :stop? (stop? nb-steps last-visited)
                :neighbours (fn [pos]
                              (->> (space-2d/direct-neighbours pos)
                                   (filter #(every? pos-or-zero? %))))
                :neighbour-allowed? (is-open-space? neighbour-pos favorite-num)))

(def start [1 1])

(defn shortest-path-length [from to favorite-num]
  (-> (explore from (fn [steps explored] (explored to)) favorite-num)
      :nb-steps))

(defpart part1 [input]
  (shortest-path-length start [31 39] input))

;; part 2

(defn count-explored [from max-steps favorite-num]
  (-> (explore from (fn [steps explored] (= steps max-steps)) favorite-num)
      :explored
      count))

(defpart part2 [input]
  (count-explored start 50 input))

;; tests

(deftest count-bits-test
  (are [n expected] (= expected (count-bits n))
    0 0
    1 1
    2 1
    3 2
    4 1
    8 1
    9 2
    10 2
    11 3
    12 2
    13 3
    14 3
    15 4
    16 1
    17 2
    18 2
    19 3
    32 1
    33 2))
  
(deftest part1-test
  (is (= 11 (shortest-path-length [1 1] [7 4] 10))))
