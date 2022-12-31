(ns aoc-2018.day09
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (parse-ints (puzzle-input-string stream)))

;; part 1

(defn turn [[scores circle :as state] marble]
  (if (multiple? marble 23)
    (let [player (-> (dec marble) (mod (count scores)))
          nb-marbles (count circle)
          removed (mod -7 nb-marbles)]
      [(update scores player + marble (circle removed))
       (into (subvec circle (inc removed)) (subvec circle 0 removed))])
    [scores
     (into [] (concat [marble] (drop 2 circle) (take 2 circle)))]))

(defn marble [nb-players]
  (let [scores (into [] (repeat nb-players 0))]
    (reductions
      turn
      [scores [0]]
      (iterate inc 1))))

(defn highscore [nb-players max-marble]
  (->> (nth (marble nb-players) max-marble)
       first
       (apply max)))

(defpart part1 [[nb-players max-marble]]
  (highscore nb-players max-marble))

;; part 2

(defpart part2 [input]
  nil)

;; tests

(deftest turn-test
  (testing "Score does not change if marble is not a multiple of 23"
    (are [scores circle marble new-scores new-circle] (= [new-scores new-circle] (turn [scores circle] marble))
      [] [0] 1 [] [1 0]
      [] [1 0] 2 [] [2 1 0]
      [] [2 1 0] 3 [] [3 0 2 1]
      [] [3 0 2 1] 4 [] [4 2 1 3 0]))
  (testing "Score changes when marble is multiple of 23"
    (is (= [[1 34 3] [19 2 20 10 21 5 22 11 1 12 6 13 3 14 7 15 0 16 8 17 4 18]]
           (turn [[1 2 3] [22 11 1 12 6 13 3 14 7 15 0 16 8 17 4 18 9 19 2 20 10 21 5]] 23)))))

(deftest highscore-test
  (are [nb-players max-marble expected] (= expected (highscore nb-players max-marble))
    9 25 32
    10 1618 8317
    13 7999 146373
    17 1104 2764
    21 6111 54718
    30 5807 37305))

(deftest part2-test)
