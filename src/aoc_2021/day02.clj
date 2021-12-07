(ns aoc-2021.day02
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines
    stream
    (fn [line]
      (let [[command param] (split line #" ")]
        [(keyword command) (parse-int param)]))))

;; part 1

(defn exec [[pos depth] [cmd param]]
  (case cmd
    :forward [(+ pos param) depth]
    :down    [pos (+ depth param)]
    :up      [pos (- depth param)]
    ))

(defn calc-pos-and-depth
  ([input] (calc-pos-and-depth [0 0] input))
  ([pos-and-depth input]
   (iterate-with [0 0] input exec)))

(defpart part1 [input]
  (->> (calc-pos-and-depth input)
       (apply *)))

;; part 2

(defn exec-2 [[pos depth aim] [cmd param]]
  (case cmd
    :forward [(+ pos param) (+ depth (* aim param)) aim]
    :down    [pos depth (+ aim param)]
    :up      [pos depth (- aim param)]))

(defn calc-pos-and-depth-2
  ([input] (calc-pos-and-depth-2 [0 0 0] input))
  ([pos-and-depth input]
   (iterate-with [0 0 0] input exec-2)))

(defpart part2 [input]
  (->> (calc-pos-and-depth-2 input)
       (take 2)
       (apply *)))

;; tests

(def test-data
  [[:forward 5]
   [:down 5]
   [:forward 8]
   [:up 3]
   [:down 8]
   [:forward 2]])

(deftest calc-pos-and-depth-test
  (are [input pos depth] (= [pos depth] (calc-pos-and-depth input))
    [] 0 0
    [[:forward 3]] 3 0
    [[:down 5]] 0 5
    [[:up 5]] 0 -5
    test-data 15 10))

(deftest calc-pos-and-depth-2-test
  (are [input pos depth] (= [pos depth] (take 2 (calc-pos-and-depth-2 input)))
    test-data 15 60))
