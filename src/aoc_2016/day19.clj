(ns aoc-2016.day19
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (parse-int (puzzle-input-string stream)))

;; part 1

(defn winner [nb-elves]
  (-> (- nb-elves
         (loop [nb-elves nb-elves
                power-of-2 0]
           (if (= 1 nb-elves)
             (reduce * (repeat power-of-2 2))
             (recur (quot nb-elves 2) (inc power-of-2)))))
      (* 2)
      inc))

(defpart part1 [input]
  (winner-fast input))

;; part 2

(defn winner-is-last-elf []
  (iterate #(* % 3) 1))

(defn winner [nb-elves]
  (let [high (find-first #(>= % nb-elves) (winner-is-last-elf))
        low (/ high 3)]
    (if (<= (- high low) nb-elves)
      (- (* 2 nb-elves) high)
      (- nb-elves low))))

(defpart part2 [input]
  (winner2-fast input))

;; tests

(deftest winner-test
  (are [n expected] (= expected (winner n))
    1 1
    2 1
    3 3
    4 1
    5 3
    6 5
    7 7))

(deftest winner2-test
  (are [n expected] (= expected (winner2-fast n))
    1 1
    2 1
    3 3
    4 1
    5 2
    6 3
    28 1
    29 2
    53 26
    54 27
    55 29
    150 69))
