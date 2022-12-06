(ns aoc-2022.day06
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-string stream))

;; part 1

(defn find-marker-pos [buffer len]
  (->> (map-indexed vector (partition len 1 buffer))
       (find-first (fn [[pos chars]] (= len (count (set chars)))))
       first
       (+ len)))

(defpart part1 [input]
  (find-marker-pos input 4))

;; part 2

(defpart part2 [input]
  (find-marker-pos input 14))

;; tests

(deftest part1-test
  (are [buffer expected] (= expected (part1 buffer))
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 7
    "bvwbjplbgvbhsrlpgdmjqwftvncz" 5
    "nppdvjthqldpwncqszvftbrmjlhg" 6
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 10
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 11))

(deftest part2-test
  (are [buffer expected] (= expected (part2 buffer))
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 19
    "bvwbjplbgvbhsrlpgdmjqwftvncz" 23
    "nppdvjthqldpwncqszvftbrmjlhg" 23
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 29
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 26))
