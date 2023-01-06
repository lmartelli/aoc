(ns aoc-2020.day10
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines stream parse-int))

;; part 1

(defn joltages-diffs [joltages]
  (->> (-> joltages (conj 0) (conj (+ 3 (apply max joltages))))
       (sort)
       (partition 2 1)
       (map (fn [[a b]] (- b a)))))

(defpart part1 [input]
  (->> (joltages-diffs input)
       (frequencies)
       (vals)
       (apply *)))

;; part 2

(def count-arrangements
  (memoize
    (fn [joltage-diffs]
      (if (< (count joltage-diffs) 2)
        1
        (let [j1 (first joltage-diffs)
              j2 (second joltage-diffs)]
          (if (<= (+ j1 j2) 3)
            (+' (count-arrangements (rest joltage-diffs))
                (count-arrangements (conj (drop 2 joltage-diffs) (+ j1 j2))))
            (count-arrangements (rest joltage-diffs))))))))

(defpart part2 [input]
  (-> input
      joltages-diffs
      count-arrangements))

;; test

(deftest part1-test
  (are [input expected] (= expected (part1 input))
    [16 10 15 5 1 11 7 19 6 12 4]
    35
    [28 33 18 42 31 14 46 20 48 47 24 23
     49 45 19 38 39 11 1 32 25 35 8 17 7
     9 4 2 34 10 3]
    220))

(deftest part2-test
  (are [input expected] (= expected (part2 input))
    [16 10 15 5 1 11 7 19 6 12 4]
    8
    [28 33 18 42 31 14 46 20 48 47 24 23
     49 45 19 38 39 11 1 32 25 35 8 17 7
     9 4 2 34 10 3]
    19208))
