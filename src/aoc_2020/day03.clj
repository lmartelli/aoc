(ns aoc-2020.day03
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(puzzle-input-lines)

;; part 1

(defn is-tree? [row pos]
  (= \# (get row (mod pos (count row)))))

(defn every-nth [coll n]
  (map first (partition 1 n coll)))

(defn count-trees
  ([m slope-x] (count-trees m slope-x 1))
  ([m slope-x slope-y]
   (->> (map is-tree? (every-nth m slope-y) (map #(* slope-x %) (range)))
        (filter identity)
        count)))

(defpart part1 [input]
  (count-trees input 3))

;; part 2

(defpart part2 [input]
  (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
       (map #(apply count-trees input %))
       (reduce *)))

;; test
(deftest is-tree?-test
  (let [row "..#.##"]
    (are [pos] (is-tree? row pos)
      2 4 5 8 10 11)
    (are [pos] (not (is-tree? row pos))
      0 1 3 6 7 9)))

(deftest every-nth-test
  (let [coll (range 10)]
    (are [n expected] (= expected (every-nth coll n))
      1 [0 1 2 3 4 5 6 7 8 9]
      2 [0 2 4 6 8]
      3 [0 3 6 9]
      4 [0 4 8])))

(deftest count-trees-test
  (let [m ["..#.."
           ".#.##"
           "..#.."
           ".#..#"
           "..#.#"
           "#.##."
           ".##.#"]]
    (are [slope expected] (= expected (count-trees m slope))
      1 5
      2 3)
    (are [slope-x slope-y expected] (= expected (count-trees m slope-x slope-y))
      1 1 5
      2 1 3
      1 2 1
      1 3 2
      3 2 1)))
