(ns aoc-2018.day23
  (:require
   [aoc.core :refer :all]
   [aoc.space-3d :as s3]
   [clojure.math.combinatorics :refer :all]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (map
    #(parse-ints % (fn [x y z r] {:pos [x y z] :radius r}))
    lines))

;; part 1

(defn in-range? [pos nanobot]
  (<= (s3/manatthan-dist (nanobot :pos) pos) (nanobot :radius)))

(defpart part1 [nanobots]
  (let [strongest (apply max-key :radius nanobots)]
    (->> nanobots
         (map :pos)
         (filter #(in-range? % strongest))
         count)))

;; part 2

(defn intersect? [a b]
  (< (s3/manatthan-dist (:pos a) (:pos b)) (+ (:radius a) (:radius b))))

(defpart part2 [nanobots]
  ;; The general case is really difficult
  ;; Need to find the pattern in the data
  (->> (combinations nanobots 3)
       (filter (fn [[a b c]]
                 (and (intersect? a b)
                      (intersect? a c)
                      (intersect? b c))))
       count))

;; tests

(deftest part1-test (part-test part1 "1" 7))

(deftest part2-test (part-test part1 "2" 36))
