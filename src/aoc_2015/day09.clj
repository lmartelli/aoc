(ns aoc-2015.day09
  (:require
   [aoc.core :refer :all]
   [clojure.math.combinatorics :refer [permutations]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines
   stream
   #(let [[_ from to dist] (re-matches #"([^ ]+) to ([^ ]+) = (\d+)" %)]
      [from to (parse-int dist)])))

;; part 1

(defn distances [input]
  (reduce
   (fn [m [from to dist]]
     (-> m
         (assoc-in [from to] dist)
         (assoc-in [to from] dist)))
   {}
   input))

(defn dist [path distances]
  (first
   (reduce
    (fn [[dist pos] next]
      [(+ dist (get-in distances [pos next])) next])
    [0 (first path)]
    (rest path))))

(defn optimize-dist [distances f]
  (->> (keys distances)
       permutations
       (map #(dist % distances))
       (apply f)))

(defpart part1 [input]
  (optimize-dist (distances input) min))

;; part 2

(defpart part2 [input]
    (optimize-dist (distances input) max))

;; tests

(deftest part1-test (part-test part1 605))

(deftest part1-test (part-test part2 982))
