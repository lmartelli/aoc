(ns aoc-2015.day12
  (:require
   [aoc.core :refer :all]
   [clojure.data.json :as json]
   [clojure.test :refer :all]))

(puzzle-input-string)

;; part 1

(defpart part1 [input]
  (->> (re-seq #"-?\d+" input)
       (map parse-int)
       (reduce +)))

;; part 2

(defn sum [x]
  (letfn [(sum-object [object]
            (if (some #{"red"} (vals object))
              0
              (->> (vals object)
                   (map sum)
                   (reduce +))))
          (sum-array [arr]
            (->> arr
                 (map sum)
                 (reduce +)))]
  (cond
    (map? x) (sum-object x)
    (vector? x) (sum-array x)
    (int? x) x
    :else 0)))

(defpart part2 [input]
  (sum (json/read-str input)))

;; tests

(deftest sum-test
  (are [input expected] (= expected (sum input))
    [1,2,3] 6
    {"a" 2, "b" 4} 6
    [[[3]]] 3
    {"a" {"b" 4}, "c" -1} 3
    {"a" [-1,1]} 0
    [-1,{"a" 1}] 0
    [] 0
    {} 0
    [1,{"c" "red", "b" 2},3] 4
    {"d" "red","e" [1,2,3,4],"f" 5} 0
    [1,"red",5] 6))
