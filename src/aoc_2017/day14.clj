(ns aoc-2017.day14
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [aoc-2017.knot-hash :refer :all]
   [aoc-2017.day12 :refer [groups]]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (first lines))

;; part 1

(def squares-bitmap
  (memoize
   (fn [key]
     (->> (range 128)
          (map #(str key "-" %))
          (map (comp bytes->bin knot-hash))))))
   
(defpart part1 [input]
  (->> input
       squares-bitmap
       (apply concat)
       (filter #{\1})
       count))

;; part 2

(defn build-adjacency-map [bitmap]
  (let [used (->> bitmap
                  (array-2d-to-map #{\1})
                  keys
                  set)]
    (->> used
         (map #(vector % (filter used (s2/direct-neighbours %))))
         (into {}))))

(defpart part2 [input]
  (->> input
       squares-bitmap
       build-adjacency-map
       groups
       count))

;; tests

(deftest part1-test
  (is (= 8108 (part1 "flqrgnkx"))))

(deftest part2-test
  (is (= 1242 (part2 "flqrgnkx"))))
