(ns aoc-2017.day14
  (:require
   [aoc.core :refer :all]
   [aoc-2017.knot-hash :refer :all]
   [aoc-2017.day12 :refer [groups]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (first (line-seq stream)))

;; part 1

(def squares-bitmap
  (memoize
   (fn [key]
     (->> (range 128)
          (map #(str key "-" %))
          (map (comp bin knot-hash))))))
   
(defpart part1 [input]
  (->> input
       squares-bitmap
       (apply concat)
       (filter #{\1})
       count))

;; part 2

(defn neighbours [coord]
  (map
   #(add coord %)
   [[0 1] [0 -1] [1 0] [-1 0]]))

(defn build-adjacency-map [bitmap]
  (let [used (->> bitmap
                  (array-2d-to-map #{\1})
                  keys
                  set)]
    (->> used
         (map #(vector % (filter used (neighbours %))))
         (into {}))))

(defpart part2 [input]
  (->> input
       squares-bitmap
       build-adjacency-map
       groups
       count))

;; tests

(deftest neighbours-test
  (is (= #{[2 7] [4 7] [3 6] [3 8]} (set (neighbours [3 7])))))
