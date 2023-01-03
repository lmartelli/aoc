(ns aoc-2016.day18
  (:require
   [aoc.core :refer :all]
   [aoc.algo :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-string stream))

;; part 1

(def trap-rules
  (map-keys
    seq
    {"..." \.
     "^.." \^
     ".^." \.
     "^^." \^
     "..^" \^
     "^.^" \.
     ".^^" \^
     "^^^" \.}))

(defn next-row [row]
  (->> (concat "." row ".")
       (partition 3 1)
       (mapv trap-rules)))

(defn rows [initial-row]
  (iterate next-row (seq initial-row)))

(defn count-safe-tiles
  ([row]
   (count (filter #(= % \.) row)))
  ([initial-row n]
   (->> (rows initial-row)
        (take n)
        (map count-safe-tiles)
        (reduce +))))

(defpart part1 [input]
  (count-safe-tiles input 40))

;; part 2

(defpart part2 [input]
  (count-safe-tiles input 400000))

;; tests

(deftest next-row-test
  (are [row expected] (= (seq expected) (next-row row))
    "..^^." ".^^^^"
    ".^^^^" "^^..^"))

(deftest count-safe-tiles-test
  (is ( = 38 (count-safe-tiles ".^^.^.^^^^" 10))))

(deftest part1-test ())

(deftest part2-test)
