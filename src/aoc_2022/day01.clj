(ns aoc-2022.day01
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn group-lines [lines]
  (if (empty? lines)
    []
    (reduce
     (fn [groups line]
       (if (empty? line)
         (conj groups '[])
         (conj (rest groups) (conj (peek groups) line))))
     '[]
     lines)))

(defn puzzle-input [stream]
  (->> (puzzle-input-lines stream)
       group-lines
       (map #(map parse-int %))))

;; part 1

(defpart part1 [input]
  (->> (map #(reduce + %) input)
       (apply max)))

;; part 2

(defpart part2 [input]
  (->> (map #(reduce + %) input)
       (sort >)
       (take 3)
       (reduce +)))

;; tests

(def test-data (puzzle-input (test-input *ns*)))

(deftest group-lines-test
  (are [lines expected] (= (set expected) (set (group-lines lines)))
    [] []
    ["a"] [["a"]]
    ["a" "" "b"] [["a"] ["b"]]
    ["a" "" "b" "" "c"] [["a"] ["b"] ["c"]]
    ["a" "" "b" "c" "" "d"] [["a"] ["b" "c"] ["d"]]
    ))

(deftest part1-test
  (is (= 24000 (part1 test-data))))

(deftest part2-test
  (is (= 45000 (part2 test-data))))
