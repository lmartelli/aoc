(ns aoc-2024.day04
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (vec (puzzle-input-lines stream)))

;; part 1

(defn rows [input]
  input)

(defn columns [input]
  (apply map #(apply str %&) input))

(defn diagonal-1 [input n]
  (-> (map #(get-in input [%1 (+ n %2)]) (range) (range (count input)))
      str/join))

(defn diagonals [input diag]
  (let [n (max (count input) (count (get input 0)))]
    (->> (map #(diag input %) (range (inc (- n)) n))
         (filter #(not (empty? %))))))

(defn diagonals-1 [input] (diagonals input diagonal-1))

(defn diagonal-2 [input n]
  (-> (map #(get-in input [%1 (+ n %2)]) (range) (reverse (range (count input))))
      str/join))

(defn diagonals-2 [input] (diagonals input diagonal-2))

(defn count-matches [s regex]
  (count (re-seq regex s)))

(defpart part1 [input]
  (let [xmas #"XMAS" samx #"SAMX"]
    (->> (mapcat #(% input) [rows columns diagonals-1 diagonals-2])
         (mapcat (fn [line] (map #(count-matches line %) [xmas samx])))
         (reduce +))))

;; part 2

(defpart part2 [input]
  input)

;; tests

(def test-puzzle
  ["ABC"
   "DEF"
   "GHI"])

(deftest rows-test
  (is (= ["ABC"
          "DEF"
          "GHI"]
         (rows test-puzzle))))

(deftest columns-test
  (is (= (set ["ADG"
               "BEH"
               "CFI"])
         (set (columns test-puzzle)))))

(deftest diagonal-1-test
  (are [board n expected] (= expected
                           (diagonal-1 board n))
    ["A"] 0 "A"
    ["AB"] 1 "B"
    ["A"
     "B"] 0 "A"
    ["A"
     "B"] -1 "B"
    ["AB"
     "CD"] 0 "AD"
    ["AB"
     "CD"] 1 "B"
    ["AB"
     "CD"] -1 "C"
    ))

(deftest diagonals-1-test
  (are [input expected] (= (set expected)
                           (set (diagonals-1 input)))
    ["A"] ["A"]
    ["AB"] ["A" "B"]
    ["A" "B"] ["A" "B"]
    ["AB"
     "CD"] ["C" "AD" "B"]
    ["ABCX"
     "DEFY"
     "GHIZ"] ["G" "DH" "AEI" "BFZ" "CY" "X"]))

(deftest diagonals-2-test
  (are [input expected] (= (set expected)
                           (set (diagonals-2 input)))
    ["A"] ["A"]
    ["AB"] ["A" "B"]
    ["A" "B"] ["A" "B"]
    ["AB"
     "CD"] ["A" "BC" "D"]
    ["ABCX"
     "DEFY"
     "GHIZ"] ["A" "BD" "CEG" "XFH" "YI" "Z"]))

(deftest part1-test
  (part-test part1 18)
  (test-with-lines
    part1
    ["..X..."
     ".SAMX."
     ".A..A."
     "XMAS.S"
     ".X...."]
    4))

(deftest part2-test (part-test part2 nil))

;;(deftest part2-test (test-with-lines part2 [""] nil))
