(ns aoc-2016.day18
  (:require
   [aoc.core :refer :all]
   [aoc.algo :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-string stream))

;; part 1

(defn next-row [^BigInteger row len]
  (let [left (-> row (.shiftLeft 1) (.clearBit len))
        right (-> row (.shiftRight 1))]
    (.xor left right)))

(defn bigint-row ^BigInteger [row]
  (BigInteger. (str/join (map {\^ 1 \. 0} row)) 2))

(defn print-row [^BigInteger row len]
  (-> (map (fn [n] (if (.testBit row n) \^ \.)) (range len))
      reverse
      str/join))

(defn rows [initial-row]
  (let [len (count initial-row)]
    (iterate #(next-row % len) (bigint-row initial-row))))

(defn count-safe-tiles [initial-row n]
  (let [len (count initial-row)]
    (->> (rows initial-row)
         (take n)
         (map (fn [^BigInteger row]
                (- len (.bitCount row))))
         (reduce +))))

(defpart part1 [input]
  (count-safe-tiles input 40))

;; part 2

(defpart part2 [input]
  (count-safe-tiles input 400000))

;; tests

(deftest next-row-test
  (are [row expected] (= (bigint-row expected) (next-row (bigint-row row) (count row)))
    "..^^." ".^^^^"
    ".^^^^" "^^..^"
    ".^^.^.^^^^" "^^^...^..^"
    "^^^...^..^" "^.^^.^.^^."
    "^.^^.^.^^." "..^^...^^^"
    "..^^...^^^" ".^^^^.^^.^"
    ".^^^^.^^.^" "^^..^.^^.."))

(deftest count-safe-tiles-test
  (is ( = 38 (count-safe-tiles ".^^.^.^^^^" 10))))
