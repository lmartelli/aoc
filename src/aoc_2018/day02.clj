(ns aoc-2018.day02
  (:require [aoc.core :refer :all]
            [clojure.test :refer :all]
            [clojure.math.combinatorics :refer :all]))

(puzzle-input-lines)

;; part 1

(defn count-matches [coll pred]
  (count (filter pred coll)))

(defn has-occurences [coll n]
  (->> coll
       frequencies
       vals
       (some #{n})))

(defn checksum [ids]
  (* (count-matches ids #(has-occurences % 2))
     (count-matches ids #(has-occurences % 3))))

(defpart part1 [input]
  (checksum input))

;; part 2

(defn similar? [a b]
  (->> (map vector a b)
       (reduce (fn [diffs [a b]]
                 (if (= a b)
                   diffs
                   (if (= 0 diffs)
                     1
                     (reduced 2))))
               0)
       (>= 1)))

(defn commons [& colls]
  (->> (apply map vector colls)
       (reduce (fn [acc items]
                 (if (apply = items)
                   (conj acc (first items))
                   acc))
               [])
       (apply str)))

(defpart part2 [input]
  (first
   (for [[a b] (combinations input 2)
         :when (similar? a b)]
     (commons a b))))
  
;; tests

(deftest has-occurences-test
  (are [coll n] (has-occurences coll n)
    "abc" 1
    "abca" 2
    "abacad" 3)
    (are [coll n] (not (has-occurences coll n))
    "abc" 2
    "abca" 3
    "abacad" 2))

(deftest similar?-test
  (are [a b] (similar? a b)
    "abcd" "abcd"
    "abcd" "!bcd"
    "abcd" "a!cd"
    "abcd" "ab!d"
    "abcd" "abc!")
  )

(deftest commons-test
  (are [a b expected] (= expected (commons a b))
    "abcd" "abcd" "abcd"
    "abcd" "efgh" ""
    "abcd" "!bcd" "bcd"
    "abcd" "a!cd" "acd"
    "abcd" "ab!d" "abd"
    "abcd" "abc!" "abc"))
