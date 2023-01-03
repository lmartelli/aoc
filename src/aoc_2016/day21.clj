(ns aoc-2016.day21
  (:require
   [aoc.core :refer :all]
   [aoc.vector :as v]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn parse-operation [l]
  (condp #(str/starts-with? %2 %1) l
    "move position" (re-parse l #"(\d) to position (\d)" #(vector :move (parse-int %1) (parse-int %2)))
    "rotate right" (re-parse l #"(\d) steps?" #(vector :rotate-right (parse-int %1)))
    "rotate left" (re-parse l #"(\d) steps?" #(vector :rotate-left (parse-int %1)))
    "rotate base" (re-parse l #"position of letter (.)" #(vector :rotate-based (first %1)))
    "swap letter" (re-parse l #"(.) with letter (.)" #(vector :swap-letter (first %1) (first %2)))
    "swap position" (re-parse l #"(\d) with position (\d)" #(vector :swap-position (parse-int %1) (parse-int %2)))
    "reverse" (re-parse l #"(\d) through (\d)" #(vector :reverse (parse-int %1) (parse-int %2)))))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (map parse-operation)))

;; part 1

(defn reverse-range [v low high]
  (reduce #(assoc %1 (+ low %2) (v (- high %2))) v (range (inc (- high low)))))

(defn move-index [v from to]
  (-> v (remove-index from) (insert-at to (v from))))

(deftest reverse-range-test
  (let [v [0 1 2 3]]
    (are [low high expected] (= expected (reverse-range v low high))
      0 0 v
      1 1 v
      0 1 [1 0 2 3]
      0 2 [2 1 0 3]
      0 3 [3 2 1 0]
      1 2 [0 2 1 3]
      2 3 [0 1 3 2])))

(def operations
  {:move (fn [s from to] (move-index s from to))
   :rotate-right (fn [s n] (v/rotate-right s n))
   :rotate-left (fn [s n] (v/rotate-left s n))
   :rotate-based (fn [s c] (let [i (index-of s c)
                                 n (+ i 1 (if (>= i 4) 1 0))]
                             (rotate-right s n)))
   :swap-position (fn [s p1 p2] (v/swap-indices s p1 p2))
   :swap-letter (fn [s c1 c2] (v/swap-indices s (index-of s c1) (index-of s c2)))
   :reverse (fn [s low high] (reverse-range s low high))})

(def reverse-operations
  (merge operations 
         {:move (fn [s from to] (move-index s to from))
          :rotate-right (fn [s n] (v/rotate-left s n))
          :rotate-left (fn [s n] (v/rotate-right s n))
          :rotate-based (fn [s c] (let [i (index-of s c)]
                                    (rotate-right s ([-1 -1 2 -2 1 -3 0 -4] i))))}))

(defn apply-ops [s ops defs]
  (-> (reduce
        (fn [s [op & args]]
          (apply (defs op) s args))
        (vec s)
        ops)
      str/join))

(defpart part1 [input]
  (apply-ops "abcdefgh" input operations))

;; part 2

()

(defpart part2 [input]
  (apply-ops "fbgdceah" (reverse input) reverse-operations))

;; tests

(deftest apply-ops-test (is (= "decab" (apply-ops "abcde" (test-data)))))

(deftest part2-test)
