(ns aoc-2016.day09
  (:require
   [aoc.core :refer :all]
   [clojure.string :as s]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-string stream))

;; part 1

(defn re-find-pos [re s]
  (let [matcher (re-matcher re s)]
    (if (re-find matcher)
      (.start matcher)
      nil)))

;; → [marker-pos sequence-start length times]
(defn find-marker [s pos]
  (when-let [marker-pos (s/index-of s \( pos)]
    (let [[marker length times] (re-find #"\((\d+)x(\d+)\)" (subs s marker-pos))]
      [marker-pos (+ marker-pos (count marker)) (parse-int length) (parse-int times)])))

(defn decompress-length [s sub-seq-length]
  (loop [res 0
         pos 0]
    (if-let [marker (find-marker s pos)]
      (let [[marker-pos seq-start length times] marker
            seq-end (+ seq-start length)
            sequence (subs s seq-start seq-end)]
        (recur
         (+ res
            (- marker-pos pos)
            (* times (sub-seq-length sequence)))
         seq-end))
      (+ res (- (count s) pos)))))

(defpart part1 [input]
  (decompress-length input count))

;; part 2

(defpart part2 [input]
  (decompress-length
   input
   (fn sub-seq-length [s] (decompress-length s sub-seq-length))))

;; tests

(deftest part1-test
  (are [input expected] (= expected (part1 input))
    "ADVENT" 6
    "A(1x5)BC" 7
    "(3x3)XYZ" 9
    "A(2x2)BCD(2x2)EFG" 11
    "(6x1)(1x3)A" 6
    "X(8x2)(3x3)ABCY" 18))

(deftest part2-test
  (are [input expected] (= expected (part2 input))
    "ADVENT" 6
    "A(1x5)BC" 7
    "(3x3)XYZ" 9
    "X(8x2)(3x3)ABCY" 20
    "(27x12)(20x12)(13x14)(7x10)(1x12)A" 241920
    "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" 445))
