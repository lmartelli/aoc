(ns aoc-2016.day09
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [index-of join]]
   [clojure.test :refer :all]))

(puzzle-input-string)

;; part 1

(defn re-find-pos [re s]
  (let [matcher (re-matcher re s)]
    (if (re-find matcher)
      (.start matcher)
      nil)))

;; → [marker-pos sequence-start length times]
(defn find-marker [s pos]
  (when-let [marker-pos (index-of s \( pos)]
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

(deftest decompress-test
  (are [in out] (= out (decompress in))
    "ADVENT" "ADVENT"
    "A(1x5)BC" "ABBBBBC"
    "(3x3)XYZ" "XYZXYZXYZ"
    "(6x1)(1x3)A" "(1x3)A"
    "X(8x2)(3x3)ABCY" "X(3x3)ABC(3x3)ABCY"))

(deftest decompress2-test
  (are [in out] (= out (decompress2 in))
    "ADVENT" "ADVENT"
    "A(1x5)BC" "ABBBBBC"
    "(3x3)XYZ" "XYZXYZXYZ"
    "X(8x2)(3x3)ABCY" "XABCABCABCABCABCABCY"
    "(27x12)(20x12)(13x14)(7x10)(1x12)A" (apply str (repeat 241920 \A))))
