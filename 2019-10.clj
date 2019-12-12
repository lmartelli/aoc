(ns aoc.day10
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]))

(defn read-line [line y]
  (for [[c x] (map vector line (range))
        :when (= c \#)]
    [x y]))

(defn read-lines [lines]
  (mapcat read-line lines (range)))

(def puzzle-input
  (-> "2019-10.txt"
      io/resource
      io/reader
      line-seq
      read-lines))

(defn range-contains? [a b c]
  (or (< a c b) (> a c b)))

(defn aligned? [[xa ya] [xb yb] [xc yc]]
  (let [rx (/ (- xa xb) (- xa xc))
        ry (/ (- ya yb) (- ya yc))]
    (and (= rx ry) (< 1 rx))))

(defn seg-contains? [[xa ya] [xb yb] [xc yc]]
  (cond
    (= xa xc) (range-contains? ya yb yc)
    (= ya yc) (range-contains? xa xb xc)
    :else (aligned? [xa ya] [xb yb] [xc yc])))

(defn visible? [a s all]
  (empty?
   (for [b all
         :when (and (not= b s)
                    (not= b a)
                    (seg-contains? s a b))]
     b)))

(defn detect [s all]
  (for [a all
        :when (and (not= s a)
                   (visible? a s all))]
    a))

(defn find-best [all]
  (apply max-key #(count (detect % all)) all))

(defn count-detected [all]
  (for [x all]
    [x (count (detect x all))]))

(defn list-detected [all]
  (for [x all]
    [x (detect x all)]))

(deftest read-line-1
  (is (= [[0 0]]
         (read-line "#...." 0))))

(deftest read-line-2
  (is (= [[0 0] [4 0]]
         (read-line "#...#" 0))))

(deftest read-lines-1
  (is (= [[0 0] [1 1] [2 1]]
         (read-lines ["#...." ".##"]))))

(deftest range-contains?-true
  (are [a b c] (true? (and (range-contains? a b c) (range-contains? b a c)))
    0 2 1
    0 5 4
    -5 -2 -3
    -5 7 0
    ))

(deftest range-contains?-false
  (are [a b c] (false? (and (range-contains? a b c) (range-contains? b a c)))
    0 3 3
    0 3 4
    0 3 0
    0 3 -1))

(deftest seg-contains?-true
  (are [a b c] (true? (and (seg-contains? a b c) (seg-contains? b a c)))
    [0 0] [2 2] [1 1]
    [0 0] [6 3] [2 1]
    [0 0] [0 5] [0 3]
    [0 0] [5 0] [2 0]
    ))

(deftest seg-contains?-false
  (are [a b c] (false? (and (seg-contains? a b c) (seg-contains? b a c)))
    [0 0] [2 2] [-1 -1]
    [0 0] [2 2] [3 3]
    [0 0] [9 9] [8 7]
    [0 0] [0 5] [0 6]
    [0 0] [0 5] [0 -1]
    [0 0] [5 0] [6 0]
    [0 0] [5 0] [-1 0]
    ))

(def test-map-small (read-lines
           [".#..#"
            "....."
            "#####"
            "....#"
            "...##"]))

(deftest find-best-small
  (is (= [3 4]
         (find-best
          (read-lines
           [".#..#"
            "....."
            "#####"
            "....#"
            "...##"])))))
