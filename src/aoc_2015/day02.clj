(ns aoc-2015.day02
  (:require [aoc.core :refer :all]
            [clojure.test :refer :all]
            [clojure.string :refer [split]]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines stream #(map parse-int (split % #"x"))))

;; part 1

(defn surface [[w h l]]
  (let [s1 (* w h)
        s2 (* w l)
        s3 (* h l)]
    (+ (* 2 (+ s1 s2 s3))
       (min s1 s2 s3))))

(defpart part1 [input]
  (reduce + (map surface input)))

;; part 2

(defn ribbon-length [[w h l]]
  (let [p1 (+ w h)
        p2 (+ w l)
        p3 (+ h l)]
    (+ (* 2 (min p1 p2 p3)) (* w h l))))

(defpart part2 [input]
  (reduce + (map ribbon-length input)))

;; Tests

(deftest surface-test
  (are [dimensions expected] (= expected (surface dimensions))
    [2 3 4] 58
    [1 1 10] 43))

(deftest ribbon-length-test
  (are [dimensions expected] (= expected (ribbon-length dimensions))
    [2 3 4] 34
    [1 1 10] 14))
