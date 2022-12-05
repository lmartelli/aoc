(ns aoc-2015.day04
  (:require [aoc.core :refer :all]
            [aoc.md5 :refer :all]
            [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-string stream))

;; part 1

(defn md5-seq [key]
  (map #(md5 (str key %)) (range)))

(defn byte-to-quad-bits [b]
  (let [int-value (bit-and b 0xff)]
    [(quot int-value 16) (mod int-value 16)]))

(defn bytes-to-quadbits [bytes]
  (mapcat byte-to-quad-bits bytes))

(defn has-nb-leading-zeros? [n bytes]
  (->> (bytes-to-quadbits bytes)
       (take n)
       (every? zero?)))

(defn find-salt [key nb-leading-zeros]
  (find-first
   (fn [salt]
     (->> (md5 (str key salt))
          (has-nb-leading-zeros? nb-leading-zeros)))
   (range)))

(defpart part1 [input]
  (find-salt input 5))

;; part 2

(defpart part2 [input]
  (find-salt input 6))

;; Tests

(deftest part1-test
  (are [key expected] (= expected (part1 key))
    "abcdef" 609043
    "pqrstuv" 1048970))
