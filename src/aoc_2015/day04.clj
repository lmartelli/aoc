(ns aoc-2015.day04
  (:require [aoc.core :refer :all]
            [clojure.string :refer [split]]))

(puzzle-input-string)

;; part 1

(import java.security.MessageDigest)

(defn md5 [^String s]
  (->> s
       .getBytes
       (.digest (MessageDigest/getInstance "MD5"))
       (BigInteger. 1)
       (format "%032x")))

(defn find-md5 [key re]
  (loop [n 1]
    (if (re-find re (md5 (str key n)))
      n
      (recur (inc n)))))

(defpart part1 [input]
  (find-md5 input #"^0{5}"))

;; part 2

(defpart part2 [input]
  (find-md5 input #"^0{6}"))
