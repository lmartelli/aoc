(ns aoc-2022.day25
  (:require
   [aoc.core :refer :all]
   [clojure.set :refer [map-invert]]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)))

;; part 1

(defn parse-snafu [s]
  (->> (map {\2 2, \1 1, \0 0, \- -1, \= -2} s)
       reverse
       (map * (iterate #(* 5 %) 1))
       (reduce +)))

(defn format-snafu [n]
  (loop [n n
         formatted nil]
    (if (zero? n)
      (if (empty? formatted)
        "0"
        (str/join formatted))
      (let [r (rem n 5)
            q (quot n 5)]
        (recur
          (if (<= r 2) q (inc q))
          (conj formatted ({0 \0, 1 \1, 2 \2, 3 \=, 4 \-} r)))))
      ))

(defpart part1 [input]
  (->> input
       (map parse-snafu)
       (reduce +)
       (format-snafu)))

;; part 2

(defpart part2 [input]
  nil)

;; tests

(deftest snafu-test
  (are [snafu decimal] (and (= decimal (parse-snafu snafu))
                            (= snafu (format-snafu decimal)))
    "0" 0
    "1" 1
    "1=" 3
    "1-" 4
    "10" 5
    "11" 6
    "12" 7
    "2=" 8
    "2-" 9
    "20" 10
    "1=0" 15
    "1-0" 20
    "1=11-2" 2022
    "1-0---0" 12345
    "1121-1110-1=0" 314159265))

(deftest part1-test (part-test part1 "2=-1=0"))

(deftest part2-test)
