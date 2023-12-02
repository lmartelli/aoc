(ns aoc-2023.day01
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-lines stream))

;; part 1

(defn get-calibration-value [line]
  (->> line
       (filterv digit?)
       ((juxt first peek))
       (apply str)
       parse-int))

(defpart part1 [input]
  (->> input
       (map get-calibration-value)
       (reduce +)))

;; part 2

(def spelled-digits
  (merge
    (zipmap ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"] (range 1 10))
    (into {} (map (juxt str identity) (range 1 10)))))

(def first-digits-regex
  (re-pattern (str/join "|" (keys spelled-digits))))

(def last-digits-regex
  (re-pattern (str/join "|" (map (comp str/join reverse) (keys spelled-digits)))))

(defn get-calibration-value-2 [line]
  (->> line
       ((juxt #(re-find first-digits-regex %)
              #(->> (str/reverse %)
                    (re-find last-digits-regex)
                    (str/reverse))))
       (map spelled-digits)
       (apply str)
       parse-int))

(defpart part2 [input]
    (->> input
       (map get-calibration-value-2)
       (reduce +)))

;; tests


(deftest part1-test (part-test part1 "1" 142))

(deftest get-calibration-value-test
  (are [line calibration-value] (= calibration-value (get-calibration-value line))
    "1abc2" 12
    "pqr3stu8vwx" 38
    "a1b2c3d4e5f" 15
    "treb7uchet" 77))

(deftest part2-test (part-test part2 "2" 281))

(deftest get-calibration-value-2-test
  (are [line calibration-value] (= calibration-value (get-calibration-value-2 line))
    "two1nine" 29))
