(ns aoc-2019.day04
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (as-> (line-seq stream) $
    (first $)
    (split $ #"-")
    (map parse-int $)))

;; part 1


(defn increasing? [s]
  (apply <= (digit-seq s)))

(defn same-adjacent-digits? [s]
  (when-let [[_ d] (re-matches #".*(.)\1.*" s)]
    d))

(defn password? [s]
  (and (increasing? s) (same-adjacent-digits? s)))

(defn count-passwords [from to f]
  (->> (range from (inc to))
       (map str)
       (filter f)
       count))

(defpart part1 [[from to]]
  (count-passwords from to password?))

;; part 2

(defn same-adjacent-digits2?
  "Only works if (increasing? s)"
  [s]
  (->> (frequencies s)
      vals
      (some #{2})))

(defpart part2 [[from to]]
  (count-passwords from to #(and (increasing? %) (same-adjacent-digits2? %))))

;; tests

(deftest increasing?-test
  (are [s] (increasing? s)
    "111" "112" "122" "123" "129")
  (are [s] (not (increasing? s))
    "110" "010" "998" "100"))

(deftest same-adjacent-digits?-test
  (are [s] (not (same-adjacent-digits? s))
    "012345" "987654")
  (are [s] (same-adjacent-digits? s)
    "0122345" "0012345" "0123455" "111111" "011111" "000001"))

(deftest same-adjacent-digits2?-test
  (are [s] (not (same-adjacent-digits2? s))
    "012345" "0001234" "111222" "122234" "123444" "111111" "000001" "1222222")
  (are [s] (same-adjacent-digits? s)
    "0122345" "0012345" "0123455"))
