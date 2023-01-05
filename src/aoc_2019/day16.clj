(ns aoc-2019.day16
  (:require
   [aoc.core :refer :all]
   [clojure.math.numeric-tower :refer [abs]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       first
       digit-seq))

(def base-pattern [0 1 0 -1])

;; part 1

(defn pattern [pos]
  (->> base-pattern
       (mapcat #(repeat pos %))
       cycle
       rest))

(defn apply-pattern [digits pos]
  (-> (reduce + (map * digits (pattern pos)))
      abs
      (mod 10)))

(defn phase [digits]
  (map
   #(apply-pattern digits (inc %))
   (range (count digits))))

(defn phases [digits n]
  (reduce (fn [input _] (phase input)) digits (range n)))

(defpart part1 [input]
   (->> (phases input 100)
        (take 8)
        (apply str)))

(deftest apply-pattern-test
  (doall
   (map
    #(is (= %1 (apply-pattern (range 1 9) %2)))
    '(4 8 2 2 6 1 5 8)
    (range 1 9))))

;; part 2

;; We take advantage of the fact that the offset is in the 2nd half of the signal.
;; And for this half, the FFT is a triangular matrix made of 1's in the upper side,
;; So each digit only depends on the following ones (and it is the sum of them).

(defn signal-offset [input]
  (parse-int (apply str (take 7 input))))

(defn partial-sum
  [signal]
  (reductions (fn [last current] (mod (+ current last) 10)) signal))

(defn transform [signal]
  (nth
   (iterate partial-sum signal)
   100))

(defpart part2 [input]
  (let [offset (signal-offset input)
        signal-tail (take (- (* 10000 (count input)) offset) (cycle (reverse input)))]
    (->> (reverse (transform signal-tail))
         (take 8)
         (apply str))))

;; tests

(deftest part1-test
  (are [input expected] (= expected (part1 (parse-input-string input)))
    "80871224585914546619083218645595" "24176176"
    "19617804207202209144916044189917" "73745418"
    "69317163492948606335995924319873" "52432133"))

(deftest part2-test
  (are [input expected] (= expected (part2 (parse-input-string input)))
    "03036732577212944063491565474664" "84462026"
    "02935109699940807407585447034323" "78725270"
    "03081770884921959731165446850517" "53553731"))
