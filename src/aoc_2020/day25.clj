(ns aoc-2020.day25
  (:require
   [aoc.core :refer :all]
   [aoc.crypto :refer :all]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (->> lines
       (map parse-int)))

;; part 1

(def modulus 20201227)

(defn find-loop-size [^long public-key]
  (loop [cur 1
         n 0]
    (if (= cur public-key)
      n
      (recur (-> (unchecked-multiply cur 7) (mod modulus) int)
             (unchecked-inc n)))))

(defn crypto-loop [subject loop-size]
  (modular-exp subject loop-size modulus))

(defpart part1 [public-keys]
  (crypto-loop (second public-keys) (find-loop-size (first public-keys))))

;; part 2

(defpart part2 [public-keys]
  nil)

;; tests

(def data ["5764801" "17807724"])

(deftest find-loop-size-test
  (are [public-key expected] (= expected (find-loop-size public-key))
    5764801 8
    17807724 11))

(deftest part1-test
  (test-with-lines part1 data 14897079))

(deftest part2-test
  (test-with-lines
    part2
    [""]
    nil))
