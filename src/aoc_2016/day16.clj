(ns aoc-2016.day16
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (puzzle-input-string stream)
       digit-seq))

;; part 1

(defn fold [bits]
  (concat bits [0] (reverse (map #(if (= 1 %) 0 1) bits))))

(defn fold-seq [bits]
  (iterate (fn [[bits size]]
             [(fold bits)  (-> (* 2 size) inc)])
           [bits (count bits)]))

(defn fold-fill [bits n]
  (->> (fold-seq bits)
       (find-first (fn [[bits size]] (>= size n)))
       first
       (take n)))

(defn checksum [bits]
  (if (odd? (count bits))
    bits
    (recur (->> (partition 2 bits)
                (map #(if (apply = %) 1 0))))))

(defn fill-and-checksum [seed size]
  (-> (fold-fill seed size)
      checksum
      str/join))

(defpart part1 [input]
  (fill-and-checksum input 272))

;; part 2

(defpart part2 [input]
    (fill-and-checksum input 35651584))

;; tests

(deftest fold-test
  (are [in out] (= (digit-seq out) (fold (digit-seq in)))
    "0" "001" 
    "1" "100"
    "11111" "11111000000"
    "111100001010" "1111000010100101011110000"))

(deftest fold-fill-test
  (are [init size expected] (= (digit-seq expected) (fold-fill (digit-seq init) size))
    "10000" 20 "10000011110010000111"))

(deftest checksum-test
  (are [bits expected] (= (digit-seq expected) (checksum (digit-seq bits)))
    "110010110100" "100"))

(deftest part1-test)

(deftest part2-test)
