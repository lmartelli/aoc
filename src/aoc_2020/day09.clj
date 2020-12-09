(ns aoc-2020.day09
  (:require
   [aoc.core :refer :all]
   [clojure.math.combinatorics :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines stream parse-int))

;; part 1

(defn valid? [n previous]
  (->> (combinations previous 2)
       (map #(apply + %))
       (find-first #{n})))

(defn find-invalid [input n]
  (->> (map vector (drop n input) (partition n 1 input))
       (find-first (fn [[number previous]] (not (valid? number previous))))
       first))

(defpart part1 [input]
  (find-invalid input 25))

;; part 2

(defn find-sum
  ([input sum]
   (for [list-size (range 2 (count input))
         :let [numbers (find-sum input sum list-size)]
         :when numbers]
     (+ (apply min numbers) (apply max numbers))))
  ([input sum list-size]
   (->> (partition list-size 1 input)
        (find-first #(= sum (reduce + %))))))

(defpart part2 [input]
  (first (find-sum input (part1 input))))

;; test

(deftest valid?-test
  (let [previous [3 2 1 4 5]]
    (are [n] (valid? n previous)
      3 4 5 6 7 8 9)
    (are [n] (not (valid? n previous))
      1 2 10 11)))

(deftest find-invalid-test
  (is (= 127 (find-invalid [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576] 5))))

(deftest find-sum-test
  (let [input [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576]]
    (is (= [15 25 47 40] (find-sum input 127 4)))
    (is (= [62] (find-sum input 127)))))
