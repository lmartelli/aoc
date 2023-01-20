(ns aoc-2020.day13
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(def-input-parser [[l1 l2]]
  {:earliest-departure-time (parse-int l1)
   :buses (->> (str/split l2 #",")
               (map parse-int-or-keyword))})

;; part 1

(defn wait-time [timestamp bus]
  (- bus (mod timestamp bus)))

(defpart part1 [{:keys [earliest-departure-time buses]}]
  (->> buses
       (filter int?)
       (associate #(wait-time earliest-departure-time %))
       (apply min-key val)
       (reduce *)))

;; part 2

(defn solve-mod-eqn
  [a n b m]
  (->> (range)
       (map #(-> % (* n) (+ a)))
       (find-first #(congruent? % (- b) m))))

(defn find-min-timestamp [buses-offsets]
  (loop [[[offset bus] & more] buses-offsets
         k 0
         n 1]
    ;; t ≡ k mod n
    ;; t ≡ offset mod bus
    (if (nil? bus)
      k
      (recur
        more
        (mod (solve-mod-eqn k n offset bus) (* n bus))
        (* n bus)))))

(defpart part2 [{buses :buses}]
  (->> buses
       (map-indexed vector)
       (filter (comp int? second))
       find-min-timestamp))

;; tests

(deftest part1-test (part-test part1 295))

(deftest part2-test
  (are [input expected] (= expected (part2 (parse-input-lines ["1" input])))
    "3" 0
    "x,3" 2
    "5,7" 20
    "x,3,5" 8
    "17,x,13,19" 3417
    "67,7,59,61" 754018
    "67,x,7,59,61" 779210
    "1789,37,47,1889" 1202161486
    "7,13,x,x,59,x,31,19" 1068781))
