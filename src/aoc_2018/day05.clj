(ns aoc-2018.day05
  (:require [aoc.core :refer :all]
            [clojure.test :refer :all]))

(puzzle-input-string)

;; part 1

(defn upper-case [^Character c]
  (when c
    (Character/toUpperCase c)))

(defn react? [a b]
  (and (not= a b)
       (apply = (map #(upper-case %) [a b]))))

(defn react [units]
  (reduce
   (fn [result unit]
     (if (react? unit (peek result))
       (pop result)
       (conj result unit)))
   []
   units))

(defpart part1 [input]
  (-> input react count))


;; part 2

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(defpart part2 [input]
  (let [reduction (react input)]
    (->> (for [unit (char-range \A \Z)]
           (remove #(or (= % unit) (= (upper-case %) unit)) reduction))
         (map #(count (react %)))
         (apply min))))

;; tests
