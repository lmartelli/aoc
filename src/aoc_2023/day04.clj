(ns aoc-2023.day04
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.math.numeric-tower :refer [expt]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (puzzle-input-lines stream)
       (re-parse-lines
         #"Card *(\d+): *([\d ]+) *\| *([\d ]+)"
         (fn [c winning yours]
           {:winning (parse-ints winning)
            :yours (parse-ints yours)}))))
;; part 1

(defpart part1 [cards]
  (->> cards
       (map (fn [{:keys [winning yours]}]
              (set/intersection (into #{} winning) (into #{} yours))))
       (remove empty?)
       (map (comp #(expt 2 %) dec count))
       (reduce +)))

;; part 2

(defpart part2 [input]
  input)

;; tests

(deftest part1-test (part-test part1 13))

(deftest part2-test (part-test part2 nil))
