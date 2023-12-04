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
           [(parse-int c)
            {:winning (into #{} (parse-ints winning))
             :yours (into #{} (parse-ints yours))}]))
       (into {})))
;; part 1

(defn count-matches [{:keys [winning yours]}]
  (count (set/intersection winning yours)))

(defpart part1 [cards]
  (->> (vals cards)
       (map count-matches)
       (filter pos?)
       (map (comp #(expt 2 %) dec))
       (reduce +)))

;; part 2

(def card-score
  (memoize
    (fn [n cards]
      (let [m (count-matches (cards n))]
        (if (zero? m)
          1
          (->> (range m)
               (map #(card-score (+ 1 n %) cards))
               (reduce +)
               inc))))))

(defpart part2 [cards]
  (->> cards
       keys
       (map (fn [n] (card-score n cards)))
       (reduce +)))

;; tests

(deftest part1-test (part-test part1 13))

(deftest part2-test (part-test part2 30))
