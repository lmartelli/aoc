(ns aoc-2022.day02
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(def shapes-mapping
  {:A :rock
   :B :paper
   :C :scissors})

(def score-table
  {[:rock :rock] 3
   [:rock :paper] 6
   [:rock :scissors] 0
   [:paper :rock] 0
   [:paper :paper] 3
   [:paper :scissors] 6
   [:scissors :rock] 6
   [:scissors :paper] 0
   [:scissors :scissors] 3})

(def shape-scores {:rock 1 :paper 2 :scissors 3})

(defn puzzle-input [stream]
  (puzzle-input-split-lines stream #" " #(map keyword %)))

;; part 1

(defn match-score [[opponent-shape self-shape]]
  (+ (shape-scores self-shape)
     (score-table [opponent-shape self-shape])))

(defn total-score [matches]
  (->> matches
       (map match-score)
       (reduce +)))

(defn map-cols [fns coll]
  (map (fn [x] (mapv #(%1 %2) fns x))
        coll))

(defpart part1 [input]
  (->> input
       (map-cols [shapes-mapping {:X :rock :Y :paper :Z :scissors}])
       total-score))

;; part 2

(def reverse-score-table
  (->> (map (fn [[[opponent-shape self-shape] score]]
              [[opponent-shape score] self-shape])
            score-table)
       (into {})))

(defpart part2 [input]
  (->> input
       (map-cols [shapes-mapping {:X 0 :Y 3 :Z 6}])
       (map #(assoc % 1 (reverse-score-table %)))
       total-score))

;; tests

(def test-data (puzzle-input (test-input *ns*)))

(deftest match-score-test
  (are [opponent-shape self-shape score] (= score (match-score [opponent-shape self-shape]))
    :rock :rock (+ 3 1)
    :rock :paper (+ 6 2)
    :rock :scissors (+ 0 3)
    :paper :rock (+ 0 1)
    :paper :paper (+ 3 2)
    :paper :scissors (+ 6 3)
    :scissors :rock (+ 6 1)
    :scissors :paper (+ 0 2)
    :scissors :scissors (+ 3 3)))

(deftest part1-test
  (is (= 15 (part1 test-data))))

(deftest part2-test
  (is (= 12 (part2 test-data))))
