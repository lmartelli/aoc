(ns aoc-2015.day16
  (:require
   [aoc.core :refer :all]
   [instaparse.core :as insta]))

(def parser
  (insta/parser
   "AUNT = <'Sue '> INT <': '> COMPOUNDS
    COMPOUNDS = COMPOUND  (<', '> COMPOUND)*
    COMPOUND = #'[a-z]+' <': '> INT
    INT = #'[0-9]+'"))

(def transform
  (partial insta/transform
           {:AUNT vector
            :COMPOUNDS (fn [& compounds] (into {} compounds))
            :COMPOUND (fn [name qty] [(keyword name) qty])
            :INT parse-int}))

(puzzle-input-parse-lines
 (fn [line]
   (->> line
        parser
        transform))
 #(into {} %))

(def clues
  {:children 3
   :cats 7
   :samoyeds 2
   :pomeranians 3
   :akitas 0
   :vizslas 0
   :goldfish 5
   :trees 3
   :cars 2
   :perfumes 1})

;; part 1

(defn match? [clues aunt]
  (every? #(= (clues (key %)) (val %)) aunt))

(defn find-aunt [aunts clues match]
  (->> aunts
       (filter #(match clues (val %)))
       first
       key))

(defpart part1 [input]
  (find-aunt input clues match?))

;; part 2

(defn match-clue [[compound qty] clues]
  (case compound
    (:cats :trees) (> qty (clues compound))
    (:pomeranians :goldfish) (< qty (clues compound))
    (= qty (clues compound))))

(defn match2? [clues aunt]
  (every? #(match-clue % clues) aunt))

(defpart part2 [input]
  (find-aunt input clues match2?))

;; tests
