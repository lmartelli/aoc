(ns aoc-2015.day21
  (:require
   [aoc.core :refer :all]
   [aoc-2015.rpg :refer :all]
   [clojure.math.combinatorics :as combi :refer [combinations]]))

(puzzle-input-rpg-properties)

(defn parse-items [coll]
  (->> coll
       (partition 4)
       (map #(zipmap [:name :cost :damage :armor] %))))

(def weapons
  (parse-items
   ["Dagger"	   8    4   0
    "Shortsword"   10   5   0
    "Warhammer"	   25   6   0
    "Longsword"	   40   7   0
    "Greataxe"	   74   8   0]))

(def armors
  (parse-items
   ["Leather"      13   0   1
    "Chainmail"    31   0   2
    "Splintmail"   53   0   3
    "Bandedmail"   75   0   4
    "Platemail"   102   0   5]))

(def rings
  (parse-items
   ["Damage +1"    25   1   0
    "Damage +2"    50   2   0
    "Damage +3"   100   3   0
    "Defense +1"   20   0   1
    "Defense +2"   40   0   2
    "Defense +3"   80   0   3]))

(def player {:hit-points 100})

;; part 1

(defn apply-damage [[attacker defender]]
  [attacker
   (update defender
           :hit-points
           -
           (max 1 (- (attacker :damage) (get defender :armor 0))))])

(defn play-turn [players]
  (reverse (apply-damage players)))

(defn win? [player enemy]
  (->> (iterate play-turn [player (assoc enemy :boss true)])
       (apply concat)
       (find-first dead?)
       :boss))

(def shop-combinations
  (for [weapon weapons
        armor (conj armors nil)
        rings (concat (combinations rings 1) (combinations rings 2) [nil])]
    (merge-with
     (fn [value other]
       (cond
         (int? value) (+ value other)
         (string? value) (str value "," other)))
     weapon armor (nth rings 0 nil) (nth rings 1 nil))))

(defpart part1 [input]
  (->> shop-combinations
       (filter #(win? (merge player %) input))
       (apply min-key :cost)
       :cost))

;; part 2

(def lose? (comp not win?))

(defpart part2 [input]
  (->> shop-combinations
       (filter #(lose? (merge player %) input))
       (apply max-key :cost)
       :cost))

;; tests
