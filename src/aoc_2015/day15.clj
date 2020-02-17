(ns aoc-2015.day15
  (:require
   [aoc.core :refer :all]))

(puzzle-input-parse-lines
 (fn [line]
   (let [[_ ingredient capacity durability flavor texture calories]
         (re-matches #"(\w*):.* (-?\d+),.* (-?\d+),.* (-?\d+),.* (-?\d+),.* (-?\d+)" line)]
     {:name ingredient
      :capacity (parse-int capacity)
      :durability (parse-int durability)
      :flavor (parse-int flavor)
      :texture (parse-int texture)
      :calories (parse-int calories)})))

;; part 1

(defn property-score [properties recipe property]
  (->> (map (fn [ingredient qty] (* qty (ingredient property)))
            properties recipe)
       (reduce +)
       (max 0)))

(defn score [properties recipe]
  (->> [:capacity :durability :flavor :texture]
       (map #(property-score properties recipe %))
       (reduce *)))

(def recipes-seq
  (let [range (range 0 101)]
    (for [a range, b range, c range, d range
               :when (= 100 (+ a b c d))]
           [a b c d])))

(def recipes-seq
  (combinations-with-sum 4 100))

(defn find-best-recipe [properties seq]
  (->> seq
       (map #(score properties %))
       (apply max)))

(defpart part1 [input]
  (find-best-recipe input recipes-seq))

;; part 2

(defn calories [properties recipe]
  (property-score properties recipe :calories))

(defpart part2 [properties]
  (find-best-recipe
   properties
   (filter #(= 500 (calories properties %)) recipes-seq)))

;; tests
