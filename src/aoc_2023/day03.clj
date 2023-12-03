(ns aoc-2023.day03
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (vec lines))

;; part 1

(defn get-numbers [line]
  (map
    #(update % :match parse-int)
    (re-seq-pos #"\d+" line)))

(defn sym? [x]
  (not (or (digit? x)
           (= \. x))))

(defn border [x-start x-end y]
  (s2/polygon-points (s2/box [(dec y) (dec x-start)]
                             [(inc y) x-end])))

(defn part-number? [x-start x-end y lines]
  (->> (border x-start x-end y)
       (map #(get-in lines %))
       (remove nil?)
       (some sym?)))

(defn- get-all-numbers [lines]
  (mapcat
    (fn [y line]
      (->> line
           get-numbers
           (map #(assoc % :y y))))
    (range) lines))

(defpart part1 [lines]
  (->> (get-all-numbers lines)
       (filter (fn [num]
                 (part-number? (num :start) (num :end) (num :y) lines)))
       (map :match)
       (reduce +)))

;; part 2

(defn adjacent-gears [x-start x-end y lines]
  (->> (border x-start x-end y)
       (filter #(= \* (get-in lines %)))))

(defpart part2 [lines]
  (->> (get-all-numbers lines)
       (mapcat (fn [part]
                 (map
                   #(vector % (part :match))
                   (adjacent-gears (part :start) (part :end) (part :y) lines))))
       multimap
       (filter-kv #(>= (count %2) 2))
       vals
       (map #(reduce * %))
       (reduce +)))

;; tests

(deftest part1-test (part-test part1 4361))

(def test-lines (puzzle-input (test-input)))

(deftest part-number?-test
  (are [y x-start x-end] (part-number? x-start x-end y test-lines)
    0 0 3)
  (are [y x-start x-end] (not (part-number? x-start x-end y test-lines))
    0 0 2))

(deftest part2-test (part-test part2 467835))
