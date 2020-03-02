(ns aoc-2017.day12
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.set :reger [difference]]))

(puzzle-input-parse-lines
 (fn [line]
   (let [[left-hand right-hand] (split line #" <-> ")]
     [(parse-int left-hand) (map parse-int (split right-hand #", "))]))
 #(into {} %))

;; part 1

(defn transitive-closure [pipes root]
  (loop [closure #{root}, prev #{}]
    (if (= closure prev)
      closure
      (recur (reduce #(into %1 (pipes %2)) closure closure) closure))))

(defpart part1 [input]
  (count (transitive-closure input 0)))

;; part 2

(defn groups [pipes]
  (loop [groups []
         remaining pipes]
    (if (empty? remaining)
      groups
      (let [group (transitive-closure remaining (first (keys remaining)))]
        (recur (conj groups group)
               (apply dissoc remaining group))))))

(defpart part2 [input]
  (count (groups input)))

;; tests
