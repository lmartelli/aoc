(ns aoc-2017.day07
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.set :refer [difference]]))

(puzzle-input-parse-lines
 #(let [[_ prog weight children] (re-matches #"(\w+) \((\d+)\)(?: -> (.*))?" %)]
    (if children
      [prog {:weight (parse-int weight), :children (split children #", *")}]
      [prog {:weight (parse-int weight)}])))

;; part 1

(defn find-roots [tower]
  (difference
   (set (keys tower))
   (->> (vals tower)
        (mapcat :children)
        set)))

(defpart part1 [input]
  (->> input
       (into {})
       find-roots
       first))

;; part 2

(defpart part2 [input]
  nil)

;; tests
