(ns aoc-2015.day03
  (:require [aoc.core :refer :all]
            [clojure.string :refer [split]]))

(puzzle-input-string)

;; part 1

(defn move [pos dir]
     (case dir
       \< (update pos 0 dec)
       \> (update pos 0 inc)
       \^ (update pos 1 dec)
       \v (update pos 1 inc)))

(defn distribute [directions]
  (reduce
       (fn [{pos :pos visited :visited} dir]
         (let [new-pos (move pos dir)]
           {:pos new-pos
            :visited (conj visited new-pos)}))
       {:pos [0 0] :visited #{[0 0]}}
       directions))

(defpart part1 [input]
  (-> (distribute input)
      :visited
      count))

;; part 2

(defn dispatch [coll n]
  (->> coll
       (partition n n (repeat nil))
       (apply map vector)
       remove-nil))

(defpart part2 [input]
  (->> (for [directions (dispatch input 2)]
        (-> (distribute directions)
            :visited))
       (reduce into #{})
       count))
