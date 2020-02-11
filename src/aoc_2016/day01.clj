(ns aoc-2016.day01
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]))

(puzzle-input-parse
 (fn [input]
   (->> (split input #", *")
        (map #(let [[_ turn dist] (re-matches #"(R|L)(\d+)" %)]
                {:turn turn, :dist (parse-int dist)})))))

;; part 1

(defn rotate [dir left-or-right]
  (case left-or-right
    "L" (rotate-left dir)
    "R" (rotate-right dir)))

(def initial-pos [0 0])

(defn compute-dirs [moves]
  (map (fn [move dir] (assoc move :dir dir))
       moves
       (rest
        (reductions
         #(rotate %1 %2)
         [0 1]
         (map :turn moves)))))

(defn make-move [pos {:keys [dir dist]}]
  (move pos dir dist))

(defpart part1 [input]
  (->> input
       compute-dirs
       (reduce make-move initial-pos)
       manatthan-dist))

;; part 2

(defn decompose-move [pos {:keys [dir dist]}]
  (rest
   (reductions
    (fn [pos _] (add pos dir))
    pos
    (range dist))))

(defpart part2 [input] 
  (->> input
       compute-dirs
       (reductions #(decompose-move (last %1) %2) [initial-pos])
       (apply concat)
       (reduce
        (fn [visited pos]
          (if (visited pos)
            (reduced pos)
            (conj visited pos)))
        #{})
       manatthan-dist))

;; tests
