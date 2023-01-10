(ns aoc-2016.day01
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.string :refer [split]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-string
   stream
   (fn [input]
     (->> (split input #", *")
          (map #(let [[_ turn dist] (re-matches #"(R|L)(\d+)" %)]
                  {:turn turn, :dist (parse-int dist)}))))))

;; part 1

(defn rotate [dir left-or-right]
  (case left-or-right
    "L" (s2/rotate-left dir)
    "R" (s2/rotate-right dir)))

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
  (s2/move pos dir dist))

(defpart part1 [input]
  (->> input
       compute-dirs
       (reduce make-move initial-pos)
       manatthan-dist))

;; part 2

(defn decompose-move [pos {:keys [dir dist]}]
  (rest
   (reductions
    (fn [pos _] (s2/+ pos dir))
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

(deftest part1-test
  (are [input expected] (= expected (part1 (parse-input-string input)))
    "R2, L3" 5
    "R2, R2, R2" 2
    "R5, L5, R5, R3" 12))

(deftest part2-test
  (are [input expected] (= expected (part2 (parse-input-string input)))
    "R8, R4, R4, R8" 4))
