(ns aoc-2020.day12
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (->> lines
       (map (juxt first (comp first parse-ints)))))

;; part 1

(defn go [dir]
  (fn [state n]
    (update state :pos s2/+ (s2/mult (s2/direction-vectors dir) n))))

(defn turn [f]
  (fn [state deg]
    (update state :dir #(-> (iterate f %) (nth (/ deg 90))))))

(def instruction-set
  {\N (go :north)
   \S (go :south)
   \E (go :east)
   \W (go :west)
   \L (turn s2/rotate-left)
   \R (turn s2/rotate-right)
   \F (fn [state n] (update state :pos s2/+ (s2/mult (state :dir) n)))})

(defn exec [state instructions instruction-set]
  (reduce
    (fn [state [instr arg]]
      ((instruction-set instr) state arg))
    state
    instructions))

(defn exec-and-measure-dist [state instruction-set instructions]
  (-> state
      (exec instructions instruction-set)
      :pos
      s2/manatthan-dist))

(defpart part1 [instructions]
  (exec-and-measure-dist
    {:pos [0 0] :dir (s2/direction-vectors :east) }
    instruction-set
    instructions))

;; part 2

(defn move-waypoint [dir]
  (fn [state n]
    (update state :dir s2/+ (s2/mult (s2/direction-vectors dir) n))))

(def instruction-set-2
  (merge instruction-set
         {\N (move-waypoint :north)
          \S (move-waypoint :south)
          \E (move-waypoint :east)
          \W (move-waypoint :west)}))

(defpart part2 [instructions]
  (exec-and-measure-dist
    {:pos [0 0] :dir (s2/+ (s2/mult (s2/direction-vectors :east) 10) (s2/direction-vectors :north)) }
    instruction-set-2
    instructions))

;; tests

(deftest part1-test (part-test part1 25))

(deftest part2-test (part-test part2 286))
