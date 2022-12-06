(ns aoc-2015.day18
  (:require
   [aoc.core :refer :all]))

(defn puzzle-input [stream]
  (-> (line-seq stream)
      array-2d-to-map))

;; part 1

(defn alive? [state]
  (= \# state))

(defn neighbours [m coord]
  (map
   #(m (add coord %))
   [[0 1] [0 -1] [1 0] [-1 0] [1 1] [-1 -1] [1 -1] [-1 1]]))

(defn alive-neighbours [m coord]
  (->> (neighbours m coord)
       (filter #{\#})
       count))

(defn next-state [m coord]
  (if (alive? (m coord))
    (when (#{2 3} (alive-neighbours m coord)) \#)
    (when (= 3 (alive-neighbours m coord)) \#)))

(defn game-of-life [m]
  (into
   {}
   (map (fn [[coord state]] [coord (next-state m coord)]) m)))

(defn count-alive [m]
  (->> (vals m)
       (filter alive?)
       count))

(defn count-alive-after [m n game]
  (-> (iterate game m)
      (nth n)
      count-alive))

(defpart part1 [input]
  (count-alive-after input 100 game-of-life))

;; part 2

(defn broken-board [m]
  (assoc m [0 0] \# [0 99] \# [99 0] \# [99 99] \#))

(defpart part2 [input]
  (count-alive-after
   (broken-board input)
   100
   (comp broken-board game-of-life)))

;; tests

(def test-data
  (array-2d-to-map
   [".#.#.#"
    "...##."
    "#....#"
    "..#..."
    "#.#..#"
    "####.."]))
