(ns aoc-2015.day06
  (:require [aoc.core :refer :all]))

(defn parse-input-line [line]
  (when-let [[_ cmd x1 y1 x2 y2] (re-matches #"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)" line)]
    [({"turn on" :on, "turn off" :off "toggle" :toggle} cmd)
     [(parse-int x1) (parse-int y1)]
     [(parse-int x2) (parse-int y2)]]))

(puzzle-input-parse-lines parse-input-line)

;; part 1

(def commands {:on (fn [old] true)
               :off (fn [old] false)
               :toggle (fn [old] (not old))})

(defn gen-rect [[x1 y1] [x2 y2]]
  (for [x (range x1 (inc x2))
        y (range y1 (inc y2))]
    [x y]))

(defn run-cmds [commands cmds]
  (->> cmds
       (map #(update % 0 commands))
       (mapcat (fn [[f p1 p2]]
                 (for [p (gen-rect p1 p2)]
                   [f p])))
       (reduce (fn [grid [f p]] (update grid p f)) {})))

(defn count-lit [grid]
  (->> grid
       vals
       (filter true?)
       count))

(defpart part1 [input]
  (->> input
       (run-cmds commands)
       count-lit))

;; part 2

(defn default-input-zero [f]
  (fn [old] (f (or old 0))))

(def commands2 {:on (default-input-zero inc)
                :off (default-input-zero (fn [old] (-> old dec (max 0))))
                :toggle (default-input-zero (fn [old] (+ 2 old)))})

(defn total-brightness [grid]
  (->> grid
       vals
       (reduce +)))

(defpart part2 [input]
  (->> input
       (run-cmds commands2)
       total-brightness))
