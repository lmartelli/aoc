(ns aoc-2015.day06
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (re-parse-lines
        #"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)"
        #(vector ({"turn on" :on, "turn off" :off "toggle" :toggle} %1)
                 [(parse-int %2) (parse-int %3)]
                 [(parse-int %4) (parse-int %5)]))))

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

;; Tests

(deftest part1-test (part-test part1 4998))
