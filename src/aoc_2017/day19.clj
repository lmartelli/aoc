(ns aoc-2017.day19
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]))

(defn puzzle-input [stream]
  (->> (line-seq  stream)
       (array-2d-to-map (complement #{\space}))))

;; part 1

(defn find-start [path-map]
  (->> path-map
       (filter #(= \| (val %)))
       keys
       (apply min-key second)))

(defn find-dir [path-map pos prev-dir]
  (let [turn-left (s2/rotate-left prev-dir)]
    (if (path-map (add pos turn-left))
      turn-left
      (s2/rotate-right prev-dir))))

(defn advance [path-map [pos dir]]
  (case (path-map pos)
    nil nil
    \+ (let [new-dir (find-dir path-map pos dir)]
         [(add pos new-dir) new-dir])
    [(add pos dir) dir]))

(defn path [path-map]
  (->> (iterate #(advance path-map %) [(find-start path-map) [0 1]])
       (take-while #(path-map (first %)))))

(defn pick-letters [path-map]
  (->> (path path-map)
       (map (comp path-map first))
       (filter letter?)
       (apply str)))

(defpart part1 [path-map]
  (pick-letters path-map))

;; part 2

(defpart part2 [path-map]
  (->> (path path-map)
       count))

;; tests
