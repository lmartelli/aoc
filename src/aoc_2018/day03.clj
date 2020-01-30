(ns aoc-2018.day03
  (:require [aoc.core :refer :all]
            [clojure.set :refer :all]
            [clojure.test :refer :all]))

(defn parse-line [l]
  (let [[_ id x y w h] (re-matches #"#(.*) @ (\d+),(\d+): (\d+)x(\d+)" l)]
    {:id id, :left (parse-int x) :top (parse-int y) :width (parse-int w) :height (parse-int h) }))

(puzzle-input-parse-lines parse-line)

;; part 1


(defn gen-rect [left top width height]
  (for [x (range left (+ left width))
        y (range top (+ top height))]
    [x y]))

(defn map-claims [claims]
    (->> claims
       (mapcat (fn [claim]
                 (map #(vector % (claim :id)) (gen-rect (claim :left) (claim :top) (claim :width) (claim :height)))))
       (reduce (fn [acc [coord id]] (update acc coord conj id)) {})))

(defpart part1 [input]
  (->> input
       map-claims
       (filter (fn [[coord ids]] (> (count ids) 1)))
       count))

;; part 2

(defn filter-on-count [claims-map pred]
   (->> claims-map
          vals
          (filter #(pred (count %)))
          (apply concat)
          (into #{})))

(defpart part2 [input]
  (let [m (map-claims input)]
    (difference
     (filter-on-count m (partial = 1))
     (filter-on-count m (partial < 1)))))
  
;; tests
