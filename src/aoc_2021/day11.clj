(ns aoc-2021.day11
  (:require
   [aoc.core :refer :all]
   [clojure.set :refer [difference]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines stream digit-vec))

;; part 1

(def grid-width 10)
(def grid-height 10)

(defn neighbours [[x y]]
  (->> [[(dec x) (dec y)] [(dec x) y] [(inc x) (dec y)] [(inc x) y] [x (dec y)] [x (inc y)] [(dec x) (inc y)] [(inc x) (inc y)]]
       (filter (fn [[x y]] (and (< -1 x grid-width) (< -1 y grid-height))))))

(defn map-levels [f levels]
  (mapv #(mapv f %) levels))

(defn map-levels-with-pos [f levels]
  (mapv (fn [row y] (mapv (fn [level x] (f [x y] level)) row (range))) levels (range)))

(defn filter-levels-pos [f levels]
  (->> (array-2d-to-map f levels)
       (map first)))

(defn flash-loop
  ([energy-levels] (flash-loop energy-levels #{}))
  ([energy-levels already-flashed]
   (let [flashes-pos (->> (filter-levels-pos #(> % 9) energy-levels) (filter (comp not already-flashed)))]
     (if (empty? flashes-pos)
       energy-levels
       (recur (let [flashes-neighbours (frequencies (mapcat neighbours flashes-pos))]
                (map-levels-with-pos
                  (fn [pos level] (+ level (get flashes-neighbours pos 0)))
                  energy-levels))
              (into already-flashed flashes-pos))))))

(defn reset-flashed [energy-levels]
  (map-levels #(if (> % 9) 0 %) energy-levels))

(defn step [energy-levels]
  (->> (map-levels inc energy-levels)
       flash-loop
       reset-flashed))

(defn count-flashes [energy-levels]
  (->> (apply concat energy-levels)
       (filter #{0})
       count))

(defpart part1 [energy-levels]
  (->> (iterate step energy-levels)
       (map count-flashes)
       (take 101)
       (reduce +)))

;; part 2

(defpart part2 [energy-levels]
  (->> (iterate step energy-levels)
       (map #(apply concat %))
       (take-while #(some (partial not= 0) %))
       count))

;; tests

(deftest neighbours-test
  (are [x y expected] (= expected (into #{} (neighbours [x y])))
    0 0 #{[1 0] [1 1] [0 1]}
    1 0 #{[2 0] [2 1] [1 1] [0 1] [0 0]}
    1 1 #{[2 1] [2 2] [1 2] [0 2] [0 1] [0 0] [1 0] [2 0]}))

(deftest step-test
  (are [steps expected] (= expected (-> (iterate step (puzzle-input (test-input))) (nth steps)))
    1 (puzzle-input (test-input "step1"))
    2 (puzzle-input (test-input "step2"))))

(deftest part1-test (part-test part1 1656))

(deftest part2-test (part-test part2 195))
