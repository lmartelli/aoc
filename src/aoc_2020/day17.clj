(ns aoc-2020.day17
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [aoc.space-3d :as s3]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (->> (s2/parse-2d-map-positions lines)
       (map #(vec (conj % 0)))
       (into #{})))

;; part 1

(defn count-neighbours [pos cubes neighbours]
  (->> (neighbours pos)
       (filter cubes)
       count))

(defn expand-range [[min max]]
  [(dec min) (inc max)])

(defn cycle [cubes neighbours]
  (let [ranges (s3/xyz-ranges cubes)]
    (->> (s3/pos-in-ranges (map expand-range ranges))
         (keep (fn [p]
                 (let [nb-neighbours (count-neighbours p cubes neighbours)]
                   (if (cubes p)
                     (when (<= 2 nb-neighbours 3) p)
                     (when (= 3 nb-neighbours) p)))))
         (into #{}))))

(defpart part1 [cubes]
  (-> (iterate cycle cubes)
      (nth 6)
      count))

;; part 2

(defmacro list-neighbours [x y z w]
  (->> (for [X `[~x (inc ~x) (dec ~x)]
             Y `[~y (inc ~y) (dec ~y)]
             Z `[~z (inc ~z) (dec ~z)]
             W `[~w (inc ~w) (dec ~w)]]
         [X Y Z W])
       (remove #(= % `[~x ~y ~z ~w]))
       vec))

(defpart part2 [cubes]
)

;; tests

(deftest part1-test
  (test-with-lines
    part1
    [".#."
     "..#"
     "###"]
    112))

(deftest part2-test
  (test-with-lines
    part2
    [""]
    nil))
