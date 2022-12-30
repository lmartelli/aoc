(ns aoc-2018.day06
  (:require
   [aoc.core :refer :all]
   [aoc.algo :as algo]
   [aoc.space-2d :as s2]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (map (comp vec parse-ints))))

;; part 1

(defn bounds [coordinates]
  [(apply min-max (map first coordinates))
   (apply min-max (map second coordinates))])

(defn in-bounds? [[x y] [x-min x-max] [y-min y-max]]
  (and (< x-min x x-max)
       (< y-min y y-max)))

(defn area [coord coordinates]
  (let [[x-range y-range] (bounds coordinates)
        others (->> coordinates
                    (filter (complement #{coord}))
                    #_(sort-by #(s2/manatthan-dist % coord)))
        exploration (algo/explore
                      :start coord
                      :neighbours s2/direct-neighbours
                      :neighbour-allowed? (every? #(< (inc nb-steps) (s2/manatthan-dist % neighbour-pos))
                                                  others)
                      :stop? (or (empty? last-visited)
                                 (some #(not (in-bounds? % x-range y-range)) last-visited)))]
    (if (some #(not (in-bounds? % x-range y-range)) (exploration :last-visited))
      ##Inf
      (-> exploration
          :visited
          count))))

(defpart part1 [coordinates]
  (->> coordinates
       (map #(area % coordinates))
       (filter (complement #{##Inf}))
       (apply max)))

;; part 2

(defn in-safe-zone? [coord coordinates threshold]
  (->> coordinates
       (map #(s2/manatthan-dist coord %))
       (reduce +)
       (> threshold)))

(defn coords-in-bounds [x-range y-range]
  (for [x (apply range-inc x-range)
        y (apply range-inc y-range)]
    [x y]))

;; It is assumed that the "safe zone" is within 
;; the smallest rectangle that contains all the coordinates
(defn safe-zone [coordinates threshold]
  (let [[x-range y-range] (bounds coordinates)]
    (->> (coords-in-bounds x-range y-range)
         (filter #(in-safe-zone? % coordinates threshold))
         count)))

(defpart part2 [coordinates]
  (safe-zone coordinates 10000))

;; tests

(deftest area-test
  (let [coordinates (test-data)]
    (are [coord expected] (= expected (area coord coordinates))
      [1 1] ##Inf
      [1 6] ##Inf
      [8 9] ##Inf
      [8 3] ##Inf 
      [3 4] 9
      [5 5] 17
     )))

(deftest part1-test (part-test part1 17))

(deftest safe-zone-test
  (is (= 16 (safe-zone (test-data) 32))))
