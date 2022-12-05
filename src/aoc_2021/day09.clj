(ns aoc-2021.day09
  (:require
   [aoc.core :refer :all]
   [clojure.set :refer [difference]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines stream digit-vec))

;; part 1

(defn dim [heightmap]
  [(count heightmap) (count (first heightmap))])

(defn neighbours [[x y] [width height]]
  (->> [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]
       (filter (fn [[x y]] (and (< -1 x width) (< -1 y height))))))

(defn find-low-points [heightmap]
  (let [[width height] (dim heightmap)]
    (for [x (range width)
          y (range height)
          :let [h (get-in heightmap [x y])]
          :when (->> (neighbours [x y] [width height])
                     (map #(get-in heightmap %))
                     (every? #(> % h) ))]
      [x y])))

(defpart part1 [heightmap]
  (->> (find-low-points heightmap)
       (map #(inc (get-in heightmap %)))
       (reduce +)))

;; part 2

(defn up-hill-neighbours [boundary heightmap]
  (let [w×h (dim heightmap)]
    (->> (mapcat (fn [pos]
                   (->> (neighbours pos w×h)
                        (filter #(> 9 (get-in heightmap %) (get-in heightmap pos)))))
                 boundary)
         (into #{}))))

(defn basin [heightmap pos]
  (loop [result #{}
         boundary #{pos}]
    (if (empty? boundary)
      result
      (recur (into result boundary)
             (difference (up-hill-neighbours boundary heightmap) result)))))

(defpart part2 [heightmap]
  (->> (find-low-points heightmap)
       (map (comp count (partial basin heightmap)))
       (sort >)
       (take 3)
       (reduce *)))

;; tests

(deftest part1-test (part-test part1 15))

(deftest part2-test (part-test part2 1134))
