(ns aoc-2021.day09
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.set :refer [difference]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines stream digit-vec))

;; part 1

(defn dim [heightmap]
  [(count heightmap) (count (first heightmap))])

(defn find-low-points [heightmap]
  (let [[width height] (dim heightmap)]
    (for [x (range width)
          y (range height)
          :let [pos [x y]
                h (get-in heightmap pos)]
          :when (->> (s2/direct-neighbours pos)
                     (keep #(get-in heightmap %))
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
                   (->> (s2/direct-neighbours pos)
                        (filter #(if-let [neighbour-height (get-in heightmap %)]
                                   (> 9 neighbour-height (get-in heightmap pos))))))
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
