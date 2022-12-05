(ns aoc-2021.day17
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (puzzle-input-string stream)
       (re-find #"x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)")
       rest
       (map parse-int)
       (partition 2)))

;; part 1

(defn trajectory [p0 v0 continue? update-v]
  (->> (iterate
         (fn [[p v]]
           [(+ p v) (update-v v)])
         [p0 v0])
       (take-while continue?)
       (map first)))

(defn hit-target? [positions [min max]]
  (some #(<= min % max) positions))

(defn find-high-point [[t-min t-max :as target]]
  (->> (range)
       (map #(trajectory 0 % (fn [[p v]] (>= p t-min)) dec))
       (filter #(hit-target? % target))
       (map (fn [traj] (let [high (apply max traj)]
                         (println ">" high "<" (- (second traj) (first traj)))
                         high)))))

(defpart part1 [[x-range y-range]]
  (find-high-point y-range))

;; part 2

(defn x-trajectory [vx]
  (trajectory
      0
      vx
      (constantly true)
      #(max 0 (dec %))))

(defn y-trajectory [vy t-min]
  (trajectory
    0
      vy
      (fn [[p v]] (>= p t-min))
      dec))

(defn trajectory-2d [[vx vy] y-target-min]
  (map vector
       (x-trajectory vx)
       (y-trajectory vy y-target-min)))

(defn hit-target-2d? [trajectory [[x-target-min x-target-max] [y-target-min y-target-max] :as target]]
  (some (fn [[x y]]
          (and (<= x-target-min x x-target-max)
               (<= y-target-min y y-target-max)))
        trajectory))

(defpart part2 [[[x-target-min x-target-max] [y-target-min y-target-max] :as target]]
  (->> (for [vx (range-inc x-target-max)
             vy (range-inc y-target-min (- y-target-min))]
           [vx vy])
       #_(map #(trajectory-2d % y-target-min))
       (filter #(hit-target-2d? (trajectory-2d % y-target-min) target))
       count))

;; tests

(deftest part1-test (part-test part1 45))

(deftest part2-test (part-test part2 nil))

