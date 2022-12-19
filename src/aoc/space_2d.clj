(ns aoc.space-2d
  (:refer-clojure :exclude [+ -])
  (:require
   [clojure.core :as core]
   [aoc.core :refer [signum]]
   [aoc.algo :as algo]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn + [[ax ay] [bx by]]
  [(core/+ ax bx) (core/+ ay by)])

(defn - [[ax ay] [bx by]]
  [(core/- ax bx) (core/- ay by)])

(defn manatthan-dist
  ([a b] (manatthan-dist (- a b)))
  ([[^int x ^int y]] (core/+ (abs x) (abs y))))

(defn direct-neighbours [[^int x ^int y]]
  (list [x (inc y)] [x (dec y)] [(inc x) y] [(dec x) y]))

(defn segment-points [[start-pos & other :as points]]
  (lazy-seq
   (cond
     (empty? points) nil
     (empty? other) (list start-pos)
     :else (let [[x1 y1] start-pos
                 [x2 y2] (first other)]
             (concat
              (cond
                (= x1 x2) (map #(vector x1 %) (range y1 y2 (signum (core/- y2 y1))))
                (= y1 y2) (map #(vector % y1) (range x1 x2 (signum (core/- x2 x1))))
                :else (throw (Exception. (str "Can only draw horizontal or vertical segments: " [x1 y1] " -> " [x2 y2]))))
              (segment-points other))))))

(defn draw-points [paper ink points]
  (reduce
    (fn [paper pos] (assoc paper pos ink))
    paper
    points))

(defn draw-segments [paper ink points]
  (draw-points paper ink (segment-points points)))

(defn draw-segment [paper ink [from to]]
  (reduce
   (fn [paper pos]
     (assoc paper pos ink))
   paper
   (segment-points from to)))

(defn print [paper x-range y-range]
  (run! (fn [y] (->> (map (fn [x] (if-let [c (paper [x y])] c \space)) x-range)
                     str/join
                     println))
        y-range))

;; Tests

(deftest segment-points-test
  (are [points expected] (and (= expected (segment-points points))
                              (= (reverse expected) (segment-points (reverse points))))
    [] []
    [[0 0]] [[0 0]]
    [[0 0] [0 1]] [[0 0] [0 1]]
    [[0 0] [0 2]] [[0 0] [0 1] [0 2]]
    [[0 0] [0 2] [2 2]] [[0 0] [0 1] [0 2] [1 2] [2 2]]
    ))
