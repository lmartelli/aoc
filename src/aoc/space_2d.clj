(ns aoc.space-2d
  (:refer-clojure :exclude [+ -])
  (:require
   [clojure.core :as core]
   [aoc.core :refer [signum min-max range-inc]]
   [aoc.algo :as algo]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn pos-and-values-seq
  "Generates a sequence of [[x y] value] from a seq of seq (rows).
  Y axis is pointing down."
  [rows]
  (mapcat
    (fn [y row]
      (map-indexed (fn [x val] [[x y] val]) row))
    (range) rows))

(defn + [[ax ay] [bx by]]
  [(core/+ ax bx) (core/+ ay by)])

(defn -
  ([[ax ay]]
   [(core/- ax) (core/- ay)])
  ([[ax ay] [bx by]]
   [(core/- ax bx) (core/- ay by)]))

(defn- transform-relative [p origin tx]
  (-> p
      (- origin)
      tx
      (+ origin)))

(defn rotate-left "Y axis points down"
  ([[x y]] [y (core/- x)])
  ([p center] (transform-relative p center rotate-left)))

(defn rotate-right "Y axis points down"
  ([[x y]] [(core/- y)  x])
  ([p center] (transform-relative p center rotate-right)))

(defn manatthan-dist
  ([a b] (manatthan-dist (- a b)))
  ([[^int x ^int y]] (core/+ (abs x) (abs y))))

(defn direct-neighbours [[^int x ^int y]]
  (list [x (inc y)] [x (dec y)] [(inc x) y] [(dec x) y]))

(defn all-neighbours [[^int x ^int y]]
  (list [x (inc y)] [x (dec y)] [(inc x) y] [(dec x) y]
        [(inc x) (inc y)] [(inc x) (dec y)]
        [(dec x) (inc y)] [(dec x) (dec y)]))

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

(defn draw-points
  ([ink points] (draw-points {} ink points))
  ([paper ink points]
   (reduce
     (fn [paper pos] (assoc paper pos ink))
     paper
     points)))

(defn draw-segments [paper ink points]
  (draw-points paper ink (segment-points points)))

(defn draw-segment [paper ink [from to]]
  (reduce
   (fn [paper pos]
     (assoc paper pos ink))
   paper
   (segment-points from to)))

(defn print
  ([paper] (print paper identity))
  ([paper xf]
   (let [positions (keys paper)
         [x-range y-range]
         (reduce
           (fn [[[x-min x-max] [y-min y-max]] [x y]]
             [(apply min-max (remove nil? [x-max x-min x]))
              (apply min-max (remove nil? [y-max y-min y]))])
           [[] []]
           positions)]
     (print paper xf (apply range-inc x-range) (apply range-inc y-range))))
  ([paper x-range y-range]
   (print paper identity x-range y-range))
  ([paper xf x-range y-range]
   (run! (fn [y] (->> (map (fn [x] (if-let [c (paper [x y])] (or (xf c) c) \space)) x-range)
                      str/join
                      println))
         y-range)))

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

(deftest rotations-test
  (testing "rotate around [0 0]"
    (are [p expected] (and (= expected (rotate-left p))
                           (= p (rotate-right expected)))
      [0 0] [0 0]
      [0 1] [1 0]
      [1 0] [0 -1]
      [0 -1] [-1 0]))
  (testing "rotate around [3 1]"
    (let [center [3 1]]
      (are [p expected] (and (= expected (rotate-left p center))
                             (= p (rotate-right expected center)))
        [0 0] [2 4]
        [4 -1] [1 0]))))

