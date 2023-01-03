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

(defn flip-vert "Y axis points down"
  ([[x y]] [(- x) y])
  ([p [cx cy]] (transform-relative p [cx 0] flip-vert)))

(defn flip-horiz "Y axis points down"
  ([[x y]] [x (- y)])
  ([p [cx cy]] (transform-relative p [0 cy] flip-horiz)))

(defn mult "Vector multiplication by a number: v × n"
  [[x y] n]
  [(* x n) (* y n)])

(defn move [pos dir dist]
  (+ pos (mult dir dist)))

(defn manatthan-dist
  ([a b] (manatthan-dist (- a b)))
  ([[^int x ^int y]] (core/+ (abs x) (abs y))))

(defn direct-neighbours
  ([[^int x ^int y]]
   (list [x (inc y)] [x (dec y)] [(inc x) y] [(dec x) y]))
  ([p [x-min x-max] [y-min y-max]]
   (->> (direct-neighbours p)
        (filter (fn [[x y]]
                  (and (<= x-min x x-max)
                       (<= y-min y y-max)))))))

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

(defn polygon-points [[vertex :as vertices]]
  (rest (segment-points (concat vertices [vertex]))))

(defn draw-polygon [paper ink vertices]
  (draw-points paper ink (polygon-points vertices)))

(defn draw-segment [paper ink [from to]]
  (reduce
   (fn [paper pos]
     (assoc paper pos ink))
   paper
   (segment-points from to)))

(defn box [[x1 y1] [x2 y2]]
  [[x1 y1] [x2 y1] [x2 y2] [x1 y2]])

(defn x-and-y-ranges [positions]
  (reduce
    (fn [[[x-min x-max] [y-min y-max]] [x y]]
      [(apply min-max (remove nil? [x-max x-min x]))
       (apply min-max (remove nil? [y-max y-min y]))])
    [[] []]
    positions))

(defn outter-box [positions]
  (let [[[x-min x-max] [y-min y-max]] (x-and-y-ranges positions)]
    (box [(dec x-min) (dec y-min)] [(inc x-max) (inc y-max)])))


(defn find-min-steps-in-maze
  "`wall-positions` should be a collection of wall positions"
  [from to wall-positions]
  (let [walls (set wall-positions)]
    (-> (algo/explore :start from
                      :stop? (last-visited to)
                      :neighbours direct-neighbours
                      :neighbour-allowed? (not (walls neighbour-pos)))
        :nb-steps)))

(defn print
  ([paper] (print paper identity))
  ([paper xf]
   (let [positions (keys paper)
         [x-range y-range] (x-and-y-ranges positions)]
     (print paper xf (apply range-inc x-range) (apply range-inc y-range))))
  ([paper x-range y-range]
   (print paper identity x-range y-range))
  ([paper xf x-range y-range]
   (run! (fn [y] (->> (map (fn [x] (if-let [c (paper [x y])] (or (xf c) c) \space)) x-range)
                      str/join
                      println))
         y-range)))

(defn print-maze [wall-positions]
  (print (draw-points \█ wall-positions)))

(defn print-to-lines
  ([paper] (print-to-lines paper identity))
  ([paper xf]
   (let [positions (keys paper)
         [x-range y-range] (x-and-y-ranges positions)]
     (print-to-lines paper xf (apply range-inc x-range) (apply range-inc y-range))))
  ([paper x-range y-range]
   (print-to-lines paper identity x-range y-range))
  ([paper xf x-range y-range]
   (map (fn [y] (->> (map (fn [x] (if-let [c (paper [x y])] (or (xf c) c) \space)) x-range)
                     str/join
                     ))
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

