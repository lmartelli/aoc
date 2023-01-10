(ns aoc-2019.day03
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.string :refer [split]]
   [clojure.math.numeric-tower :refer [abs]]
   [clojure.test :refer :all]))

(def directions {"R" [1 0], "L" [-1 0], "U" [0 1], "D" [0 -1]})

(defn parse-vector [s]
  (let [[_ dir dist] (re-matches #"([RLUD])(\d+)" s)]
    (s2/mult (directions dir) (parse-int dist))))

(defn parse-wire [l]
  (->> (split l #",")
       (map parse-vector)
       (reduce
        (fn [[pos segments] v] [(s2/+ pos v) (conj segments [pos v])])
        [[0 0] []])
       second))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (map parse-wire)))

;; part 1

(defn orthogonal? [v1 v2]
  (every? #{0} (map * v1 v2)))

(defn <or> [a b c]
  (or (< a b c) (> a b c)))

(defn intersection [[p1 v1] [p2 v2]]
  (when (orthogonal? v1 v2)
    (let [[[x1 y1] [_ vert]] (if (= 0 (v1 0)) [p1 v1] [p2 v2])
          [[x2 y2] [horiz _]] (if (= 0 (v1 1)) [p1 v1] [p2 v2])]
      (when
          (and (<or> y1 y2 (+ y1 vert))
               (<or> x2 x1 (+ x2 horiz)))
        [x1 y2]))))

(defn intersections [wire1 wire2]
  (for [s1 wire1, s2 wire2
        :let [cross (intersection s1 s2)]
        :when (some? cross)]
    cross))

(defn dist [v]
  (reduce + (map abs v)))

(defn nearest-intersection [wire1 wire2 f]
  (f (apply min-key f (intersections wire1 wire2))))

(defpart part1 [[wire1 wire2]]
  (nearest-intersection wire1 wire2 dist))

;; part 2

(defn <=or>= [a b c]
  (or (<= a b c) (>= a b c)))

(defn first-if-all-same [coll]
  (if (apply = coll) (first coll) false))

(defn parallel? [v1 v2]
  (->> (map #(cond (every? zero? [%1 %2]) nil
                   (zero? %2) (* ##Inf %1)
                   :else (/ %1 %2))
            v1 v2)
       (remove nil?)
       first-if-all-same))

(defn in-segment? [p [start vect]]
  (if-let [coef (parallel? (s2/- p start) vect)]
    (< 0 coef 1)))

(defn signal-delay
  ([vectors] (reduce + (map dist vectors)))
  ([point wire]
   (let [before-point (take-while #(not (in-segment? point %)) wire)]
     (signal-delay
      (conj (map second before-point) (s2/- point (first (nth wire (count before-point))))))))
  ([point wire1 & wires]
   (reduce + (map #(signal-delay point %) (conj wires wire1)))))

(defpart part2 [[wire1 wire2]]
  (nearest-intersection wire1 wire2 #(signal-delay % wire1 wire2)))

;; tests

(deftest parse-vector-test
  (are [s v] (= v (parse-vector s))
    "R2" [2 0]
    "L3" [-3 0]
    "U4" [0 4]
    "D5" [0 -5]))

(deftest orthogonal?-test
  (are [v1 v2 expected] (and (= expected (orthogonal? v1 v2))
                             (= expected (orthogonal? v2 v1)))
    [0 1] [1 0] true
    [1 0] [0 1] true
    [1 0] [2 0] false
    [0 1] [0 2] false))

(deftest intersection-test
  (are [s1 s2 cross] (and (= cross (intersection s1 s2))
                          (= cross (intersection s2 s1)))
    [[-1 0] [2 0]] [[0 -1] [0 2]] [0 0]
    [[1 3] [2 0]] [[2 2] [0 2]] [2 3])
  (are [s1 s2] (and (nil? (intersection s1 s2))
                    (nil? (intersection s2 s1)))
    [[-1 0] [-2 0]] [[0 -1] [0 -2]]
    [[1 3] [-2 0]] [[2 2] [0 -2]]))

(deftest intersections-test
  (are [wire1 wire2 expected]
      (= expected (intersections (parse-wire wire1) (parse-wire wire2)))
    "R8,U5,L5,D3"
    "U7,R6,D4,L4"
    [[6 5] [3 3]]))

(deftest dist-test
  (are [x y d] (and (= d (dist [x y]))
                    (= d (dist [y x]))
                    (= d (dist [x (- y)]))
                    (= d (dist [(- x) y])))
    0 0  0
    0 1  1
    2 3  5))

(deftest nearest-intersection-with-dist-test
  (are [w1 w2 d]
      (= d (nearest-intersection (parse-wire w1) (parse-wire w2) dist))
    "R8,U5,L5,D3"
    "U7,R6,D4,L4"
    6
    "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    "U62,R66,U55,R34,D71,R55,D58,R83"
    159
    "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
    135))

(deftest in-segment?-test
  (are [p start vector] (and (in-segment? p [start vector])
                             (in-segment? p [(s2/+ start vector) (s2/mult vector -1)]))
    [1 1] [0 1] [2 0]
    [1 1] [1 0] [0 2])
  (are [p start vector] (and (not (in-segment? p [start vector]))
                             (not (in-segment? p [(s2/+ start vector) (s2/mult vector -1)])))
    [1 1] [0 1] [0 2]
    [1 1] [1 0] [2 0]))

(deftest signal-delay-test
  (are [wire point delay] (= delay (signal-delay point (parse-wire wire)))
    "R8,U5,L5,D3" [3 3] 20
    "U7,R6,D4,L4" [3 3] 20
    "R8,U5,L5,D3" [6 5] 15
    "U7,R6,D4,L4" [6 5] 15))

(deftest nearest-intersection-with-signal-delay-test
  (are [wire1 wire2 d]
      (= d (let [w1 (parse-wire wire1)
                 w2 (parse-wire wire2)]
             (nearest-intersection w1 w2 #(signal-delay % w1 w2))))
    "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    "U62,R66,U55,R34,D71,R55,D58,R83"
    610
    "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
    410))

