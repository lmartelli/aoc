(ns aoc-2019.day10
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.test :refer :all]
   [clojure.algo.generic.math-functions :refer [atan2]]
   [clojure.math.combinatorics :refer [permutations nth-permutation count-permutations]]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (s2/parse-2d-map-positions)))

;; part 1

(defn range-contains? [a b c]
  (or (< a c b) (> a c b)))

(defn aligned? [[xa ya] [xb yb] [xc yc]]
  (let [rx (/ (- xa xb) (- xa xc))
        ry (/ (- ya yb) (- ya yc))]
    (and (= rx ry) (< 1 rx))))

(defn seg-contains? [[xa ya] [xb yb] [xc yc]]
  (cond
    (= xa xc) (and (= xa xb)
                   (range-contains? ya yb yc))
    (= ya yc) (and (= ya yb)
                   (range-contains? xa xb xc))
    :else (aligned? [xa ya] [xb yb] [xc yc])))

(defn visible? [a s all]
  (empty?
   (for [b all
         :when (and (not= b s)
                    (not= b a)
                    (seg-contains? s a b))]
     b)))

(defn detect [s all]
  (for [a all
        :when (and (not= s a)
                   (visible? a s all))]
    a))

(defn count-detected [all]
  (for [x all]
    [x (count (detect x all))]))

(defn list-detected [all]
  (for [x all]
    [x (detect x all)]))

(defn find-best [all]
  (apply max-key second (count-detected all)))

(defpart part1 [input]
  (-> input find-best last))

;; part 2

(defn phase
  ([[x y]] (- (atan2 x y)))
  ([a b] (phase (map - b a))))

(defn pow2 [x] (* x x))

(defn dist
  ([point] (reduce + (map pow2 point)))
  ([a b] (dist (map - b a))))

(defn pad [col size v]
  (take size (concat col (repeat v))))

(defn map-pad [f p & colls]
  (let [max-size (apply max (map count colls))
        padded-colls (map #(pad % max-size p) colls)]
    (apply map f padded-colls)))

(defn vaporization-order [all station]
  (let [all (remove #{station} all)]
    (->> all
         (sort-by #(dist station %))
         (group-by #(phase station %))
         (into (sorted-map))
         vals
         (apply map-pad list nil)
         (mapcat #(remove nil? %)))))

;; More efficient way for part1
(defn count-detected-2 [s all]
  (count (group-by #(phase s %) (remove #{s} all))))

(defn find-best-station [all]
  (apply max-key
         #(count-detected-2 % all)
         all))

(defpart part2 [input]
  (let [[x y] (nth (vaporization-order input (find-best-station input))
                   199)]
    (+ x (* 100 y))))

;; tests

(deftest read-line-1
  (is (= [[0 0]]
         (read-line "#...." 0))))

(deftest read-line-2
  (is (= [[0 0] [4 0]]
         (read-line "#...#" 0))))

(deftest range-contains?-true
  (are [a b c] (true? (and (range-contains? a b c) (range-contains? b a c)))
    0 2 1
    0 5 4
    -5 -2 -3
    -5 7 0))

(deftest range-contains?-false
  (are [a b c] (false? (and (range-contains? a b c) (range-contains? b a c)))
    0 3 3
    0 3 4
    0 3 0
    0 3 -1))

(deftest seg-contains?-true
  (are [a b c] (true? (and (seg-contains? a b c) (seg-contains? b a c)))
    [0 0] [2 2] [1 1]
    [0 0] [6 3] [2 1]
    [0 0] [0 5] [0 3]
    [0 0] [5 0] [2 0]))

(deftest seg-contains?-false
  (are [a b c] (and (false? (seg-contains? a b c))
                    (false? (seg-contains? b a c)))
    [0 0] [2 2] [-1 -1]
    [0 0] [2 2] [3 3]
    [0 0] [9 9] [8 7]
    [0 0] [0 5] [0 6]
    [0 0] [0 5] [0 -1]
    [0 0] [5 0] [6 0]
    [0 0] [5 0] [-1 0]
    [4 4] [3 2] [4 3]))

(deftest find-best-small
  (is (= [[3 4] 8]
         (find-best (test-data "s")))))

(deftest find-best-medium1
  (is (= [[5 8] 33]
         (find-best (test-data "m1")))))

(deftest find-best-medium2
  (is (= [[1 2] 35]
         (find-best (test-data "m2")))))

(deftest find-best-medium3
  (is (= [[6 3] 41]
         (find-best (test-data "m3")))))

(deftest find-best-large
  (is (= [[11 13] 210]
         (find-best (test-data "l")))))

(deftest phase-test-1-point
  (are [point expected] (== expected (phase point))
    [1 0] 0
    [2 0] 0
    [0 1] (/ Math/PI 2)
    [0 2] (/ Math/PI 2)))

(deftest phase-test-2-points
  (are [a b expected] (== expected (phase a b))
    [0 0] [1 0] 0
    [0 0] [0 1] (/ Math/PI 2)
    [1 1] [2 1] 0
    [1 1] [1 2] (/ Math/PI 2)))

(deftest dist-1-point
  (are [p expected] (every? #(== expected (dist %)) (permutations p))
    [0 0] 0
    [0 1] 1
    [0 -1] 1
    [1 1] 2
    [-1 -1] 2
    [3 4] 25))

(deftest dist-2-points
  (are [a b expected] (every? #(== expected (apply dist %)) (permutations [a b]))
    [0 0] [3 4] 25
    [0 1] [1 0] 2
    [1 2] [3 4] 8))

(deftest vaporization-order-test-small
  (is (= [[[1 0] [1 -1]]
          [[2 0]]
          [[2 1]]
          [[2 2]]
          [[1 2]]
          [[0 2]]
          [[0 1]]
          [[0 0]]]
         (vaporization-order
          (let [input  [[1 0] [1 -1] [2 0] [2 1] [2 2] [1 2] [0 2] [0 1] [0 0]]
                n (rand-int (count-permutations input))
                permutation (nth-permutation input n)]
            (println n permutation)
            permutation)
          [1 1]))))

(deftest vaporization-order-test
  (doseq [asteroids (permutations [[0 -1] [0 -2] [1 -1] [1 0] [0 1] [-1 1] [-1 0]])]
    (is (= [[[0 -1] [0 -2]] [[1 -1]] [[1 0]] [[0 1]] [[-1 1]] [[-1 0]]]
           (vaporization-order asteroids [0 0])))))
