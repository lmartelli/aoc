(ns aoc-2022.day15
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as v]
   [clojure.math.combinatorics :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (map parse-points)))

;; part 1

(defn intersect-disc-with-horizontal-line [[x y] radius ly]
  (let [D (abs (- y ly)) ;; distance from center to line
        d (- radius D)]
    (if (>= d 0)
      [(- x  d) (+ x d)]
      nil)))

(defn merge-ranges [ranges]
  (loop [[[from to :as range] & more :as ranges] (sort ranges)
         last-to ##-Inf
         merged-ranges []]
    (if (empty? ranges)
      merged-ranges
      (recur
        more
        (max to last-to)
        (cond
          (> from last-to) (conj merged-ranges range)
          (< to last-to ) merged-ranges
          :else (let [[last-from last-to] (peek merged-ranges)]
                  (conj (pop merged-ranges) [last-from to])))))))

(defn range-include? [[from to] x]
  (<= from x to))

(defn ranges-include? [ranges x]
  (some #(range-include? % x) ranges))

(defn count-positions-where-beacon-cannot-be-present [sensors-and-beacons ly]
  (let [ranges (->> (map
                      (fn [[sensor beacon]]
                        (intersect-disc-with-horizontal-line sensor (v/manatthan-dist sensor beacon) ly))
                      sensors-and-beacons)
                    (remove nil?)
                    merge-ranges)
        beacon-xs (->> (map (fn [[sensor beacon]] beacon) sensors-and-beacons)
                       (into #{})
                       (filter (fn [[bx by]] (= by ly)))
                       (map (fn [[bx by]] bx)))]
    (- (count-positions ranges)
       (count (filter #(ranges-include? ranges %) beacon-xs)))))
  
(defpart part1 [input]
  (count-positions-where-beacon-cannot-be-present input 2000000))

;; part 2

(defn tangent-beteween-circle [s1 s2]
  (let [[[[x1 y1] r1 :as s1] 
         [[x2 y2] r2 :as s2]] (sort [s1 s2])]
    (if (> y2 y1)
      [-1 (+ x1 y1 r1 1)]
      [ 1 (- y1 (+ x1 r1 1))])))

(defn line-intersection
  "Intersection between y=a1*x+b1 and y=a2*x+b2"
  [[a1 b1] [a2 b2]]
  (if (= a1 a2)
    nil
    (let [x (-> (- b2 b1) (/ (- a1 a2)))]
      [x (-> (* a1 x) (+ b1))])))

(defn circles-serparated-by-1? [[center-a radius-a] [center-b radius-b]]
  (= (+ radius-a radius-b 2) (v/manatthan-dist center-a center-b)))

;; The beacon must be just next to the zone of one or more sensors.
;; It could be just 1 or 2, if located at a corner of the search area,
;; 2 or 3, if located at the edge of the area
;; Or 4 if located inside the area.
;; We only consider the case of 
(defpart part2 [input]
  (as-> (map (fn [[sensor beacon]] [sensor (v/manatthan-dist sensor beacon)]) input) $
    (combinations $ 2)
    (filter #(apply circles-serparated-by-1? %) $)
    (combinations $ 2)
    (keep (fn [[a b]]
           (let [l1 (apply tangent-beteween-circle a)
                 l2 (apply tangent-beteween-circle b)]
             (line-intersection l1 l2)))
          $)
    (first $)
    (let [[x y] $]
      (-> (* x 4000000) (+ y)))))

;; tests

(deftest intersect-disc-with-horizontal-line-test
  (testing "center of disc on the line"
    (are [center radius ly expected] (= expected (intersect-disc-with-horizontal-line center radius ly))
      [0 0] 0 0 [0 0]
      [1 0] 0 0 [1 1]
      [0 0] 1 0 [-1 1]
      [0 0] 2 0 [-2 2]))
  (testing "disc does not intersec with line"
    (are [center radius ly] (nil? (intersect-disc-with-horizontal-line center radius ly))
      [0 0] 0 1))
  (testing "center is not on the line"
    (are [center radius ly expected] (= expected (intersect-disc-with-horizontal-line center radius ly))
      [0 1] 1 0 [0 0]
      [2 1] 2 -1 [2 2]
      [2 1] 2 0 [1 3])))

(deftest count-positions-test
  (are [ranges expected] (= expected (count-positions ranges))
    [] 0
    [[1 3]] 3
    [[1 3] [4 7]] 7
    [[1 3] [3 7]] 7
    [[1 3] [2 7]] 7
    [[1 7] [2 7]] 7
    [[1 7] [2 4]] 7
    ))

(deftest count-positions-where-beacon-cannot-be-present-test
  (is (= 26 (count-positions-where-beacon-cannot-be-present (test-data) 10))))
  
(deftest part2-test)
