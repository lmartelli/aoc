(ns aoc-2021.day19
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.math.combinatorics :refer :all]
   [clojure.set :refer [intersection]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (split-seq empty?)
       (map (fn [[_ & coords]]
              (map (comp #(mapv parse-int %) #(split % #",")) coords)))))

;; part 1

(defn translate [delta positions]
  (-> (map #(+ % delta) positions)
      set))

(defn relative-to-min [positions]
  (translate (- (apply min positions)) positions))

(defn translations [beacons-a beacons-b]
  (->> (cartesian-product beacons-a beacons-b)
       (map #(apply - %))
       set))

(defn max-overlapping-beacons [beacons-a beacons-b]
  (->> (translations beacons-a beacons-b)
       (map #(translate % beacons-b))
       (map #(intersection beacons-a %))
       (apply max-key count)
       relative-to-min))
  
(defn count-beacons [scans min-overlapping-beacons]
  (cond
    (empty? scans) 0
    :else (- (->> scans
                  (map count)
                  (reduce +))
             (dec (count scans)))))

(defpart part1 [scans] 0)
;  (count (first scans)))

;; part 2

(defpart part2 [numbers]
  )

;; tests

(def test-data (puzzle-input (test-input *ns*)))

(deftest max-overlapping-beacons-test
  (is (= #{0} (max-overlapping-beacons [0 1] [0 2])))
  (is (= #{0 1} (max-overlapping-beacons [0 1] [0 1])))
  (is (= #{0 1} (max-overlapping-beacons [0 1] [1 0])))
  (is (= #{0 1} (max-overlapping-beacons [0 1] [1 2])))
  (is (= #{0 1} (max-overlapping-beacons [1 2] [0 1])))
  (is (= #{0 1} (max-overlapping-beacons [1 2] [4 5])))
  (is (= #{0 1} (max-overlapping-beacons [4 5] [1 2])))
  (is (= #{0 1} (max-overlapping-beacons [0 1 3] [0 1])))
  (is (= #{0 1} (max-overlapping-beacons [0 1] [0 1 3])))
  (is (= #{0 1} (max-overlapping-beacons [0 2 3] [0 1])))
  (is (= #{0 1} (max-overlapping-beacons [0 1] [0 2 3])))
  (is (= #{0 2} (max-overlapping-beacons [0 1 3] [0 2])))
  (is (= #{0 3} (max-overlapping-beacons [0 1 3] [0 3])))
  (is (= #{0} (max-overlapping-beacons [0 1 3] [0 4])))
  )

(deftest count-beacons-test
  (testing "min-overlapping-beacons == 1"
    (let [min-overlapping-beacons 1]
      (is (= 0 (count-beacons [] min-overlapping-beacons)))
      (is (= 1 (count-beacons [[[0 0 0]]] min-overlapping-beacons)))
      (is (= 1 (count-beacons [[[0 0 0]]
                               [[1 1 1]]]
                              min-overlapping-beacons)))
      (is (= 1 (count-beacons [[[0 0 0]]
                               [[1 1 1]]
                               [[2 2 2]]]
                              min-overlapping-beacons)))
      (is (= 2 (count-beacons [[[0 0 0] [1 2 3]]
                               [[1 1 1]]]
                              min-overlapping-beacons)))
      (is (= 3 (count-beacons [[[0 0 0] [1 2 3]]
                               [[1 1 1] [4 5 6]]]
                              min-overlapping-beacons)))
      )
    )
    (testing "min-overlapping-beacons == 2"
    (let [min-overlapping-beacons 1]
      (is (= 0 (count-beacons [] min-overlapping-beacons)))
      (is (= 1 (count-beacons [[[0 0 0]]] min-overlapping-beacons)))
      (is (= 2 (count-beacons [[[0 0 0] [0 0 1]]] min-overlapping-beacons)))
      (is (= 2 (count-beacons [[[0 0 0] [0 0 1]]
                               [[0 0 0] [0 0 1]]]
                              min-overlapping-beacons)))
      )
    )
  )




;(deftest part1-test-full
;  (is (= 79 (part1 test-data))))

;(deftest part2-test
;  (is (= nil (part2 test-data))))

