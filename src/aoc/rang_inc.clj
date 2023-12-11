(ns aoc.range-inc
  (:require
   [clojure.test :refer :all]
   [clojure.math.combinatorics :as combo]))

;; A range [low, high] is a set of contiguous integers low <= i < high

(defn contains? [[r-min r-max] n]
  (<= r-min n r-max))

(defn intersection [[a-min a-max] [b-min b-max]]
  (if (or (> b-min a-max) (< b-max a-min))
    nil
    [(max a-min b-min) (min a-max b-max)]))

(defn slice [[a-min a-max :as a] & bs]
  (->> bs
       (skip-while (fn [[b-min b-max]] (< b-max a-min)))
       (take-while (fn [[b-min b-max]] (<= b-min a-max)))
       (mapcat (fn [[start end]]
                 (cond
                   (< start a-min))
                 [[:start start] [:end end]]))
)
  )

(defn expand
  "Returns the sequence of integers in this range"
  [[start end]]
  (range start (inc end)))

;; Tests

(deftest intersection-test
  (testing "No intersection"
    (are [a b expected] (and (= expected (intersection a b))
                             (= expected (intersection b a)))
      [0 1] [2 3] nil
      [0 1] [3 3] nil))
  (testing "B ⊂ A or A ⊂ B"
    (are [a b expected] (and (= expected (intersection a b))
                             (= expected (intersection b a)))
      [3 6] [3 6] [3 6]
      [3 6] [3 7] [3 6]
      [3 6] [2 6] [3 6]
      [3 6] [2 7] [3 6]))
  (testing "Partial overlap a"
    (are [a b expected] (and (= expected (intersection a b))
                             (= expected (intersection b a)))
      [3 6] [2 3] [3 3]
      [3 6] [6 7] [6 6]
      [3 6] [4 7] [4 6])))

(deftest expand-test
  (doseq [offset (range -2 2)]
    (are [min max expected] (= (map #(+ offset %) expected) (expand [(+ min offset) (+ max offset)]))
      0 0 [0]
      0 1 [0 1]
      0 2 [0 1 2]
      0 3 [0 1 2 3])))

(deftest slice-test
  (let [intervals (->> (combo/selections (range 4) 2) (filter #(apply < %)))]
    (doseq [a intervals, b intervals]
      (let [i (intersection a b)
            slices (slice a b)]
        (testing "One returned range is intersection"
          (is (or (nil? i) (some #{i} slices))))
        (testing "All slices are disjoint"
          (is (every? (fn [[a b]] (nil? (intersection a b))) (combo/combinations slices 2))))
        (testing "The union of all slices = a"
          (is (= (expand a) (sort (mapcat expand slices)))))))))
