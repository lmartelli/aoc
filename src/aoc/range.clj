(ns aoc.range
  (:require
   [clojure.test :refer :all]
   [clojure.math.combinatorics :as combo]))

;; A range [low, high] is a set of contiguous integers low <= i < high

(defn intersection [[a-min a-max] [b-min b-max]]
  (if (or (>= b-min a-max) (<= b-max a-min))
    nil
    [(max a-min b-min) (min a-max b-max)]))

(defn slice
  "Returns (a ∩ b) followed by one or two intervals representing (a - b)"
  [a b]
  (let [i (intersection a b)]
    (if (nil? i)
      [nil a]
      (let [[i-min i-max] i
            [a-min a-max] a
            [b-min b-max] b]
        (cond
          ;; a: |     |
          ;; i:   | |
          (< a-min i-min i-max a-max) [i [a-min i-min] [i-max a-max]]
          ;; a: |     |
          ;; i:   |   |
          (> i-min a-min) [i [a-min i-min]]
          ;; a: |     |
          ;; i: |   |
          (< i-max a-max) [i [i-max a-max]]
          ;; a: |     |
          ;; i: |     |
          :else [i])))))

(defn expand
  "Returns the sequence of integers in this range"
  [r]
  (apply range r))

;; Tests

(deftest intersection-test
  (testing "No intersection"
    (are [a b expected] (and (= expected (intersection a b))
                             (= expected (intersection b a)))
      [0 1] [1 2] nil
      [0 1] [2 3] nil))
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
      [3 6] [4 7] [4 6])))

(deftest expand-test
  (doseq [offset (range -2 2)]
  (are [min max expected] (= (map #(+ offset %) expected) (expand [(+ min offset) (+ max offset)]))
    0 0 []
    0 1 [0]
    0 2 [0 1]
    0 3 [0 1 2])))

(deftest slice-test
  (let [intervals (->> (combo/selections (range 4) 2) (filter #(apply < %)))]
    (doseq [a intervals, b intervals] 
      (let [i (intersection a b)
            slices (slice a b)]
        ;; 1st range is the intersection
        (is (= i (first slices)))
        ;; At most 2 ranges after intersection
        (is (<= (count slices) 3))
        ;; ALl slices are disjoint
        (is (every? (fn [[a b]] (nil? (intersection a b))) (combo/combinations (remove nil? slices) 2)))
        ;; The union of all slices = a
        (is (= (expand a) (sort (mapcat expand (remove nil? slices)))))))))
