(ns aoc.vector
  (:require
   [clojure.test :refer :all]))

(defn rotate-left [v n]
  (let [n (mod n (count v))]
    (into (subvec v n) (subvec v 0 n))))

(defn rotate-right [v n]
  (rotate-left v (- n)))

(defn swap-indices [v i1 i2]
  (assoc v i1 (v i2) i2 (v i1)))

;; Tests

(deftest rotate-left-test
  (let [v [0 1 2 3]]
    (testing "n >= 0 : rotate left"
      (are [n expected] (and (= expected (rotate-left v n))
                             (= expected (rotate-left v (+ n (count v)))))
        0 [0 1 2 3]
        1 [1 2 3 0]
        2 [2 3 0 1]
        3 [3 0 1 2]))
    (testing "n < 0 : rotate right"
      (are [n expected] (and (= expected (rotate-left v n))
                             (= expected (rotate-left v (- n (count v)))))
        -1 [3 0 1 2]
        -2 [2 3 0 1]
        -3 [1 2 3 0]))))

(deftest swap-indices-test
  (let [v [0 1 2 3]]
    (are [i1 i2 expected] (= expected (swap-indices v i1 i2))
      0 0 v
      0 1 [1 0 2 3]
      0 3 [3 1 2 0]
      1 2 [0 2 1 3])))
