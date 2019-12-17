(ns aoc.2019.day12
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

;; {:pos [x,y,...] :velocity [x,y,...]}

(defn gravity
  ([pos planets]
   (reduce
    add
    (map #(sub % pos) planets))))

(defn apply-gravity
  ([p planets] (update p :velocity add (gravity (p :pos) (map :pos planets))))
  ([planets] (for [p planets] (apply-gravity p planets))))

;; tests

(deftest gravity-test
  (are [pos planets expected] (= expected (gravity pos planets))
    [1 2 3] [[1 2 3]] [0 0 0]
    [1 2 3] [[1 2 3] [9 8 7]] [8 6 4]))

(deftest apply-gravity-1-planet
  (is (= {:pos [1 2 3] :velocity [10 11 12]}
         (apply-gravity {:pos [1 2 3] :velocity [10 11 12]}
                        [{:pos [1 2 3]}]))))

(deftest apply-gravity-all-planets
  (is (= [{:pos [1 2 3] :velocity [20 18 16]}]
         (apply-gravity [{:pos [1 2 3] :velocity [20 18 16]}])))
  (is (= [{:pos [1 2 3] :velocity [20 18 16]}
          {:pos [9 8 7] :velocity [-2 -1 0]}
          {:pos [3 3 3] :velocity [4 4 4]}]
         (apply-gravity 
          [{:pos [1 2 3] :velocity [10 11 12]}
           {:pos [9 8 7] :velocity [0 0 0]}
           {:pos [3 3 3] :velocity [3 4 5]}]))))
