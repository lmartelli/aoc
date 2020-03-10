(ns aoc-2017.day24
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(puzzle-input-split-lines #"/" #(mapv parse-int %) set)

;; part 1

(defn matching-components [components n]
  (filter #(contains? (set %) n) components))

(defn other-port [component n]
  (if (= n (first component))
    (second component)
    (first component)))

(defn bridges
  ([components]
   (bridges components 0 []))
  ([components n prefix]
   (let [next-components (matching-components components n)]
     (if (empty? next-components)
       [prefix]
       (mapcat
         (fn [next]
           (bridges
             (disj components next)
             (other-port next n)
             (conj prefix next)))
         next-components)
       ))))

(defn strength [bridges]
  (apply + (apply concat bridges)))

(defn strongest-bridge [components]
  (apply
    max-key
    strength
    (bridges components)))

(defpart part1 [components]
  (strength (strongest-bridge components)))

;; part 2

(defn strongest-and-longest-bridge [components]
  (loop [max-strength, 0 max-length 0, remaining (bridges components)]
    (if (empty? remaining)
      max-strength
      (let [bridge (first remaining)
            bridge-length (count bridge)
            more (rest remaining)]
        (cond
          (> bridge-length max-length)
          (recur (strength bridge) bridge-length more)
          (= bridge-length max-length)
          (recur (max (strength bridge) max-strength) bridge-length more)
          :else
          (recur max-strength max-length more))))))

(defpart part2 [components]
    (strongest-and-longest-bridge components))

;; tests

(deftest strength-test
  (are [bridges expected] (= expected (strength bridges))
    [] 0
    [[0 0]] 0
    [[0 3]] 3 
    [[4 3]] 7
    [[0 1] [4 6]] 11))

(deftest matching-components-test
  (let [components [[3 7] [0 1] [7 9] [11 3]]]
    (are [n expected] (= expected (matching-components components n))
      0 [[0 1]]
      1 [[0 1]]
      2 []
      3 [[3 7] [11 3]]
      7 [[3 7] [7 9]]
      9 [[7 9]]
      11 [[11 3]])))

(deftest other-port-test
  (are [component n expected] (= expected (other-port component n))
    [2 5] 2 5
    [2 5] 5 2
    [3 3] 3 3))

(def test-input #{[0 2] [2 2] [2 3] [3 4] [3 5] [0 1] [10 1] [9 10]})

(deftest bridges-test
  (are [components expected] (= (set expected) (set (bridges components)))
    #{[0 1]} [[[0 1]]]
    #{[3 4] [0 1]} [[[0 1]]]
    #{[1 4] [0 1]} [[[0 1] [1 4]]]
    test-input
    '(([0 1] [10 1] [9 10])
      ([0 2] [2 3] [3 4])
      ([0 2] [2 3] [3 5])
      ([0 2] [2 2] [2 3] [3 4])
      ([0 2] [2 2] [2 3] [3 5]))))

(deftest strongest-bridge-test
  (is (= 31 (strongest-bridge test-input))))
