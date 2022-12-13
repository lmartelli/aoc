(ns aoc-2022.day13
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (remove empty?)
       (map read-string)))

;; part 1

(defn compare-packets [a b]
  (if (= a b)
    0
    (cond
      (every? number? [a b]) (compare a b)
      (every? vector? [a b]) (or (->> (map compare-packets a b)
                                      (find-first (complement zero?)))
                                 (compare (count a) (count b)))
      :else (apply compare-packets (map #(if (number? %) [%] %) [a b])))))

(defn ordered? [[a b]]
  (neg? (compare-packets a b)))

(defpart part1 [packets]
  (->> (partition 2 packets)
       (map-indexed #(vector (inc %1) %2))
       (filter (comp ordered? second))
       (map first)
       (reduce +)))

;; part 2

(defpart part2 [packets]
  (let [dividers [[[2]] [[6]]]
        sorted-packets (sort compare-packets (concat packets dividers))]
    (->> (map #(inc (index-of sorted-packets %))  dividers)
         (reduce *))))

;; tests

(deftest compare-packets-test
  (testing "comparing 2 ints or 2 lists"
    (testing "a = b"
      (are [a b] (= 0 (compare-packets a b))
        1 1
        [] []
        [1] [1]
        [[1]] [[1]]
        [[1 2] 1 [3]] [[1 2] 1 [3]]))
    (testing "a < b && b > a"
      (are [a b] (and (= -1 (compare-packets a b))
                      (= 1 (compare-packets b a)))
        1 2
        [1] [2]
        [] [1]
        [1 2] [1 3]
        [1 2 4] [1 3 0]
        [1 2] [1 2 0] 
        [1 2 [3]] [1 2 [4]]
        [1 2 [3 1]] [1 2 [4]]
        )))
  (testing "comparing an int and a list"
    (are [a b expected] (and (= expected (compare-packets a b))
                             (= (- expected) (compare-packets b a)))
      1 [1] 0
      2 [1] 1)
    )
  )

(deftest ordered?-test
  (are [a b] (ordered? [a b])
    [1,1,3,1,1]
    [1,1,5,1,1]

    [[1],[2,3,4]]
    [[1],4]

    [[4,4],4,4]
    [[4,4],4,4,4]

    []
    [3])
  (are [a b] (not (ordered? [a b]))
    [9]
    [[8,7,6]]
    
    [7,7,7,7]
    [7,7,7]
    
    [[[]]]
    [[]]
    
    [1,[2,[3,[4,[5,6,7]]]],8,9]
    [1,[2,[3,[4,[5,6,0]]]],8,9]))

(deftest part1-test (part-test part1 13))

(deftest part2-test (part-test part2 140))
