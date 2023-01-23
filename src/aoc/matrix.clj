(ns aoc.matrix
  (:require
   [aoc.core :refer [split-seq eq]]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn transpose [m]
  (apply mapv vector m))

(defn flip-cols
  "Flip columns, that is, invert left and right"
  [m]
  (mapv (comp vec reverse) m))

(defn flip-rows
  "Flip rows, that is, invert up and down"
  [m]
  (-> m reverse vec))

(defn rotate-left [m]
  (-> m transpose flip-rows))

(defn rotate-right [m]
  (-> m transpose flip-cols))

(defn split-cols [m]
  (->> m
       transpose
       (split-seq #(every? (eq \space) %))
       (map transpose)))

(defn trim
  "Removes 1 row and 1 column on all sides."
  [m]
  (->> m
       (drop 1)
       (drop-last 1)
       (mapv (comp vec #(drop 1 %) #(drop-last 1 %)))))

(defn cat-cols [& ms]
  (apply mapv (comp vec concat) ms))

(defn cat-rows [& ms]
  (vec (apply concat ms)))

;; Tests

(deftest cat
  (testing "Concat columns"
    (are [m1 m2 expected] (= expected (cat-cols m1 m2))
      [[1 2]
       [3 4]]
      [[5 6]
       [7 8]]
      [[1 2 5 6]
       [3 4 7 8]]))
  (testing "Concat rows"
        (are [m1 m2 expected] (= expected (cat-rows m1 m2))
      [[1 2]
       [3 4]]
      [[5 6]
       [7 8]]
      [[1 2]
       [3 4]
       [5 6]
       [7 8]])))

(deftest trim-test
  (are [m expected] (= expected (trim m))
    ["123"
     "456"
     "789"]
    [[\5]]
    ["123A"
     "456B"
     "789C"]
    [[\5 \6]]
    ["123A"
     "456B"
     "789C"
     "GFED"]
    [[\5 \6]
     [\8 \9]]
    ))

(deftest split-cols-test
  (are [rows expected] (= expected (split-cols rows))
    ["12 ab"
     "34 cd"]
    [[[\1 \2]
      [\3 \4]]
     [[\a \b]
      [\c \d]]]))

(deftest transpose-test
  (are [colls transposed] (= transposed (transpose colls))
    [[1 2]] [[1] [2]]
    [[1]
     [2]] [[1 2]]
    [[1 2]
     [3 4]] [[1 3]
             [2 4]]))

(deftest flip-cols-test
  (are [m expected] (= expected (flip-cols m))
    [[1 2]] [[2 1]]
    [[1 2 3]] [[3 2 1]]
    [[1 2 3]
     [4 5 6]] [[3 2 1]
               [6 5 4]]))

(deftest flip-rows-test
  (are [m expected] (= expected (flip-rows m))
    [[1 2]] [[1 2]]
    [[1 2 3]] [[1 2 3]]
    [[1 2 3]
     [4 5 6]] [[4 5 6]
               [1 2 3]]
    [[1 2 3]
     [4 5 6]
     [7 8 9]] [[7 8 9]
               [4 5 6]
               [1 2 3]]))

(deftest rotate-left-test
  (are [m expected] (= expected (rotate-left m))
    [[1]] [[1]]
    [[1 2]] [[2]
             [1]]
    [[1 2]
     [3 4]] [[2 4]
             [1 3]]
    [[1 2 3]
     [4 5 6]] [[3 6]
               [2 5]
               [1 4]]
    [[1 2 3]
     [4 5 6]
     [7 8 9]] [[3 6 9]
               [2 5 8]
               [1 4 7]]))

(deftest rotate-right-test
  (are [m] (= m (-> m rotate-left rotate-right))
    [[1]]
    
    [[1 2]]
    
    [[2]
     [1]]
    
    [[1 2]
     [3 4]]
    
    [[1 2 3]
     [4 5 6]]
    
    [[3 6]
     [2 5]
     [1 4]]
    
    [[1 2 3]
     [4 5 6]
     [7 8 9]]))
