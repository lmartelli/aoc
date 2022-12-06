(ns aoc-2016.day13
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (-> (puzzle-input-string stream) parse-int))

;; part 1

(defn count-bits [n]
  (loop [n n
         res 0]
    (if (zero? n)
      res
      (recur (bit-shift-right n 1) (+ res (bit-and n 1))))))

(defn is-open-space? [[x y] favorite-num]
  (->> (+ (* x x) (* 3 x) (* 2 x y) y (* y y))
       (+ favorite-num)
       count-bits
       even?))

(defn neighbours [[x y] favorite-num]
  (->> (list [x (inc y)] [x (dec y)] [(inc x) y] [(dec x) y])))

(defn pos-or-zero? [n] (>= n 0))

(defn allowed-neighbour? [pos explored favorite-num]
  (and (not (explored pos))
       (every? pos-or-zero? pos)
       (is-open-space? pos favorite-num)))

(defn explore [from stop? favorite-num]
  (loop [explored #{from}
         last-explored #{from}
         length 0]
    (if (stop? length last-explored)
      {:length length :explored explored}
      (let [next-explored (->> last-explored
                               (mapcat #(neighbours % favorite-num))
                               (filter #(allowed-neighbour? % explored favorite-num))
                               (into #{}))]
        (recur
         (into explored next-explored)
         next-explored
         (inc length))))))

(def start [1 1])

(defn shortest-path-length [from to favorite-num]
  (-> (explore from (fn [steps explored] (explored to)) favorite-num)
      :length))

(defpart part1 [input]
  (shortest-path-length start [31 39] input))

;; part 2

(defn count-explored [from max-steps favorite-num]
  (-> (explore from (fn [steps explored] (= steps max-steps)) favorite-num)
      :explored
      count))

(defpart part2 [input]
  (count-explored start 50 input))

;; tests

(deftest count-bits-test
  (are [n expected] (= expected (count-bits n))
    0 0
    1 1
    2 1
    3 2
    4 1
    8 1
    9 2
    10 2
    11 3
    12 2
    13 3
    14 3
    15 4
    16 1
    17 2
    18 2
    19 3
    32 1
    33 2))
  
(deftest part1-test
  (is (= 11 (shortest-path-length [1 1] [7 4] 10))))
