(ns aoc-2018.day11
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (-> (puzzle-input-string stream)
      parse-int))

;; part 1

(def grid-size 300)

(defn hundreds-digit [n]
  (-> n
      (mod 1000)
      (quot 100)))

(defn power-level [x y grid-serial-number]
  (let [rack-id (+ 10 x)]
    (-> (* rack-id y)
        (+ grid-serial-number)
        (* rack-id)
        hundreds-digit
        (- 5))))

(defn sliding-sum [coll n]
  (reductions
    (fn [sum [a b]]
      (-> sum
          (+ a)
          (- b)))
    (reduce + (take n coll))
    (map vector (drop n coll) coll)))

(defn max-indexed-key [key coll]
  (loop [max-value (first coll)
         max-key (key max-value)
         max-index 0
         index 1
         [v & more] (rest coll)]
    (if (nil? v)
      [max-value max-index]
      (let [current-key (key v)]
        (if (> current-key max-key)
          (recur v current-key index (inc index) more)
          (recur max-value max-key max-index (inc index) more))))))

(defn max-indexed [coll]
  (max-indexed-key identity coll))

(defn max-power [grid-serial-number square-size]
  (->> (for [x (range 1 (inc grid-size))
             y (range 1 (inc grid-size))]
         (power-level x y grid-serial-number))
       (partition grid-size)
       (map #(sliding-sum % square-size))
       transpose
       (map #(sliding-sum % square-size))
       (apply concat)
       (max-indexed)))

(defn index-to-coord [index size]
  (->> ((juxt #(mod % size) #(quot % size)) index)
       (map inc)))

(defpart part1 [grid-serial-number]
  (let [[power index] (max-power grid-serial-number 3)]
    (as-> index $
         (index-to-coord $ (- grid-size 2))
         (str/join "," $))))

;; part 2

(defpart part2 [grid-serial-number]
  (let [[[power index] size] (->> (map #(max-power grid-serial-number (inc %)) (range grid-size))
                                  (max-indexed-key first))
        [x y] (index-to-coord index (- grid-size size))]
    (str/join "," [x y (inc size)])))

;; tests

(deftest hundreds-digit-test
  (are [n expected] (= expected (hundreds-digit n))
    99 0
    100 1
    234 2
    1234 2))

(deftest power-level-test
  (are [x y grid-serial-number expected] (= expected (power-level x y grid-serial-number))
    3 5 8 4
    122 79 57 -5
    217 196 39 0
    101 153 71 4))

(deftest part1-test
  (are [grid-serial-number expected] (= expected (part1 grid-serial-number))
    18 "33,45"
    42 "21,61"))

(deftest part2-test
  (are [grid-serial-number expected] (= expected (part2 grid-serial-number))
    18 "90,269,16"
    42 "232,251,12"))
