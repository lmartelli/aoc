(ns aoc-2023.day06
  (:require
   [aoc.core :refer :all]
   [clojure.math :refer [floor ceil sqrt]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (puzzle-input-lines stream)
       (take 2)
       (map parse-ints)
       (apply map #(zipmap [:time :distance] %&))))

;; part 1

(defn solve-quadratic [a b c]
  (let [delta (- (* b b) (* 4 a c))]
    (map #(-> (* % (sqrt delta))
              (- b)
              (/ (* 2 a)))
         [1 -1])))

(defn ceil* [x]
  (let [res (ceil x)]
    (if (= res x)
      (inc res)
      res)))

(defn floor* [x]
  (let [res (floor x)]
    (if (= res x)
      (dec res)
      res)))

(defn count-ways-to-beat-record [{:keys [time distance]}]
  (let [[m M] (->> (solve-quadratic 1 (- time) distance)
                       (apply min-max))]
    (-> (- (min (floor* M) time) (ceil* m))
        int
        inc)))

(defpart part1 [input]
  (->> input
       (map count-ways-to-beat-record)
       (reduce *)))

;; part 2

(defpart part2 [input]
  (->> input
       (apply merge-with #(parse-int (str %1 %2)))
       (count-ways-to-beat-record)))

;; tests

(deftest part1-test (part-test part1 288))

;;(deftest part1-test (test-with-lines part1 [""] nil))

(deftest count-ways-to-beat-record-test
  (are [time distance expected] (= expected (count-ways-to-beat-record {:time time :distance distance }))
    7 9 4
    15 40 8
    30 200 9))

(deftest part2-test (part-test part2 71503))

;;(deftest part2-test (test-with-lines part2 [""] nil))
