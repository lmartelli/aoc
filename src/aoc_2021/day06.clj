(ns aoc-2021.day06
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn init-population [input]
  (let [p (frequencies input)]
       (mapv #(get p %1 0) (range 9))))

(defn puzzle-input [stream]
  (-> (puzzle-input-int-array stream)
      init-population))

;; part 1

(defn pass-1-day [population]
  (let [p0 (get population 0)]
    (-> population
        (subvec 1)
        (conj p0)
        (update 6 +' p0))))

(defn pass-days [n population]
  (-> (iterate pass-1-day population)
      (nth n)))

(defn count-population-after-days [days population]
  (->> (pass-days days population)
       (apply +)))

(defpart part1 [input]
  (count-population-after-days 80 input))

;; part 2

(defpart part2 [input]
  (count-population-after-days 256 input))

;; tests

(def test-data (init-population [3 4 3 1 2]))

(deftest part1-test
  (is (= 5934 (part1 test-data))))

(deftest part2-test
  (is (= 26984457539 (part2 test-data))))
