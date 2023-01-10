(ns aoc-2017.day03
  (:require [aoc.core :refer :all]
            [aoc.space-2d :as s2]
            [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (-> (line-seq stream)
      first
      parse-int))

;; part 1

(defn inc-last [m pos dir]
  (-> (s2/+ pos (s2/- dir))
      m
      inc))

(defn get-next-dir [m pos dir]
  (let [rotated (s2/rotate-left dir)]
    (if (m (s2/+ pos rotated))
      dir
      rotated)))

(defn spiral-seq [n0 compute-next]
  (->> (reductions
        (fn [{:keys [:m :pos :dir]} _]
          (let [n (m pos)]
            (let [next-pos (s2/+ pos dir)]
              {:m (assoc m next-pos (compute-next m next-pos dir))
               :pos next-pos
               :dir (get-next-dir m next-pos dir)})))
        {:m {[0 0] n0}, :pos [0 0], :dir [1 0]}
        (range))
       (map (fn [{:keys [:m :pos :dir]}]
              {:pos pos, :val (m pos)}))))

(defpart part1 [input]
  (->> (spiral-seq 1 inc-last)
       (find-first #(= (% :val) input))
       :pos
       manatthan-dist))

;; part 2

(defn sum-neighbours [m pos dir]
  (->> [[1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]]
       (map (partial add pos))
       (map #(get m % 0))
       (reduce +)))

(defpart part2 [input]
  (->> (spiral-seq 1 sum-neighbours)
       (find-first #(> (% :val) input))
       :val))

;; tests

(deftest part1-test
  (are [input expected] (= expected (part1 input))
    1 0
    12 3
    23 2
    1024 31))

(deftest part2-test
  (are [input expected] (= expected (part2 input))
    7 10
    10 11
    20 23
    26 54
    100 122
    147 304
    747 806))
