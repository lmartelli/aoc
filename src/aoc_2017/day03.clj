(ns aoc-2017.day03
  (:require [aoc.core :refer :all]
            [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (-> (line-seq stream)
      first
      parse-int))

;; part 1

(defn find-first [f c]
  (first (filter f c)))

(defn inc-last [m pos dir]
  (-> (add pos (sub dir))
      m
      inc))

(defn get-next-dir [m pos dir]
  (let [rotated (rotate-left dir)]
    (if (m (add pos rotated))
      dir
      rotated)))

(defn spiral-seq [n0 compute-next]
  (->> (reductions
        (fn [{:keys [:m :pos :dir]} _]
          (let [n (m pos)]
            (let [next-pos (add pos dir)]
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
