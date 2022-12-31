(ns aoc-2018.day10
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [aoc.ocr :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (let [data (map parse-points (line-seq stream))]
    {:positions (map first data)
     :velocities (map second data)}))

;; part 1

(defn print-points [positions]
  (->> positions
       (s2/draw-points \#)
       s2/print))

(defn min-rect-area [points]
  (let [[[x-min x-max] [y-min y-max]] (s2/x-and-y-ranges points)]
  (* (- x-max x-min) (- y-max y-min))))

(defn step [positions velocities]
  (map s2/+ positions velocities))

(defn local-min-key [f coll]
  (loop [index 0
         prev-val (first coll)
         prev-key (f prev-val)
         [val & more] (rest coll)]
    (let [key (f val)]
      (if (> key prev-key)
        [index prev-val]
        (recur (inc index) val key more)))))

(defn print-message [{:keys [positions velocities]}]
  (->> (iterate #(step % velocities) positions)
       (local-min-key min-rect-area)
       second
       (s2/draw-points \#)
       s2/print-to-lines))

(defpart part1 [input]
  (->> (print-message input)
       ocr-big))

;; part 2

(defpart part2 [{:keys [positions velocities]}]
  (->> (iterate #(step % velocities) positions)
       (local-min-key min-rect-area)
       first))

;; tests

(deftest print-message-test
  (is (= ["#   #  ###"
          "#   #   # "
          "#   #   # "
          "#####   # "
          "#   #   # "
          "#   #   # "
          "#   #   # "
          "#   #  ###"]
         (print-message (test-data)))))

(deftest part2-test (part-test part2 3))
