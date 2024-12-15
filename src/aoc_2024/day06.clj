(ns aoc-2024.day06
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (let [rows (puzzle-input-lines stream)
        input (->> (s2/parse-2d-map-positions rows \# \^)
                   (map-keys {\^ :start \# :obstacles}))]
    {:start (first (input :start))
     :obstacles (set (input :obstacles))
     :width (count (first rows))
     :height (count rows)
     :dir (s2/direction-vectors :up)}))

;; part 1

(defn step [state obstacles]
  (let [next-pos (s2/+ (state :pos) (state :dir))]
    (if (obstacles next-pos)
      (update state :dir s2/rotate-right)
      (assoc state :pos next-pos))))

(defn in-area? [{:keys [width height]} [x y]]
  (and (< -1 x width)
       (< -1 y height)))

(defn guard-iterations [{:keys [obstacles start dir]}]
  (iterate #(step % obstacles) {:pos start :dir dir}))

(defn guard-path [input]
  (->> (guard-iterations input)
       (map :pos)
       dedupe))

(defpart part1 [input]
  (->> (guard-path input)
       (take-while #(in-area? input %))
       set
       count))

;; part 2

(defn loops? [input]
  (loop [visited #{}
         [current & more] (guard-iterations input)]
    (cond
      (visited current) true
      (not (in-area? input (current :pos))) false
      :else (recur (conj visited current)
                   more))))

(defpart part2 [input]
  (let [path (set (->> (guard-path input)
                       (take-while #(in-area? input %))))]
    (->> path
         (filter #(loops? (update input :obstacles conj %)))
         count)))

;; tests

(deftest part1-test (part-test part1 41))

;;(deftest part1-test (test-with-lines part1 [""] nil))

(deftest part2-test (part-test part2 6))

;;(deftest part2-test (test-with-lines part2 [""] nil))
