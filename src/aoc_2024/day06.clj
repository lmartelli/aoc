(ns aoc-2024.day06
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (let [rows (puzzle-input-lines stream)
        input (->> (s2/parse-2d-map-positions rows \# \^)
                   (map-keys {\^ :start \# :obstacles}))]
    {:pos (first (input :start))
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

(defn guard-iterations [{:keys [obstacles pos dir]}]
  (iterate #(step % obstacles) {:pos pos :dir dir}))

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

(defn loops? [input visited]
  (loop [visited visited
         [current & more] (guard-iterations input)]
    (cond
      (visited current) true
      (not (in-area? input (current :pos))) false
      :else (recur (conj visited current)
                   more))))

(defpart part2 [input]
  (count
    (loop [visited #{}
           visited-pos #{}
           current {:pos (input :pos) :dir (input :dir)}
           new-obstacles #{}]
      (if (not (in-area? input (current :pos)))
        new-obstacles
        (let [next (step current (input :obstacles))]
          (recur (conj visited current)
                 (conj visited-pos (current :pos))
                 next
                 (if (and (not ((input :obstacles) (next :pos)))
                          (not (visited-pos (next :pos)))
                          (loops? (-> input
                                      (update :obstacles conj (next :pos))
                                      (merge current))
                                  visited))
                   (conj new-obstacles (next :pos))
                   new-obstacles)))))))

;; tests

(deftest part1-test (part-test part1 41))

;;(deftest part1-test (test-with-lines part1 [""] nil))

(deftest part2-test (part-test part2 6))

;;(deftest part2-test (test-with-lines part2 [""] nil))
