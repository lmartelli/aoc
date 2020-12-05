(ns aoc-2020.day05
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn get-seat-id [boarding-pass]
  (->> boarding-pass
       (map {\B 1, \F 0, \L 0, \R 1})
       reverse
       (map * (iterate #(* 2 %) 1))
       (reduce +)))

(puzzle-input-parse-lines get-seat-id)

;; part 1


(defpart part1 [input]
  (apply max input))

;; part 2

(defpart part2 [input]
  (let [m (apply min input)
        M (apply max input)
        seat-ids (set input)]
    (find-first #(and (not (seat-ids %))
                      (seat-ids (inc %))
                      (seat-ids (dec %)))
            (range m (inc M)))))

;; test

(deftest boarding-pass-test
  (are [pass-number seat-id] (= seat-id (get-seat-id pass-number))
    "BFFFBBFRRR" 567
    "FFFBBBFRRR" 119
    "BBFFBBFRLL" 820))
