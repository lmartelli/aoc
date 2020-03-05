(ns aoc-2017.day17
  (:require
   [aoc.core :refer :all]))

(puzzle-input-string parse-int)

;; part 1

(defn next-step [[buffer pos] nb-steps]
  (let [buffer-size (count buffer)
        insert-pos (inc (mod (+ pos nb-steps) buffer-size))]
    [(insert-at buffer insert-pos buffer-size)
     insert-pos]))

(defn steps [nb-steps]
  (iterate #(next-step % nb-steps) [[0] 0]))

(defpart part1 [input]
  (let [[values pos] (nth (steps input) 2017)]
    (values (inc pos))))

;; part 2

(defpart part2 [nb-steps]
  (loop [buffer-size 1
         pos 0
         value-after-zero nil]
    (if (= buffer-size 50000001)
      value-after-zero
      (let [insert-pos (inc (mod (+ pos nb-steps) buffer-size))]
        (recur (inc buffer-size)
               insert-pos
               (if (= 1 insert-pos) buffer-size value-after-zero))))))

;; tests
