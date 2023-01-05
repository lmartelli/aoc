(ns aoc-2017.day23
  (:require
   [aoc.core :refer :all]
   [aoc-2017.instr :refer :all]))

;; Use aoc-2017.instr/puzzle-input

;; part 1

(defpart part1 [prog]
  (let [mul-count (atom 0)]
    (run-prog
      prog
      (update basic-instr-set :mul
              #(fn [& args]
                 (swap! mul-count inc)
                 (apply % args))))
    @mul-count))

;; part 2

(defpart part2 [prog]
  (:a
   (run-prog-until
     {:ip 0 :a 1}
     prog
     basic-instr-set
     terminated?)))

;; tests
