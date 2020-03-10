(ns aoc-2017.day23
  (:require
   [aoc.core :refer :all]
   [aoc-2017.instr :refer :all]))

(puzzle-input-parse-lines parse-instr)

;; part 1

(def mul-count (atom 0))

(defn wrap-op [op-fn]
  )

(defpart part1 [prog]
  (reset! mul-count 0)
  (run-prog
    prog
    (update basic-instr-set :mul
            #(fn [& args]
               (swap! mul-count inc)
               (apply % args))))
  @mul-count)

;; part 2

(defpart part2 [prog]
  (:a
   (run-prog-until
     {:ip 0 :a 1}
     prog
     basic-instr-set
     terminated?)))

;; tests
