(ns aoc-2015.day23
  (:require
   [aoc.core :refer :all]
   [aoc.cpu :refer :all]
   [clojure.test :refer :all]))

;; part 1

(def instructions
  {:hlf (instr [r] (update-reg r #(quot % 2)))
   :tpl (instr [r] (update-reg r #(* % 3)))
   :inc (instr [r] (update-reg r inc))
   :jmp (instr [offset] offset)
   :jie (instr [tst offset] (jump-if offset even? tst))
   :jio (instr [tst offset] (jump-if offset #(= 1 %) tst))})

(defpart part1 [prog]
  (-> (run-prog {} prog instructions)
      :b))

;; part 2

(defpart part2 [prog]
  (-> (run-prog {:a 1} prog instructions)
      :b))

;; tests

(deftest basic-prog-test (part-test #(run-prog % {:ip 0, :a 0 }) {:a 2 :ip 4}))
