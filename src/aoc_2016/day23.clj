(ns aoc-2016.day23
  (:require
   [aoc.core :refer :all]
   [aoc-2016.assembunny :refer :all :exclude [run-prog]]
   [clojure.test :refer :all]))

;; Use aoc-2016.assembunny/puzzle-input

;; part 1

(defn update-operation [instr new-op]
  (assoc instr 0 new-op))

(defn toggle-instr [[operation & operands :as instr]]
  (case (count operands)
    1 (update-operation instr (if (= operation :inc) :dec :inc))
    2 (update-operation instr (if (= operation :jnz) :cpy :jnz))))

(defn run-prog [prog registers]
  (loop [tick 0
         prog (transient prog)
         registers (transient registers)]
    (if (or (not (<= 0 (registers :ip) (dec (count prog)))))
      (persistent! registers)
      (let [ip (registers :ip)
            [op arg :as instr] (prog ip)]
        (if (= :tgl op)
          (let [toggle-target (+ ip (eval-expr registers arg))]
              (recur
                (inc tick)
                (if (contains? prog toggle-target)
                  (update! prog (+ ip (eval-expr registers arg)) toggle-instr)
                  prog)
                (update! registers :ip inc)))
          (recur
            (inc tick)
            prog
            (exec-instr registers instr instruction-set)))))))

(defpart part1 [prog]
  (-> (run-prog prog (assoc (init-registers) :a 7))
      :a))

;; part 2

(defpart part2 [prog]
    (-> (run-prog prog (assoc (init-registers) :a 12))
      :registers
      :a))

;; tests

(deftest part1-test (part-test part1 3))
