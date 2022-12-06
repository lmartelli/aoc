(ns aoc-2015.day23
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.test :refer :all]))

(defn keyword-or-int [s]
  (if (letter? (first s))
    (keyword s)
    (parse-int s)))

(defn puzzle-input [stream]
  (->> stream
       line-seq
       (mapv (fn [line]
               (->> (split line #"[, ]+")
                    (mapv keyword-or-int))))))

;; part 1

(defn update-register [f]
  (fn [registers r]
    (-> registers
        (update r f)
        (update :ip inc))))

(defn jump-if [pred]
  (fn [registers r offset]
    (update registers :ip
            #(+ % (if (pred (registers r)) offset 1)))))

(defn jump [registers offset]
  (update registers :ip + offset))

(def instructions
  {:hlf (update-register #(quot % 2))
   :tpl (update-register #(* % 3))
   :inc (update-register inc)
   :jmp jump
   :jie (jump-if even?)
   :jio (jump-if #(= 1 %))})

(defn exec-instr [registers [instr & operands]]
  (apply (instructions instr) registers operands))

(def init-registers (constantly {:a 0 :b 0 :ip 0}))

(defn run-prog [prog registers]
  (->> (iterate
        (fn [registers]
          (exec-instr registers (get prog (registers :ip))))
        registers)
       (find-first #(not (<= 0 (% :ip) (dec (count prog)))))))

(defpart part1 [input]
  (-> (run-prog input (init-registers))
      :b))

;; part 2

(defpart part2 [input]
  (-> (run-prog input (-> (init-registers) (assoc :a 1)))
        :b))

;; tests

(deftest run-prog-test
  (is (= {:a 2 :b 0 :ip 4} (run-prog (puzzle-input (test-input))))))
