(ns aoc-2020.day08
  (:require
   [aoc.core :refer :all]
   [aoc.cpu :refer :all]
   [aoc.algo :refer :all]
   [clojure.test :refer :all]))

;; Use aoc.cpu/puzzle-input

;; part 1

(def ops
  {:nop (instr [_] (nop))
   :acc (instr [arg] (update-reg :acc + arg))
   :jmp (instr [offset] (jump offset))})

(def init-state {:ip 0, :acc 0})

(defn instr-op [instr] (first instr))

(defn find-loop [prog]
  (->> (trace-registers init-state prog ops)
       (find-cycle-key :ip)))

(defn exec-until-loop [state prog]
  (->> (trace-registers state prog ops)
       (find-cycle-key :ip)
       :repeat-value))

(defpart part1 [prog]
  (->> (find-loop prog)
       :repeat-value
       :acc))

;; part 2

(defn exec-until-loop-or-termination [state prog]
  (->> (trace-registers state prog ops)
       (reduce
         (fn [ip-history {ip :ip :as state}]
           (cond
             (ip-history ip) (reduced nil)
             (terminated? state prog) (reduced state)
             :else (conj ip-history ip)))
         #{})))

(defn switch-jmp-or-nop [prog pos]
  (update-in prog [pos 0] {:nop :jmp, :jmp :nop}))

(defn fix-infinite-loop [prog]
  (loop [state init-state]
    (let [ip (state :ip)
          instr (prog ip)
          op (instr-op instr)]
      (if (= op :acc)
        (recur (step-instr state prog ops))
        (if-let [terminal-state (exec-until-loop-or-termination state (switch-jmp-or-nop prog ip))]
          terminal-state
          (recur (step-instr state prog ops)))))))

(defpart part2 [prog]
  (-> (fix-infinite-loop prog)
      :acc))

;; test

(deftest part1-test (part-test part1 5))

(deftest part2-test (part-test part2 8))
