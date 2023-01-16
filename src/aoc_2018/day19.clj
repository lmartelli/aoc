(ns aoc-2018.day19
  (:require
   [aoc.core :refer :all :exclude [eq]]
   [aoc.cpu :as cpu]
   [aoc-2018.opcodes :refer :all]
   [clojure.test :refer :all]))

(def-input-parser [[ip-decl & prog]]
  {:ip-register (first (parse-ints ip-decl))
   :prog (cpu/parse-prog prog)})

(defn set-register-from-ip [registers ip-register]
  (assoc registers ip-register (registers :ip)))

(defn set-ip-from-register [registers ip-register]
  (assoc registers :ip (registers ip-register)))

(defn step-instr [ip-register]
  (fn [registers prog instr-set]
    (-> registers
        (set-register-from-ip ip-register)
        (cpu/step-instr prog instr-set)
        (set-ip-from-register ip-register)
        (update :ip inc))))

(defn run-prog [{:keys [ip-register prog]} registers]
  (cpu/run-prog registers prog instruction-set :step-instr (step-instr ip-register)))

(defn trace [{:keys [ip-register prog]} registers]
  (cpu/trace-instr-and-registers registers prog instruction-set :step-instr (step-instr ip-register)))

;; part 1

(defpart part1 [input]
  (-> (run-prog input (init-registers 6))
      (get 0)))

;; part 2

(defpart part2 [input]
  (-> (run-prog input (-> (init-registers 6) (assoc 0 1)))
      (get 0)))

;; tests

(deftest run-prog-test
  (is (= (-> (vector-as-map [6 5 6 0 0 9]) (assoc :ip 7))
         (run-prog (test-data)))))
