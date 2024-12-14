(ns aoc-2024.day03
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-string stream))

;; part 1

(defpart part1 [input]
  (->> (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" input)
       (map (fn [[_ a b]] [(parse-int a) (parse-int b)]))
       (map #(apply * %))
       (reduce +)))

;; part 2

(defn make-instr-pattern [[name nb-args]]
  (str "(" name ")\\(" (str/join "," (repeat nb-args "(\\d{1,3})")) "\\)"))

(def instr-pattern
  (->> ["do" 0
        "don't" 0
        "mul" 2]
       (partition 2)
       (map make-instr-pattern)
       (str/join "|")
       re-pattern))

(defn instr-seq [input]
  (->> (re-seq instr-pattern input)
       (map (fn [[_ & matches]]
              (let [[instr & args] (filter some? matches)]
                [instr (map parse-int args)])))))

(def instruction-set
  {"mul" (fn [state a b]
           (if (state :enabled)
             (update state :acc + (* a b))
             state))
   "do" (fn [state] (assoc state :enabled true))
   "don't" (fn [state] (assoc state :enabled false))})

(defn exec-instr [state [instr args]]
  (apply (instruction-set instr) state args))

(defpart part2 [input]
  (-> (reduce exec-instr {:enabled true :acc 0} (instr-seq input))
      :acc))

;; tests

(deftest part1-test
  (is (= 161 (part1 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"))))

(deftest part2-test
  (is (= 48 (part2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"))))
