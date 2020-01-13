(ns aoc-2019.day02
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(puzzle-input-int-array)

;; part 1

(defn run-op [opcodes ip]
  (let [target (get opcodes (+ ip 3))
        op1 (get opcodes (get opcodes (+ ip 1)))
        op2 (get opcodes (get opcodes (+ ip 2)))]
    (case (get opcodes ip)
      1 (assoc opcodes target (+ op1 op2))
      2 (assoc opcodes target (* op1 op2)))))

(defn run-op [memory ip]
  (let [operator (case (get memory ip)
                   1 +
                   2 *)
        operand-value (fn [a] (get memory (get memory (+ ip a))))]
    (assoc memory
           (get memory (+ ip 3))
           (operator
            (operand-value 1)
            (operand-value 2)))))

(defn run [input noun verb]
  (loop [opcodes (assoc input
                        1 noun
                        2 verb)
         ip 0]
    (if (= (get opcodes ip) 99)
      (get opcodes 0)
      (recur (run-op opcodes ip) (+ ip 4)))
  ))

(defpart part1 [input]
  (run input 12 2))

;; part 2

(defn cartesian-poduct [coll]
    (for [x coll
          y coll]
      [x y]))

(defn find-noun-and-verb [input expected]
  (loop [nouns-and-verbs (cartesian-poduct (range 0 100))]
    (let [[noun verb] (first nouns-and-verbs)
          result (run input noun verb)]
      (if (= expected result)
        (+ (* 100 noun) verb)
        (recur (rest nouns-and-verbs))))))

(defpart part2 [input]
  (find-noun-and-verb input 19690720))
