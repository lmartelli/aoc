(ns aoc-2019.day05
  (:require
   [clojure.java.io :as io]
   [clojure.string :refer [split split-lines]]))

(defn parse-string [str]
  (vec
   (map
    #(Long/parseLong %)
    (split str #","))))

(defn parse-input [resource]
  (vec
   (map
    #(Long/parseLong %)
    (mapcat
     #(split % #",")
     (split-lines
      (slurp (io/resource resource)))))))

(def puzzle-input (parse-input "2019-05.txt"))

(defn get-digit [n pos]
  (mod (reduce quot n (repeat (dec pos) 10)) 10))

(defn param-mode [instr n]
  (get-digit instr (+ 2 n)))

(defn op-code [instr]
  (mod instr 100))

(def debug? false)

(defn set-debug [val]
  (def debug? val))

(defn debug [& args]
  (if debug?
    (apply println args)))

(defn run-instr [ip memory in out]
  (let [instr (memory ip)
        op-code (op-code instr)
        imm (fn [n] (memory (+ ip n)))
        arg (fn [n]
              (let [immediate (imm n)]
                (case (param-mode instr n)
                  1 immediate
                  0 (memory immediate))))]
    (debug "instr:" instr "op-code:" op-code)
    (case op-code
      ;; + *
      (1 2) (let [op ({1 +' 2 *'} op-code)]
              (debug op (arg 1) (arg 2) "→" (imm 3))
              [(+ ip 4)
               (assoc memory
                      (imm 3)
                      (op
                       (arg 1)
                       (arg 2)))
               in
               out])
      ;; input → [1]
      3 (let [input-value (first in)]
          (debug "input" input-value "→" (imm 1))
          [(+ ip 2)
           (assoc memory (imm 1) (first in))
           (rest in)
           out])
      ;; output ← [1]
      4 [(+ ip 2)
         memory
         in
         (conj out (arg 1))]
      ;; jump-if-true
      5 (do
          (debug "jump-if-true" (arg 1))
          (if (not= 0 (arg 1))
            [(arg 2) memory in out]
            [(+ ip 3) memory in out]))
      ;; jump-if-false
      6 (do
          (debug "jump-if-false" (arg 1))
          (if (= 0 (arg 1))
            [(arg 2) memory in out]
            [(+ ip 3) memory in out]))
      ;; less than
      7 (do
          (debug "less than" (arg 1) "<" (arg 2) "→" (imm 3))
          [(+ ip 4)
           (assoc memory (imm 3) (if (< (arg 1) (arg 2)) 1 0))
           in
           out])
      ;; equals
      8 (do
          (debug "equals" (arg 1) "=" (arg 2) "→" (imm 3))
          [(+ ip 4)
           (assoc memory (imm 3) (if (= (arg 1) (arg 2)) 1 0))
           in
           out])
      )
    ))

(defn run [memory in]
  (loop [ip 0
         memory memory
         in in
         out []]
    (if (= (op-code (memory ip)) 99)
      out
      (let [[ip memory in out] (run-instr ip memory in out)]
        (debug "new ip:" ip)
        (recur ip memory in out)))))

(defn part2 [input]
  (run input [5]))
