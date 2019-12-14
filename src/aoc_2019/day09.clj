(ns aoc-2019.day09
  (:require
   [aoc-2019.day05 :refer [parse-string parse-input param-mode op-code debug set-debug]]
   [aoc-2019.day07 :refer [terminated? cur-instr] :exclude [run]]
   [clojure.pprint :refer [pprint]]))

(def puzzle-input-9 (parse-input "2019-09.txt"))

(defn run-instr [state]
  (let [instr (cur-instr state)
        {ip :ip, base :base, memory :mem, in :in, out :out} state
        op-code (op-code instr)
        mem-get (fn [addr] (or (get memory addr) 0))
        mem-set (fn [addr value]
                  (try
                    (assoc memory addr value)
                    (catch IndexOutOfBoundsException e
                      (assoc (-> memory (concat (repeat (- addr (count memory)) 0)) vec) addr value))))
        arg-out (fn [n]
                 (let [value (mem-get (+ ip n))]
                   (case (param-mode instr n)
                     0 value
                     2 (+ base value)
                     )))
        arg-in (fn [n]
                 (let [value (mem-get (+ ip n))]
                   (case (param-mode instr n)
                     0 (mem-get value)
                     1 value
                     2 (mem-get (+ base value))
                     )))]
    (debug "instr:" instr "op-code:" op-code)
    (merge
     state
     (case op-code
       ;; +
       1 (do (debug "+" (arg-in 1) (arg-in 2) "→" (arg-out 3))
             {:ip (+ ip 4)
              :mem (mem-set (arg-out 3) (+' (arg-in 1) (arg-in 2)))})
       ;; *
       2 (do (debug "*" (arg-in 1) (arg-in 2) "→" (arg-out 3))
             {:ip (+ ip 4)
              :mem (mem-set (arg-out 3) (*' (arg-in 1) (arg-in 2)))})
       ;; input → [1]
       3 (let [input-value (first in)]
           (debug "input" input-value "→" (arg-in 1))
           {:ip (+ ip 2)
            :mem (mem-set (arg-out 1) input-value)
            :in (rest in)})
       ;; output ← [1]
       4 {:ip (+ ip 2)
          :out (conj out (arg-in 1))}
       ;; jump-if-true
       5 (do (debug "jump-if-true" (arg-in 1))
             {:ip (if (not= 0 (arg-in 1)) (arg-in 2) (+ ip 3))})
       ;; jump-if-false
       6 (do (debug "jump-if-false" (arg-in 1))
             {:ip (if (= 0 (arg-in 1)) (arg-in 2) (+ ip 3))})
       ;; less than
       7 (do (debug "less than" (arg-in 1) "<" (arg-in 2) "→" (arg-out 3))
             {:ip (+ ip 4)
              :mem (mem-set (arg-out 3) (if (< (arg-in 1) (arg-in 2)) 1 0))})
       ;; equals
       8 (do (debug "equals" (arg-in 1) "=" (arg-in 2) "→" (arg-out 3))
             {:ip (+ ip 4)
              :mem (mem-set (arg-out 3) (if (= (arg-in 1) (arg-in 2)) 1 0))})
       ;; adjust relative base
       9 (do (debug "adjust relative base" (arg-in 1))
             {:ip (+ ip 2)
              :base (+ base (arg-in 1))})
       ))))

(defn run [state]
  (loop [state (merge {:ip 0, :base 0, :mem [], :in [], :out []} state)]
    (debug "ip:" (state :ip))
    (if (terminated? state)
      state
      (recur (run-instr state)))))

(defn run-prog [mem]
  (run {:ip 0, :base 0, :mem mem, :in [], :out []}))

;; part 2
