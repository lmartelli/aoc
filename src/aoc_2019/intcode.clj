(ns aoc-2019.intcode
  (:require
   [aoc.core :refer :all]))

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

(defn cur-instr [state]
  ((state :mem) (state :ip)))

(defn terminated? [state] (= (op-code (cur-instr state)) 99))

(defn run-instr
  "Executes an instruction.
  `state` is the machine's state (ip,base and memory)
  `in` is a no argument function to be called to get an input value
  `out` is a one argument function to be called to output a value"
  [state in out]
  (let [instr (cur-instr state)
        {ip :ip, base :base, memory :mem} state
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
       3 (let [input-value (in)]
           (debug "input" input-value "→" (arg-out 1))
           {:ip (+ ip 2)
            :mem (mem-set (arg-out 1) input-value) })
       ;; output ← [1]
       4 (do (debug "output" (arg-in 1))
             (out (arg-in 1))
             { :ip (+ ip 2) })
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

(defn run
  "Runs a intcode program until termination (opcode 99).
   `in` is a no argument function to be called to get an input value
   `out` is a one argument function to be called to output a value"
  [state in out]
  (loop [state (merge {:ip 0, :base 0, :mem []} state)]
    (debug "ip:" (state :ip))
    (if (terminated? state)
      state
      (recur (run-instr state in out)))))

(defn run-prog
  "Runs a intcode program until termination (opcode 99).
   `prog` intcode program to run
   `in` is a no argument function to be called to get an input value
   `out` is a one argument function to be called to output a value"
  [prog in out]
  (run {:mem prog} in out))
