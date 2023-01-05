(ns aoc-2017.day08
  (:require
   [aoc.core :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines
    stream
    #(let [[_ target-reg instr param test-reg test-op test-value]
           (re-matches #"(\w+) (inc|dec) (-?\d+) if (\w+) (>|<|>=|<=|==|!=) (-?\d+)" %)]
       {:instr (list (symbol instr) (symbol target-reg) (parse-int param))
        :cond (list (symbol test-op) (symbol test-reg) (parse-int test-value))}))  )

(defmacro $
  "Gets value of a register, defaulting to 0."
  [r]
  `(get ~'registers ~r 0))

(defmacro $fn
  "Defines a function with an implicit 1st param `register`"
  [[& params] body]
  (list 'fn (vec (cons 'registers params)) body))

(def ops
  {:== ($fn [r v] (= ($ r) v))
   :!= ($fn [r v] (not= ($ r) v))
   :< ($fn [r v] (< ($ r) v))
   :> ($fn [r v] (> ($ r) v))
   :<= ($fn [r v] (<= ($ r) v))
   :>= ($fn [r v] (>= ($ r) v))
   :inc ($fn [r v] (assoc registers r (+ ($ r) v)))
   :dec ($fn [r v] (assoc registers r (- ($ r) v)))
   })

(defn call [registers [op reg param]]
  ((ops (keyword op)) registers reg param))

(defn eval-instr [registers {:keys [:instr :cond]}]
  (if (call registers cond)
    (call registers instr)
    registers))

;; part 1

(defpart part1 [input]
  (max-val (reduce eval-instr {} input)))

;; part 2

(defpart part2 [input]
  (max-val (apply merge-with max (reductions eval-instr {} input))))

;; tests
