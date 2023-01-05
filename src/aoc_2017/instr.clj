(ns aoc-2017.instr
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.test :refer :all]))

(defn parse-arg [str]
  (if (re-matches #"[a-z]" str)
    (keyword str)
    (parse-int str)))

(defn parse-instr [line]
  (let [[op & args] (split line #" ")]
    (vec
      (cons (keyword op)
            (map parse-arg args)))))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (mapv parse-instr)))

;; part 1

(defmacro defop
  "Defines a function with an implicit 1st param `{:keys [registers last-freq]}`"
  [name [& params] & body]
  (vector (keyword name) (concat (list 'fn (vec (cons 'registers params))) body)))

(defmacro get-reg [reg]
  `(get ~'registers ~reg 0))

(defn eval-arg [registers arg]
  (if (keyword? arg) (get-reg arg) arg))

(defmacro $ "Evaluates a value (literal int or register)"
  [arg] `(eval-arg ~'registers ~arg))

(defmacro update-reg [reg f param]
  `(assoc ~'registers ~reg (~f ($ ~reg) ($ ~param))))

(defmacro set-reg [reg value]
  `(assoc ~'registers ~reg ($ ~value)))

(def basic-instr-set
  (into
   {}
   [(defop set [reg val] (set-reg reg val))
    (defop add [reg y] (update-reg reg + y))
    (defop sub [reg y] (update-reg reg - y))
    (defop mul [reg y] (update-reg reg * y))
    (defop mod [reg y] (update-reg reg mod y))
    (defop jnz [t offset] (if-not (zero? ($ t)) (set-reg :jmp offset) registers))
    (defop jgz [t offset] (if (pos? ($ t)) (set-reg :jmp offset) registers))]))

(def debug? (atom false))

(defn set-debug [value]
  (reset! debug? value))

(defn trace [& args]
  (when @debug?
    (apply println args)))

(defn exec-instr [[op & args] registers instr-set]
  (trace "exec-instr" op args)
  (if-let [f-op (instr-set op)]
    (apply f-op (cons registers args))
    (throw (Exception. (format "#%d: ip=%d Unknown operator '%s' " (registers :pid) (registers :ip) op)))))

(defn get-instr [registers prog]
  (get prog (registers :ip)))

(defn step-instr [registers prog instr-set]
  (let [instr (get-instr registers prog)
        next-state (exec-instr instr registers instr-set)]
    (-> next-state
        (update :ip + (or (:jmp next-state) 1))
        (dissoc :jmp))))

(defn run-prog-until [registers prog instr-set pred]
  (->> (iterate #(step-instr % prog instr-set) registers)
       (find-first #(pred % prog))))

(defn terminated? [registers prog]
  (not (contains? prog (registers :ip))))

(defn run-prog [prog instr-set]
  (run-prog-until {:ip 0} prog instr-set terminated?))

;; tests

(deftest exec-instr-test
  (are [instr registers expected] (= expected (exec-instr instr registers basic-instr-set))
    [:set :a 42] {} {:a 42}
    [:set :a :b] {:b 42} {:a 42 :b 42}
    [:add :a 3] {} {:a 3}
    [:add :a 3] {:a 5} {:a 8}
    [:add :a :b] {:a 5} {:a 5}
    [:add :a :b] {:a 5 :b -1} {:a 4 :b -1}
    [:sub :a 3] {} {:a -3}
    [:sub :a 3] {:a 5} {:a 2}
    [:sub :a :b] {:a 5} {:a 5}
    [:sub :a :b] {:a 5 :b -1} {:a 6 :b -1}
    [:mul :a 3] {} {:a 0}
    [:mul :a 3] {:a 5} {:a 15}
    [:mul :a :b] {} {:a 0}
    [:mul :a :b] {:b 5} {:a 0 :b 5}
    [:mul :a :b] {:a 3 :b 5} {:a 15 :b 5}
    [:jgz  0 3] {} {}
    [:jgz -1 3] {} {}
    [:jgz  1 3] {} {:jmp 3}
    [:jgz  1 :a] {:a -2} {:a -2 :jmp -2}
    [:jgz :t :a] {:a -2 :t  0} {:a -2 :t  0}
    [:jgz :t :a] {:a -2 :t -1} {:a -2 :t -1}
    [:jgz :t :a] {:a -2 :t  1} {:a -2 :t  1 :jmp -2}
    [:jnz  0 3] {} {}
    [:jnz -1 3] {} {:jmp 3}
    [:jnz  1 3] {} {:jmp 3}
    [:jnz  1 :a] {:a -2} {:a -2 :jmp -2}
    [:jnz :t :a] {:a -2 :t  0} {:a -2 :t  0}
    [:jnz :t :a] {:a -2 :t -1} {:a -2 :t -1 :jmp -2}
    [:jnz :t :a] {:a -2 :t  1} {:a -2 :t  1 :jmp -2}))
