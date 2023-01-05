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

(defmacro update-reg
  ([reg f]
   `(update ~'registers ~reg (fnil ~f 0)))
  ([reg f param]
   `(update ~'registers ~reg (fnil ~f 0) ($ ~param)))
  ([reg f p1 p2]
   `(update ~'registers ~reg (fnil ~f 0) ($ ~p1) ($ ~p2))))

(defmacro set-reg [reg value]
  `(assoc ~'registers ~reg ($ ~value)))

(defmacro jump-if [condition offset]
  `(if ~condition ($ ~offset) 1))

(def basic-instr-set
  (into
   {}
   [(defop set [reg val] (set-reg reg val))
    (defop add [reg y] (update-reg reg + y))
    (defop sub [reg y] (update-reg reg - y))
    (defop mul [reg y] (update-reg reg * y))
    (defop mod [reg y] (update-reg reg mod y))
    (defop jnz [t offset] (jump-if (not= 0 ($ t)) offset))
    (defop jgz [t offset] (jump-if (pos? ($ t)) offset))]))

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
        jump-offset-or-registers (exec-instr instr registers instr-set)]
    (if (integer? jump-offset-or-registers)
      (update registers :ip + jump-offset-or-registers)
      (update jump-offset-or-registers :ip inc))))

(defn run-prog-until [registers prog instr-set pred]
  (->> (iterate #(step-instr % prog instr-set) registers)
       (find-first #(pred % prog))))

(defn terminated? [registers prog]
  (not (contains? prog (registers :ip))))

(defn run-prog
  ([prog] (run-prog prog basic-instr-set))
  ([prog instr-set]
   (run-prog-until {:ip 0} prog instr-set terminated?)))

;; tests

(deftest exec-instr-test
  (testing "Registers manipulation"
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
      [:mul :a :b] {:a 3 :b 5} {:a 15 :b 5}))
  (testing "Jumps"
    (are [instr registers expected] (= expected (exec-instr instr registers basic-instr-set))
      [:jgz  0 3] {} 1
      [:jgz -1 3] {} 1
      [:jgz  1 3] {} 3
      [:jgz  1 :a] {:a -2} -2
      [:jgz :t :a] {:a -2 :t  0} 1
      [:jgz :t :a] {:a -2 :t -1} 1
      [:jgz :t :a] {:a -2 :t  1} -2
      [:jnz  0 3] {} 1
      [:jnz -1 3] {} 3
      [:jnz  1 3] {} 3
      [:jnz  1 :a] {:a -2} -2
      [:jnz :t :a] {:a -2 :t  0} 1
      [:jnz :t :a] {:a -2 :t -1} -2
      [:jnz :t :a] {:a -2 :t  1} -2)))

(deftest run-prog-test
  (is (= {:ip 32, :b 65, :c 65, :f 0, :d 65, :e 65, :g 0, :h 1}
         (run-prog (puzzle-input (clojure.java.io/reader "test/aoc_2017/instr.input"))))))
