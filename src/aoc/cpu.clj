(ns aoc.cpu
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.test :refer :all]))

;; CPU simulator

(defn parse-arg [str]
  (if (re-matches #"[a-z]" str)
    (keyword str)
    (parse-int str)))

(defn parse-instr [line]
  (let [[op & args] (split line #"[ ,]+")]
    (vec
      (cons (keyword op)
            (map parse-arg args)))))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (mapv parse-instr)))

(defmacro defop
  "Defines a function with an implicit 1st param `{:keys [registers last-freq]}`"
  [name [& params] & body]
  (vector (keyword name) (concat (list 'fn (vec (cons 'registers params))) body)))

(defmacro instr [params & body]
  `(fn [~'registers ~@params]
     ~@body))

(defmacro get-reg [reg]
  `(get ~'registers ~reg 0))

(defmacro $ "Evaluates a value (literal int or register)"
  [arg] `(if (keyword? ~arg) (get-reg ~arg) ~arg))

(defmacro update-reg
  ([reg f]
   `(update ~'registers ~reg (fnil ~f 0)))
  ([reg f param]
   `(update ~'registers ~reg (fnil ~f 0) ($ ~param)))
  ([reg f p1 p2]
   `(update ~'registers ~reg (fnil ~f 0) ($ ~p1) ($ ~p2))))

(defmacro set-reg [reg value]
  `(assoc ~'registers ~reg ($ ~value)))

(defmacro nop []
  'registers)

(defmacro jump-if
  ([offset predicate arg]
   `(if (~predicate ($ ~arg)) ($ ~offset) 1))
  ([offset predicate arg1 arg2]
   `(if (~predicate ($ ~arg1) ($ ~arg2)) ($ ~offset) 1)))

(defmacro jump [offset] `($ ~offset))

(defmacro defops
  "Defines an instruction set.
  An `op` is like `(name [args] body)`
  You can use `set-reg`, `update-reg` or `jump-if` to define the body of operations.
  Arguments of operations, as well as jump offsets can be registers or values.

  Should you need to write more customized operations, you can use ($ x) to
  evaluate a register (to its value) or a constant to itself.
  
  (defops basic-instr-set
    (:set [r arg] (set-reg r arg))
    (:inc [r] (update-reg r inc))
    (:add [r arg] (update-reg r + arg))
    (:jnz [test offset] (jump-if offset #(not= 0 %) test))
    (:jeq [a b offset] (jump-if offset = a b)))"
  [name & ops]
  `(def ~name
     (into {}
           [~@(for [[name args body] ops]
                `(defop ~name ~args ~body))])))

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
    (throw (Exception. (format "Unknown instruction '%s' at ip=%d"  op (registers :ip))))))

(defn step-instr [registers prog instr-set]
  (let [instr (prog (registers :ip))
        jump-offset-or-registers (exec-instr instr registers instr-set)]
    (if (integer? jump-offset-or-registers)
      (update registers :ip + jump-offset-or-registers)
      (update jump-offset-or-registers :ip inc))))

(defn terminated? [registers prog]
  (not (contains? prog (registers :ip))))

(defn trace-registers
  "Returns sequence of registers state until program terminates.
  Last value is when :ip is out of bounds.
  If provided, `stop?` must be a function of [registers instr],
  and allows to stop execution before normal termination."
  [registers prog instr-set &{:keys [stop?] :or {stop? (constantly false)}}]
  (->> (iterate #(step-instr % prog instr-set) (merge {:ip 0} registers))
       (take-until #(or (terminated? % prog)
                        (stop? % (prog (% :ip)))))))

(defn trace-instr-and-registers
  "Returns sequence of [instr registers] until program terminates.
  Last value is when :ip is out of bounds."
  [registers prog instr-set]
  (->> (trace-registers registers prog instr-set)
       (map (juxt #(prog (:ip %)) identity))))

(defn trace-instr
  "Returns sequence of instr executed until program terminates.
  Last value is when :ip is out of bounds."
  [registers prog instr-set]
  (->> (trace-registers registers prog instr-set)
       (take-while #(not (terminated? % prog)))
       (map #(prog (:ip %)))))

(defn run-prog
  "Terminates when IP points out of the program.
   If provided, `stop?` must be a function of [registers instr],
   and allows to stop execution before normal termination."
  ([registers prog instr-set &{:keys [stop?] :or {stop? (constantly false)}}]
   (last (trace-registers (merge {:ip 0} registers) prog instr-set :stop? stop?))))

;; tests

(def test-instr-set
  {:set (instr [reg val] (set-reg reg val))
   :add (instr [reg y] (update-reg reg + y))
   :jnz (instr [tst offset] (jump-if offset #(not= 0 %) tst))})

(deftest step-instr-test
      (testing "If instr return an integer, jump by that offset"
        (is (= {:ip 42} (step-instr {:ip 0} [[:jmp]] {:jmp (instr [] 42)})))))

(deftest step-instr-test
  ;; Run a single instruction, adding implicit register :ip 0
  (let [test-instr (fn [instr registers] 
                     (step-instr (merge {:ip 0} registers) [instr] test-instr-set))]
    (testing "Registers manipulation"
      (testing "IP is incremented by 1"
        (are [instr registers] (= 1 (:ip (test-instr instr registers)))
          [:set :a 42] {:a 3}
          [:add :a 3] {:a 4}))
      (testing "Registers are updated"
        (are [instr registers expected] (= expected (dissoc (test-instr instr registers) :ip))
          [:set :a 42] {} {:a 42}
          [:set :a :b] {:b 42} {:a 42 :b 42}
          [:add :a 3] {:a 5} {:a 8}
          [:add :a :b] {:a 5 :b -1} {:a 4 :b -1}))
      (testing "Registers default to 0"
        (are [instr registers expected] (= expected (dissoc (test-instr instr registers) :ip))
          [:set :a :b] {:a 42} {:a 0}
          [:add :a 3] {} {:a 3}
          [:add :a :b] {:a 5} {:a 5})))
    (testing "Jumps"
      (testing "If instr return an integer, jump by that offset"
        (is (= {:ip 42} (step-instr {:ip 0} [[:jmp]] {:jmp (instr [] 42)}))))
      (testing "Do not update normal registers"
        (let [registers {:a 42 :b 3}]
          (are [instr] (= registers (dissoc (test-instr instr registers) :ip))
            [:jnz  0 3]
            [:jnz -1 3]
            [:jnz  1 3]
            [:jnz  :a :b])))
      (testing "Adds offset to :ip"
        (are [instr registers expected] (= expected (:ip (test-instr instr registers)))
          [:jnz  0 3] {} 1
          [:jnz -1 3] {} 3
          [:jnz  1 3] {} 3
          [:jnz  1 :a] {:a -2} -2
          [:jnz :t :a] {:a -2 :t  0} 1
          [:jnz :t :a] {:a -2 :t -1} -2
          [:jnz :t :a] {:a -2 :t  1} -2)))))

(deftest run-prog-test
  (testing "Stops when :ip is out of bounds"
    (are [prog expected] (= expected (run-prog {} prog test-instr-set))
      [[:set :a 42] [:add :a 3]] {:ip 2 :a 45}
      [[:set :a 42] [:add :a 3] [:jnz :a -5]] {:ip -3 :a 45}))
  (testing "Stops on specific condition"
    (are [prog expected] (= expected (run-prog {} prog test-instr-set :stop? (fn [registers instr] (> ($ :a) 42))))
      [[:set :a 42]
       [:add :a 3]
       [:jnz :a -5]
       ] {:ip 2 :a 45})))
