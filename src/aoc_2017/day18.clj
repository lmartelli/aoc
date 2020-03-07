(ns aoc-2017.day18
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn parse-arg [str]
  (if (re-matches #"[a-z]" str)
    (keyword str)
    (parse-int str)))

(puzzle-input-split-lines
 #" "
 (fn [[op & args]]
   (vec
    (cons (keyword op)
          (map parse-arg args)))))

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

(def instr-set
  (into
   {}
   [(defop snd [freq] (set-reg :last-freq freq))
    (defop set [reg val] (set-reg reg val))
    (defop add [reg y] (update-reg reg + y))
    (defop mul [reg y] (update-reg reg * y))
    (defop mod [reg y] (update-reg reg mod y))
    (defop rcv [t] (if (zero? ($ t)) registers (set-reg :out :last-freq)))
    (defop jgz [t offset] (if (> ($ t) 0) (set-reg :jmp offset) registers))]))

(defn exec-instr [[op & args] registers instr-set]
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

(defpart part1 [prog]
  (-> (run-prog-until {:ip 0} prog instr-set (fn [registers prog] (registers :out)))
      :out))

;; part 2

(defn queue
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([& items] (into (queue) items)))

(defn inc-snd-count [registers]
  (if (:snd-count registers)
    (update registers :snd-count inc)
    registers))

(def instr-set2
  (into
   instr-set
   [(defop snd [value]
      (-> registers
          inc-snd-count
          (update :out conj ($ value))))
    (defop rcv [reg]
      (if (empty? (get-reg :in))
        (assoc registers :jmp 0)
        (-> registers
            (assoc reg (peek (get-reg :in)))
            (update :in pop))))
    ]))

(defn init-process [pid prog]
  {:registers {:p pid :pid pid :ip 0 :in (queue) :out [] :snd-count 0}
   :prog prog})

(defn init-processes [prog n]
  (mapv #(init-process % prog) (range n)))

(defn waiting? [registers prog]
  (and (= :rcv (first (get-instr registers prog)))
       (empty? (registers :in))))

(defn terminated? [registers prog]
  (not (contains? prog (registers :ip))))

(defn send [processes from to]
  (-> processes
      (assoc-in  [from :registers :out] [])
      (update-in [to :registers :in] into (get-in processes [from :registers :out]))))

(defn cycle-send [processes]
  (let [nb-proc (count processes)]
    (reduce
     (fn [processes from] (send processes from (mod (inc from) nb-proc)))
     processes
     (range nb-proc))))

(defn stopped? [registers prog]
  (or (waiting? registers prog) (terminated? registers prog)))

(defn run-process-until-waiting [{:keys [registers prog] :as process} instr-set]
  (update process :registers
          run-prog-until prog instr-set stopped?))

(defn all-stopped? [processes]
  (every? (fn [{:keys [registers prog]}]
            (stopped? registers prog))
          processes))

(defn run-progs [prog]
  (loop [processes (init-processes prog 2)]
    (let [processes (cycle-send processes)]
      (if (deadlock? processes)
        processes
        (recur (mapv #(run-process-until-waiting % instr-set2) processes)))
      )))

(defpart part2 [prog]
  (-> (run-progs prog)
      (get-in [1 :registers :snd-count])))

;; tests

(deftest exec-instr-test
  (are [instr registers expected] (= expected (exec-instr instr registers instr-set))
    [:snd 123] {} {:last-freq 123}
    [:snd :x] {:x 123} {:last-freq 123, :x 123}
    [:set :a 42] {} {:a 42}
    [:set :a :b] {:b 42} {:a 42 :b 42}
    [:add :a 3] {} {:a 3}
    [:add :a 3] {:a 5} {:a 8}
    [:add :a :b] {:a 5} {:a 5}
    [:add :a :b] {:a 5 :b -1} {:a 4 :b -1}
    [:mul :a 3] {} {:a 0}
    [:mul :a 3] {:a 5} {:a 15}
    [:mul :a :b] {} {:a 0}
    [:mul :a :b] {:b 5} {:a 0 :b 5}
    [:mul :a :b] {:a 3 :b 5} {:a 15 :b 5}
    [:rcv 0] {:a 123 :b 42} {:a 123 :b 42}
    [:rcv 1] {:a 123 :b 42 :last-freq 7} {:a 123 :b 42 :last-freq 7 :out 7}
    [:rcv -1] {:a 123 :b 42 :last-freq 7} {:a 123 :b 42 :last-freq 7 :out 7}
    [:rcv :a] {:a 123 :last-freq 7} {:a 123 :last-freq 7 :out 7}
    [:rcv :b] {:a 123 :last-freq 7} {:a 123 :last-freq 7}
    [:jgz 0 3] {} {}
    [:jgz -1 3] {} {}
    [:jgz 1 3] {} {:jmp 3}
    [:jgz 1 :a] {:a -2} {:a -2 :jmp -2}
    [:jgz :t :a] {:a -2 :t 0} {:a -2 :t 0}
    [:jgz :t :a] {:a -2 :t -1} {:a -2 :t -1}
    [:jgz :t :a] {:a -2 :t 1} {:a -2 :t 1 :jmp -2}))

(deftest exec-instr-set2-test
  (are [instr registers expected] (= expected (exec-instr instr registers instr-set2))
    [:snd 4] {:out [1 2 3]} {:out [1 2 3 4]}
    [:snd :x] {:x 4 :out [1 2 3]} {:x 4, :out [1 2 3 4]}
    [:rcv :a] {:a 123 :in (queue 1 2 3)} {:a 1 :in (queue 2 3)}
    [:rcv :a] {:a 123 :in (queue)} {:a 123 :in (queue) :jmp 0}
    ))

(deftest send-test
  (let [before [{:registers {:in [1 2] :out [3 4]}}
                {:registers {:in [5 6] :out [7 8]}}
                {:registers {:in [9 10] :out [11 12]}}]]
    (are [from to after] (= after (send before from to))
      0 1 [{:registers {:in [1 2] :out []}}
           {:registers {:in [5 6 3 4] :out [7 8]}}
           {:registers {:in [9 10] :out [11 12]}}]
      0 2 [{:registers {:in [1 2] :out []}}
           {:registers {:in [5 6] :out [7 8]}}
           {:registers {:in [9 10 3 4] :out [11 12]}}]
      1 0 [{:registers {:in [1 2 7 8] :out [3 4]}}
           {:registers {:in [5 6] :out []}}
           {:registers {:in [9 10] :out [11 12]}}]
      1 2 [{:registers {:in [1 2] :out [3 4]}}
           {:registers {:in [5 6] :out []}}
           {:registers {:in [9 10 7 8] :out [11 12]}}])))
      
(deftest cycle-send-test
  (is (= [{:registers {:in [1 2 11 12] :out []}}
          {:registers {:in [5 6 3 4] :out []}}
          {:registers {:in [9 10 7 8] :out []}}]
         (cycle-send
          [{:registers {:in [1 2] :out [3 4]}}
           {:registers {:in [5 6] :out [7 8]}}
           {:registers {:in [9 10] :out [11 12]}}]))))

(deftest run-process-until-waiting-test
  (let [prog [[:set :a 24] [:rcv :b]]]
    (is (= {:registers {:p 0 :pid 0 :a 24 :ip 1 :in (queue) :out []} :prog prog}
           (run-process-until-waiting
            (init-process 0 prog)
            instr-set2)))))
