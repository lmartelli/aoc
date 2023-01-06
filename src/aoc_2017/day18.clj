(ns aoc-2017.day18
  (:require
   [aoc.core :refer :all]
   [aoc.cpu :refer :all]
   [aoc-2017.instr :refer :all]
   [clojure.test :refer :all]))

;; part 1

(def instr-set
  (merge
    basic-instr-set
    {:snd (instr [freq] (set-reg :last-freq freq))
     :rcv (instr [tst] (if (zero? ($ tst)) registers (set-reg :out :last-freq)))}))

(defpart part1 [prog]
  (->> (trace-registers {:ip 0} prog instr-set)
       (find-first :out)
       :out))

;; part 2

(def instr-set2
  (merge
    instr-set
    {:snd (instr [value] (update-reg :out conj value))
     :rcv (instr [reg]
                 (-> registers
                     (assoc reg (peek ($ :in)))
                     (update :in pop)))}))

(defn init-process [pid]
  {:p pid :ip 0 :in (queue) :out (queue):snd-count 0})

(defn waiting? [registers [instr]]
  (and (= :rcv instr)
       (empty? (registers :in))))

(defn runnable? [registers prog]
  (and (not (terminated? registers prog))
       (not (waiting? registers (prog (registers :ip))))))

(defn pipe [[p0 p1]]
  [(-> p0
       (update :in into (p1 :out))
       (assoc :out (queue)))
   (-> p1
       (update :in into (p0 :out))
       (assoc :out (queue)))])

(defn update-snd-counts [snd-counts processes]
  (mapv #(+ %1 (count (%2 :out))) snd-counts processes))

(defn run-processes-and-count-snd [prog]
  (loop [processes (mapv init-process [0 1])
         snd-counts [0 0]]
    (if-let [pid-to-run (first-index #(runnable? % prog) processes)]
      (let [new-process (update processes
                                pid-to-run
                                #(run-prog % prog instr-set2 :stop? waiting?))]
        (recur (pipe new-process)
               (update-snd-counts snd-counts new-process)))
      snd-counts)))

(defpart part2 [prog]
  (-> (run-processes-and-count-snd prog)
      (nth 1)))

;; tests

(deftest instruction-set-test-part1
  (testing "Sound instructions"
    (are [instr registers expected] (= expected (exec-instr instr registers instr-set))
      [:snd 123] {} {:last-freq 123}
      [:snd :x] {:x 123} {:last-freq 123, :x 123}
      [:rcv 0] {:a 123 :b 42} {:a 123 :b 42}
      [:rcv 1] {:a 123 :b 42 :last-freq 7} {:a 123 :b 42 :last-freq 7 :out 7}
      [:rcv -1] {:a 123 :b 42 :last-freq 7} {:a 123 :b 42 :last-freq 7 :out 7}
      [:rcv :a] {:a 123 :last-freq 7} {:a 123 :last-freq 7 :out 7}
      [:rcv :b] {:a 123 :last-freq 7} {:a 123 :last-freq 7})))

(deftest instruction-set-test-part2
  (testing ":snd pushes :out register"
    (are [instr registers expected] (= expected (exec-instr instr registers instr-set2))
      [:snd 42] {:out (queue)} {:out [42]}
      [:snd :a] {:a 4 :out (queue [1 2 3])} {:a 4 :out [1 2 3 4]}))
  (testing ":rcv reads from :in register"
    (are [instr registers expected] (= expected (exec-instr instr registers instr-set2))
      [:rcv :a] {:in (queue [1 2 3])} {:a 1 :in [2 3]}
      [:rcv :a] {:in (queue)} {:a nil :in []})))

(deftest part1-test (part-test part1 "1" 4))

(deftest part2-test (part-test part2 "2" 3))

