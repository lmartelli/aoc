(ns aoc-2020.day08
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-split-lines
    stream #" "
    (fn [[op arg]] [(keyword op) (parse-int arg)])))

;; part 1

(def ops
  {:nop (fn [state arg] (update state :ip inc))
   :acc (fn [state arg] (-> state (update :acc #(+ arg %)) (update :ip inc)))
   :jmp (fn [state arg] (-> state (update :ip #(+ arg %))))
   })

(def init-state {:ip 0, :acc 0})

(defn instr-op [instr] (first instr))

(defn instr-arg [instr] (second instr))

(defn exec-instr [state prog]
  (let [instr (prog (state :ip))]
    ((ops (instr-op instr)) state (instr-arg instr))))

(defn exec-until-loop [state prog]
  (->>
    (iterate
      (fn [[state ip-history]]
        [(exec-instr state prog) (conj ip-history (state :ip))])
      [state #{}])
    (find-first (fn [[state ip-history]] (ip-history (state :ip))))
    first))

(defpart part1 [prog]
  (->> (exec-until-loop init-state prog)
      :acc))

;; part 2

(defn terminated? [state prog]
  (>= (state :ip) (count prog)))

(defn terminal-state-or-nil [prog state]
  (if (terminated? state prog)
    state
    nil))

(defn exec-until-loop-or-termination [state prog]
  (->>
    (iterate
      (fn [[state ip-history]]
        [(exec-instr state prog) (conj ip-history (state :ip))])
      [state #{}])
    (find-first (fn [[state ip-history]]
                  (or (terminated? state prog)
                      (ip-history (state :ip)))))
    first
    (terminal-state-or-nil prog)))

(defn switch-jmp-or-nop [prog pos]
  (update-in prog [pos 0] {:nop :jmp, :jmp :nop}))

(defn fix-infinite-loop [prog]
  (loop [state init-state]
    (let [ip (state :ip)
          instr (prog ip)
          op (instr-op instr)]
      (if (= op :acc)
        (recur (exec-instr state prog))
        (if-let [terminal-state (exec-until-loop-or-termination state (switch-jmp-or-nop prog ip))]
          terminal-state
          (recur (exec-instr state prog)))))))

(defpart part2 [prog]
  (-> (fix-infinite-loop prog)
      :acc))

;; test

(deftest exec-instr-test
  (let [prog (puzzle-input (test-input))]
    (are [state expected] (= expected (exec-instr state prog))
      {:ip 0, :acc 0} {:ip 1, :acc 0}
      {:ip 1, :acc 0} {:ip 2, :acc 1}
      {:ip 2, :acc 1} {:ip 6, :acc 1})))

(deftest part1-test (part-test part1 5))

(deftest part1-test (part-test part2 8))
