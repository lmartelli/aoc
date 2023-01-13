(ns aoc-2018.day16
  (:require
   [aoc.core :refer :all :exclude [eq]]
   [aoc.algo :as algo]
   [aoc.cpu :as cpu]
   [clojure.set :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (let [[sample-lines prog-lines]
        (->> lines
             (remove empty?)
             (partition-all 3)
             (split-with #(str/starts-with? (first %) "Before:")))]
    {:samples  (map (fn [[before instr after]]
                      (if (str/starts-with? before "Before:")
                        {:instr (parse-ints instr)
                         :before (parse-ints before)
                         :after (parse-ints after)}))
                    sample-lines)
     :program (->> (apply concat prog-lines)
                   (mapv parse-ints))}))

;; part 1

(defmacro $ [mode x]
  `(case ~mode
     :i ~x
     :r (~'registers ~x)))

(defn instr
  ([f ma]
   (fn [registers a _ c]
     (assoc registers c (f ($ ma a)))))
  ([f ma mb]
   (fn [registers a b c]
     (assoc registers c (f ($ ma a) ($ mb b))))))

(defn gt [a b]
  (if (> a b) 1 0))

(defn eq [a b]
  (if (= a b) 1 0))

(def instruction-set
  {:addr (instr + :r :r)
   :addi (instr + :r :i)
   :mulr (instr * :r :r)
   :muli (instr * :r :i)
   :banr (instr bit-and :r :r)
   :bani (instr bit-and :r :i)
   :borr (instr bit-or :r :r)
   :bori (instr bit-or :r :i)
   :gtir (instr gt :i :r)
   :gtri (instr gt :r :i)
   :gtrr (instr gt :r :r)
   :eqir (instr eq :i :r)
   :eqri (instr eq :r :i)
   :eqrr (instr eq :r :r)
   :seti (instr identity :i)
   :setr (instr identity :r)})

(defn exec-instr [registers [op a b c]]
  ((instruction-set op) registers a b c))

(defn sample-match? [{:keys [before after instr]} f]
  (= after (apply f before (rest instr))))

(defn instr-matches [sample]
  (keep
    (fn [[name f]]
      (if (sample-match? sample f)
        name))
    instruction-set))

(defn count-matches [sample]
  (count (instr-matches sample)))

(defpart part1 [{samples :samples}]
  (->> samples
       (filter #(>= (count-matches %) 3))
       count))

;; part 2

(defn sample-opcode [{[code] :instr :as sample}]
  code)

(defn mappings-code->names [samples]
  (-> (group-by sample-opcode samples)
      (update-vals #(->> (map (comp set instr-matches) %)
                         (reduce intersection)))))

(defn resolve-op-codes [samples]
  (algo/resolve-bijection (mappings-code->names samples)))

(defpart part2 [{:keys [samples program]}]
  (let [op-codes (update-keys instruction-set (map-invert (resolve-op-codes samples)))]
    (-> (cpu/run-prog (zipmap (range 4) (repeat 0)) program op-codes)
        (get 0))))

;; tests

(deftest part1-test)

(deftest part2-test)

(deftest count-matches-test
  (is (= 3 (count-matches {:before [3 2 1 1], :instr [9 2 1 2], :after [3 2 2 1]}))))

(deftest instructions-test
  (testing "Addition"
    (are [before instr after] (= after (exec-instr before instr))
      [3 0 0 0] [:addi 0 4 2] [3 0 7 0]
      [3 1 0 0] [:addr 0 1 2] [3 1 4 0]))
  (testing "Multiplication"
    (are [before instr after] (= after (exec-instr before instr))
      [3 0 0 0] [:muli 0 4 2] [3 0 12 0]
      [3 2 0 0] [:mulr 0 1 2] [3 2 6 0]))
  (testing "Bitwise and"
    (are [before instr after] (= after (exec-instr before instr))
      [3 0 0 0] [:bani 0 5 2] [3 0 1 0]
      [3 2 0 0] [:banr 0 1 2] [3 2 2 0]))
  (testing "Bitwise or"
    (are [before instr after] (= after (exec-instr before instr))
      [3 0 0 0] [:bori 0 5 2] [3 0 7 0]
      [3 6 0 0] [:borr 0 1 2] [3 6 7 0]))
  (testing "Greater-than testing"
    (are [before instr after] (= after (exec-instr before instr))
      [2 0 2 1] [:gtir 3 0 2] [2 0 1 1]
      [3 0 2 2] [:gtir 3 0 2] [3 0 0 2]
      [4 0 2 3] [:gtir 3 0 2] [4 0 0 3]
      [2 0 2 4] [:gtri 0 3 2] [2 0 0 4]
      [3 0 2 5] [:gtri 0 3 2] [3 0 0 5]
      [4 0 2 6] [:gtri 0 3 2] [4 0 1 6]
      [2 3 2 7] [:gtrr 0 1 2] [2 3 0 7]
      [3 3 2 8] [:gtrr 0 1 2] [3 3 0 8]
      [4 3 2 9] [:gtrr 0 1 2] [4 3 1 9]))
  (testing "Equality testing"
    (are [before instr after] (= after (exec-instr before instr))
      [2 0 2 1] [:eqir 3 0 2] [2 0 0 1]
      [3 0 2 2] [:eqir 3 0 2] [3 0 1 2]
      [4 0 2 3] [:eqir 3 0 2] [4 0 0 3]
      [2 0 2 4] [:eqri 0 3 2] [2 0 0 4]
      [3 0 2 5] [:eqri 0 3 2] [3 0 1 5]
      [4 0 2 6] [:eqri 0 3 2] [4 0 0 6]
      [2 3 2 7] [:eqrr 0 1 2] [2 3 0 7]
      [3 3 2 8] [:eqrr 0 1 2] [3 3 1 8]
      [4 3 2 9] [:eqrr 0 1 2] [4 3 0 9]))
  (testing "Assignment"
    (are [before instr after] (= after (exec-instr before instr))
      [1 2 3 4] [:seti 7 0 2] [1 2 7 4]
      [1 2 3 4] [:setr 1 0 3] [1 2 3 2])))
