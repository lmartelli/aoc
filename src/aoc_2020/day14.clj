(ns aoc-2020.day14
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (->> lines
       (map (fn [l]
              (if (str/starts-with? l "mask")
                [:mask (re-find #"[X01]+" l)]
                (let [[addr value] (parse-ints l)]
                  [:mem addr value]))))))

;; part 1

(defn parse-bit-mask [bit-mask]
  [(tr bit-mask {\X \1, \1 \0})
   (tr bit-mask {\X \0})])

(defn apply-bit-mask [value bit-mask]
  (-> (map
        (fn [value-bit mask]
          (if (= \X mask)
            value-bit
            mask))
        (format-bin value (count bit-mask))
        bit-mask)
      bits-to-int)
  )

(defn exec [prog]
  (reduce
    (fn [state [instr arg1 arg2]]
      (case instr
        :mask (assoc state :mask arg1)
        :mem (assoc-in state [:mem arg1] (apply-bit-mask arg2 (state :mask)))))
    {}
    prog))

(defn exec-and-sum [exec prog]
    (->> (exec prog)
         :mem
         vals
         (reduce +)))

(defpart part1 [prog]
  (exec-and-sum exec prog))

;; part 2

(defn apply-bit-mask-2 [value mask]
  (loop [[v-bit :as value] (format-bin value (count mask))
         [m-bit :as mask] mask
         values [[]]]
    (if (nil? v-bit)
      (map bits-to-int values)
      (recur
        (rest value)
        (rest mask)
        (case m-bit
          \X (mapcat #(list (conj % \0) (conj % \1)) values)
          \0 (map #(conj % v-bit) values)
          \1 (map #(conj % \1) values))))))

(defn write-mem [mem addr value mask]
  (reduce
    (fn [mem addr]
      (assoc mem addr value))
    mem
    (apply-bit-mask-2 addr mask)))

(defn exec-2 [prog]
  (reduce
    (fn [state [instr arg1 arg2]]
      (case instr
        :mask (assoc state :mask arg1)
        :mem (update state :mem write-mem arg1 arg2 (state :mask))))
    {}
    prog))

(defpart part2 [prog]
  (exec-and-sum exec-2 prog))

;; tests

(deftest apply-bit-mask-test
  (are [value bit-mask expected] (= expected (apply-bit-mask value bit-mask))
     11  "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 73
     101 "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 101
     0   "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 64))

(deftest part1-test
  (test-with-lines
    part1
    ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
     "mem[8] = 11"
     "mem[7] = 101"
     "mem[8] = 0"]
    165))

(deftest apply-bit-mask-2-test
  (are [value mask expected] (= expected (set (apply-bit-mask-2 value mask)))
    1 "0" #{1}
    0 "0" #{0}
    0 "1" #{1}
    1 "1" #{1}
    0 "X" #{0 1}
    1 "X" #{0 1}
    42 "X1001X" #{26 27 58 59}
    26 "0X0XX" #{16 17 18 19 24 25 26 27}))

(deftest part2-test
  (test-with-lines
    part2
    ["mask = 000000000000000000000000000000X1001X"
     "mem[42] = 100"
     "mask = 00000000000000000000000000000000X0XX"
     "mem[26] = 1"]
    208))
