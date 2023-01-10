(ns aoc-2022.day11
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn token [s]
  (if (re-matches #"-?\d+" s) (parse-int s) (symbol s)))

(defn update-worryness-fn [[op arg1 arg2]]
  (let [body (list op arg1 arg2)]
    (eval `(fn [~'old] ~body))))

(defn next-monkey-fn [divisor if-true if-false]
  (fn [worryness] (if (multiple? worryness divisor) if-true if-false)))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (split-seq empty?)
       (map #(str/join "\n" (rest %)))
       (re-parse-lines #".*items: (.*)\n.*new = ([a-z0-9]+) (\+|\*) ([a-z0-9]+)\n.*divisible by (\d+)\n.*true:.*(\d+)\n.*false:.*(\d+)"
                       #(hash-map :items (parse-ints %1)
                                  :update-worryness (update-worryness-fn (map token [%3 %2 %4]))
                                  :divisor (parse-int %5)
                                  :next-monkey-fn (next-monkey-fn (parse-int %5) (parse-int %6) (parse-int %7))))
       ))

;; part 1

(defn monkey-play [state n {:keys [update-worryness next-monkey-fn]}]
  (let [items-worryness (get state n)]
    (reduce
      (fn [state worryness]
        (let [new-worryness (update-worryness worryness)
              next-monkey (next-monkey-fn new-worryness)]
          (update state next-monkey conj new-worryness)))
      (assoc state n [])
      (state n))))

(defn round [state rules]
  (reductions
    (fn [state monkey-num]
      (monkey-play state monkey-num (get rules monkey-num)))
    state
    (range (count state))))

(defn rounds [input]
  (let [state (mapv :items input)
        rules input]
    (drop 1 (iterate #(round (last %) rules) [state]))))

(defn monkey-business-level [input nb-rounds]
  (let [nb-monkeys (count input)]
    (->> (rounds input)
         (take nb-rounds)
         (map drop-last)
         (apply concat)
         (map (fn [n items] [n (count (get items n))]) (cycle (range nb-monkeys)))
         (reduce
           (fn [inspected [n m]]
             (update inspected n + m))
           (into [] (repeat nb-monkeys 0)))
         (sort >)
         (take 2)
         (apply *))))

(defn div-by-3 [n] (quot n 3))

(defpart part1 [input]
  (monkey-business-level
    (mapv (fn [monkey-rules] (update monkey-rules :update-worryness #(comp div-by-3 %))) input)
    20))

;; part 2

(defn modulo [n]
  #(mod % n))

(defpart part2 [input]
  (let [max-worryness (reduce * (map :divisor input))]
    (monkey-business-level
      (mapv (fn [monkey-rules] (update monkey-rules :update-worryness #(comp (modulo max-worryness) %))) input)
      10000)))

;; tests

(deftest rounds-test
  (are [n expected] (= expected (-> (rounds (test-data)) (nth n)))
    0 [[79,98] [54,65,75,74] [79,60,97] [74]]
    1 [[20,23,27,26] [2080,25,167,207,401,1046] [] []]
    2 [[695,10,71,135,350] [43,49,58,55,362] [] []]
    20 [[10,12,14,26,34] [245,93,53,199,115] [] []]
    ))
  
(deftest part1-test (part-test part1 10605))

(deftest part2-test (part-test part2 2713310158))
