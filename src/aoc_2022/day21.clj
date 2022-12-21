(ns aoc-2022.day21
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as v]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (split-lines #"[: ]+"
                    (fn ([monkey n]
                         [(keyword monkey) (parse-int n)])
                      ([monkey arg1 op arg2]
                       [(keyword monkey) (vec (map keyword [op arg1 arg2]))])))
       (into {})))

;; part 1

(defn eval-expr [expr monkeys ]
  (cond
    (number? expr) expr
    (keyword? expr) (if-let [monkey-value (monkeys expr)]
                      (eval-expr monkey-value monkeys)
                      expr)
    :else (let [[op & args] expr]
            (apply ({:+ +, :- -, :* *, :/ /} op)
                   (map #(eval-expr % monkeys) args)))))

(defpart part1 [monkeys]
  (eval-expr
    :root
    monkeys))

;; part 2

(defn p-mult [[p0 p1] [q0 q1]]
  (if (some zero? [p1 q1])
    [(* p0 q0) (+ (* p0 q1) (* p1 q0))]
    (throw (Exception. "Cannot multiply to polynomials of degree > 0"))))

(defn p-div [[p0 p1] [q0 q1]]
  (if (zero? q1)
    [(/ p0 q0) (/ p1 q0)]
    (throw (Exception. "Cannot divide by a polynomials"))))

(defn eval-p-expr [expr monkeys]
  (cond
    (number? expr) [expr 0]
    (= :humn expr) [0 1]
    (keyword? expr) (if-let [monkey-value (monkeys expr)]
                      (eval-p-expr monkey-value monkeys)
                      expr)
    :else (let [[op & args] expr]
            (apply ({:+ v/+, :- v/-, :* p-mult, :/ p-div} op)
                   (map #(eval-p-expr % monkeys) args)))))

(defpart part2 [monkeys]
  (let [[p0 p1] (eval-p-expr
                  :root
                  (-> monkeys
                      (dissoc :humn)
                      (update :root assoc 0 :-)))]
    (long (- (/ p0 p1)))))

;; tests

(deftest get-monkey-number-test
  (are [monkeys expected] (= expected (get-monkey-number monkeys :a))
    {:a 42} 42
    {:a [:+ :b :c] :b 3 :c 4} 7
    {:a [:* :b :c] :b 3 :c 4} 12
    {:a [:/ :b :c] :b 10 :c 2} 5
    {:a [:- :b :c] :b 3 :c 4} -1
    ))

(deftest part1-test (part-test part1 152))

(deftest part2-test  (part-test part2 301))
