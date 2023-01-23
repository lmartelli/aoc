(ns aoc-2020.day18
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn parse-expr [s]
  (->> (re-seq #"\d+|[()+*]" s)
       (map #(if (digit? (first %)) (parse-int %) %))))

(def-input-parser [lines]
  (->> (map parse-expr lines)))

;; part 1

(declare eval-group eval-all)

(defn eval-group [[token :as tokens]]
  (cond
    (int? token) [token (rest tokens)]
    (= "(" token) (eval-all (rest tokens))
    :else (throw (Exception. (str "Invalid token" token)))))

(defn eval-all [tokens]
  (loop [value nil
         [token :as tokens] tokens]
    (cond
      (empty? tokens) [value tokens]
      (= ")" token) [value (rest tokens)]
      :else (case token
              "+" (let [[right-hand tokens] (eval-group (rest tokens))]
                    (recur (+ value right-hand) tokens))
              "*" (let [[right-hand tokens] (eval-group (rest tokens))]
                    (recur (* value right-hand) tokens))
              (let [[left-hand tokens] (eval-group tokens)]
                (recur left-hand tokens))
              ))))

(defn eval-expr [tokens]
  (first (eval-all tokens)))

(defpart part1 [input]
  (->> input
       (map eval-expr)
       (reduce +)))

;; part 2

(declare eval-sum eval-prod)

(defn eval-sum [tokens]
  (loop [sum 0
         tokens tokens]
    (if (empty? tokens)
      [sum]
      (case (first tokens)
        "*" [sum tokens]
        ")" [sum tokens]
        "+" (recur sum (rest tokens))
        "(" (let [[term tokens] (eval-prod (rest tokens))]
              (recur (+ sum term) tokens))
        (recur (+ sum (first tokens)) (rest tokens))
        ))))

(defn eval-prod [tokens]
  (loop [prod 1
         tokens tokens]
    (if (empty? tokens)
      [prod]
      (case (first tokens)
        "*" (recur prod (rest tokens))
        ")" [prod (rest tokens)]
        (let [[term tokens] (eval-sum tokens)]
          (recur (* prod term) tokens))))))

(defn eval-expr-2 [tokens]
  (first (eval-prod tokens)))

(defpart part2 [input]
  (->> input
       (map eval-expr-2)
       (reduce +)))

;; tests


(deftest eval-expr-test
  (are [expr expected] (= expected (eval-expr (parse-expr expr)))
    "0" 0
    "1" 1
    "1+2" 3
    "1+2+3" 6
    "1 + 2 * 3 + 4 * 5 + 6" 71
    "2*3" 6
    "1+2*3" 9
    "1+(2*3)" 7
    "1 + (2 * 3) + (4 * (5 + 6))" 51
    "2 * 3 + (4 * 5)" 26
    "5 + (8 * 3 + 9 + 3 * 4 * 3)" 437
    "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" 12240
    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" 13632))

(deftest eval-expr-2-test
  (are [expr expected] (= expected (eval-expr-2 (parse-expr expr)))
    "0" 0
    "1" 1
    "1+2" 3
    "2*3" 6
    "1+2+3" 6
    "2*3*4" 24
    "2*3+1" 8
    "2+3*4" 20
    "2+3*1+2" 15
    "2*3+4*2" 28
    "(2*3)+1" 7
    "1+2+3" 6
    "1 + 2 * 3 + 4 * 5 + 6" 231
    "1+2*3" 9
    "1+(2*3)" 7
    "1 + (2 * 3) + (4 * (5 + 6))" 51
    "2 * 3 + (4 * 5)" 46
    "5 + (8 * 3 + 9 + 3 * 4 * 3)" 1445
    "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" 669060
    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" 23340
    ))

(deftest eval-expr-2-test
  (are [expr expected] (= expected (eval-expr-2 (parse-expr expr)))
    "(2*3)+1" 7
    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" 23340))
