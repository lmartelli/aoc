(ns aoc-2015.day07
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]))

(defn convert-token [t]
  (if (re-matches #"\d+" t)
    (parse-int t)
    (symbol t)))

(puzzle-input-parse-lines
 (fn [line]
   (let [[expr name] (split line #" -> ")]
     (let [[a b c :as tokens] (->> (split expr #" ") (map convert-token))]
       [(symbol name)
        (case (count tokens)
          1 a
          2 (list a b)
          3 (list b a c))]))))

;; part 1

(def max-val 65535)

(defn eval [e bindings]
  (cond
    (list? e) (let [[op & params] e]
                (apply (resolve op) (map #(eval % bindings) params)))
    (int? e) e
    (symbol? e) (let [def (@bindings e)]
                  (let [res (eval def bindings)]
                    (swap! bindings assoc e res)
                    res))))

(defn RSHIFT [val n]
  (bit-shift-right val n))

(defn LSHIFT [val n]
  (bit-and max-val (bit-shift-left val n)))

(defn OR [a b]
  (bit-or a b))

(defn AND [a b]
  (bit-and a b))

(defn NOT [a]
  (- max-val a))

(defn make-bindings [input]
  (->> input (into {}) atom))

(defpart part1 [input]
  (let [bindings (make-bindings input)]
    (eval 'a bindings)))

;; part 2

(defpart part2 [rules]
  (let [a (part1 rules)
        bindings (make-bindings rules)]
    (swap! bindings assoc 'b a)
    (eval 'a bindings)))

;; tests
