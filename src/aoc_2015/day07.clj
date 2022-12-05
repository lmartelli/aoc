(ns aoc-2015.day07
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.test :refer :all]))

(defn convert-token [t]
  (if (re-matches #"\d+" t)
    (parse-int t)
    (keyword t)))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines
   stream
   (fn [line]
     (let [[expr name] (split line #" -> ")]
       (let [[a b c :as tokens] (->> (split expr #" ") (map convert-token))]
         [(keyword name)
          (case (count tokens)
            1 a
            2 (list a b)
            3 (list b a c))])))))

;; part 1

(def MAX_VAL 65535)

(def operators
  {:OR bit-or
   :AND bit-and
   :NOT #(- MAX_VAL %)
   :LSHIFT #(bit-and MAX_VAL (bit-shift-left %1 %2))
   :RSHIFT bit-shift-right})

(def eval
  (memoize
   (fn [e bindings]
     ;;(println e)
     (cond
       (list? e) (let [[op & params] e]
                   (apply (operators op) (map #(eval % bindings) params)))
       (int? e) e
       (keyword? e) (eval (bindings e) bindings)))))

(defn make-bindings [input]
  (into {} input))

(defpart part1 [input]
  (let [bindings (make-bindings input)]
    (eval :a bindings)))

;; part 2

(defpart part2 [input]
  (let [bindings (make-bindings input)
        a (eval :a bindings)]
    (eval :a (assoc bindings :b a))))

;; tests

(deftest eval-test
  (let [bindings (make-bindings (puzzle-input (test-input)))]
    (are [wire expected] (= expected (eval wire bindings))
      :d 72
      :e 507
      :f 492
      :g 114
      :h 65412
      :i 65079
      :x 123
      :y 456)))
