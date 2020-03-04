(ns aoc-2017.day16
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.test :refer :all]))

(defn parse-move [move]
  (let [cmd (first move)
        args (split (subs move 1) #"/")]
    (case cmd
      \s [:spin (parse-int (args 0))]
      \x [:exchange (parse-int (args 0)) (parse-int (args 1))]
      \p [:partner (get move 1) (get move 3)])))

(defn parse-moves [moves]
  (map parse-move (split moves #",")))

(puzzle-input-split #"," parse-move)

;; part 1

(defmacro $move
  "Defines a function with an implicit 1st param `programs`"
  [[& params] body]
  (list 'fn (vec (cons 'programs params)) body))

(def moves
  {:spin
   ($move [n] (shift-right programs n))
   :exchange
   ($move [a b] (swap programs a b))
   :partner
   ($move [a b] (swap programs (index-of programs a) (index-of programs b)))})

(defn dance [programs moves-seq]
  (reduce
   (fn [programs [move & args]]
     (apply (moves move) programs args))
   programs
   moves-seq))

(def initial-state (vec "abcdefghijklmnop"))

(defpart part1 [input]
  (apply str (dance initial-state input)))

;; part 2

(defn dance-seq [initial-state moves-seq]
  (iterate #(dance % moves-seq) initial-state))

(defn find-freq [seq]
  (let [first-value (first seq)]
    (reduce
     (fn [i value]
       (if (= value first-value)
         (reduced i)
         (inc i)))
     1
     (rest seq))))

(defpart part2 [input]
  (let [s (dance-seq initial-state input)
        freq (find-freq s)]
    (apply str (nth s (rem (int 1e9) freq)))))

;; tests

(deftest dance-test
  (is (= "baedc" (apply str (dance (vec "abcde") (parse-moves "s1,x3/4,pe/b"))))))
