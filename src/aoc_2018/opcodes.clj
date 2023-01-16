(ns aoc-2018.opcodes
  (:require
   [clojure.test :refer :all]))

(defmacro $ [mode x]
  `(case ~mode
     :i ~x
     :r (~'registers ~x)))

(defn set-reg [f ma]
  (fn [registers a _ c]
    (assoc registers c (f ($ ma a)))))

(defn update-reg [f ma mb]
  (fn [registers a b c]
    (assoc registers c (f ($ ma a) ($ mb b)))))

(defn gt [a b]
  (if (> a b) 1 0))

(defn eq [a b]
  (if (= a b) 1 0))

(def instruction-set
  {:addr (update-reg + :r :r)
   :addi (update-reg + :r :i)
   :mulr (update-reg * :r :r)
   :muli (update-reg * :r :i)
   :banr (update-reg bit-and :r :r)
   :bani (update-reg bit-and :r :i)
   :borr (update-reg bit-or :r :r)
   :bori (update-reg bit-or :r :i)
   :gtir (update-reg gt :i :r)
   :gtri (update-reg gt :r :i)
   :gtrr (update-reg gt :r :r)
   :eqir (update-reg eq :i :r)
   :eqri (update-reg eq :r :i)
   :eqrr (update-reg eq :r :r)
   :seti (set-reg identity :i)
   :setr (set-reg identity :r)})


(defn init-registers [nb-registers]
  (zipmap (range nb-registers) (repeat 0)))
