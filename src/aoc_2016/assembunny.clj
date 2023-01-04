(ns aoc-2016.assembunny
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

;; Input parsing

(defn keyword-or-int [s]
  (if (letter? (first s))
    (keyword s)
    (parse-int s)))

(defn puzzle-input [stream]
  (->> stream
       line-seq
       (mapv (fn [line]
               (->> (str/split line #" +")
                    (mapv keyword-or-int))))))

;; Program running

(def init-registers (constantly {:a 0 :b 0 :c 0 :d 0 :ip 0}))

(defn eval-expr [registers expr]
  (if (keyword? expr) (registers expr) expr))

(defn non-jump-instr [f]
  (fn [register & operands]
    (-> (apply f register operands)
        (update! :ip inc))))

(defn update-register [f]
  (fn [registers r] (update! registers r f)))

(defn jump-if [pred]
  (fn [registers value offset]
    (update! registers :ip
             #(+ % (if (pred (eval-expr registers value)) (eval-expr registers offset) 1)))))

(defn copy [registers value r]
  (assoc! registers r (eval-expr registers value)))

(def instruction-set
  {:cpy (non-jump-instr copy)
   :inc (non-jump-instr (update-register inc))
   :dec (non-jump-instr (update-register dec))
   :jnz (jump-if #(not= 0 %))})

(defn exec-instr [registers [instr & operands] instruction-set]
  (apply (instruction-set instr) registers operands))

(defn run-prog [prog registers]
  (loop [registers (transient registers)]
    (if (not (<= 0 (registers :ip) (dec (count prog))))
      (persistent! registers)
      (recur (exec-instr registers (prog (registers :ip)) instruction-set)))))

;; tests
