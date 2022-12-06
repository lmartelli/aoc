(ns aoc-2016.day12
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.test :refer :all]))

(defn keyword-or-int [s]
  (if (letter? (first s))
    (keyword s)
    (parse-int s)))

(defn puzzle-input [stream]
  (->> stream
       line-seq
       (mapv (fn [line]
               (->> (split line #" +")
                    (mapv keyword-or-int))))))

;; part 1

(defn eval [registers expr]
  (if (keyword? expr) (registers expr) expr))

(defn default-instr [f]
  (fn [register & operands]
    (-> (apply f register operands)
        (update :ip inc))))

(defn update-register [f]
  (fn [registers r] (update registers r f)))

(defn jump-if [pred]
  (fn [registers value offset]
    (update registers :ip
            #(+ % (if (pred (eval registers value)) offset 1)))))

(defn copy [registers value r]
  (assoc registers r (eval registers value)))

(def instructions
  {:cpy (default-instr copy)
   :inc (default-instr (update-register inc))
   :dec (default-instr (update-register dec))
   :jnz (jump-if #(not= 0 %))})

(defn instruction [instr]
  (let [i (instructions instr)]
    (when-not i (throw (Exception. (str "no such instruction: " instr))))
    i))

(defn exec-instr [registers [instr & operands]]
  ;;(apply println registers instr operands)
  (apply (instruction instr) registers operands))

(def init-registers (constantly {:a 0 :b 0 :c 0 :d 0 :ip 0}))

(defn run-prog [prog registers]
  (->> (iterate
        (fn [registers]
          (exec-instr registers (get prog (registers :ip))))
        registers)
       (find-first #(not (<= 0 (% :ip) (dec (count prog)))))))

(defpart part1 [input]
  (-> (run-prog input (init-registers))
      :a))

;; part 2

(defpart part2 [input]
  (-> (run-prog input
                (-> (init-registers) (assoc :c 1)))
      :a))

;; tests

(deftest part1-test (part-test part1 42))
