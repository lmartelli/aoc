(ns aoc.prime
  (:require 
   [aoc.core :refer :all]))

(defn- sieve [s]
  (cons (first s)
        (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                 (rest s))))))

(def primes (sieve (iterate inc 2)))

(defn divisible? [n m]
  (zero? (rem n m)))

(defn prime-factors [n]
  (if (< n 2)
    {}
    (frequencies
     (loop [factors []
            current n]
       (if-let [factor (find-first #(divisible? current %)
                                   (take-while #(<= (* % % ) current) primes))]
         (recur (conj factors factor) (quot current factor))
         (conj factors current))))))
