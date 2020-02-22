(ns aoc.prime)

(defn- sieve [s]
  (cons (first s)
        (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                 (rest s))))))

(def primes (sieve (iterate inc 2)))
