(ns aoc-2016.day20
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (map (comp vec parse-pos-ints))))

;; part 1

(defpart part1 [input]
  (reduce
    (fn [res [low high]]
      (if (< res low)
        (reduced res)
        (max res (inc high))))
    0
    (sort compare input)))

;; part 2

(defn count-allowed-ips [black-list max-ip]
  (-> (reduce
        (fn [[total ip] [low high]]
          [(if (< ip low) (+ total (- low ip)) total)
           (max (inc high) ip)])
        [0 0]
        (sort compare (conj black-list [(inc max-ip) (inc max-ip)])))
      first))

(defpart part2 [input]
  (count-allowed-ips input 4294967295))

;; tests

(deftest part1-test (part-test part1 3))

(deftest count-allowed-ips-test
  (is (= 2 (count-allowed-ips (test-data) 9))))
