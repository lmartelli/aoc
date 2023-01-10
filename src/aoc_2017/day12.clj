(ns aoc-2017.day12
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (map (comp (juxt first rest) parse-ints))
       (into {})))

;; part 1

(defn transitive-closure [pipes root]
  (loop [closure #{root}, prev #{}]
    (if (= closure prev)
      closure
      (recur (reduce #(into %1 (pipes %2)) closure closure) closure))))

(defpart part1 [input]
  (count (transitive-closure input 0)))

;; part 2

(defn groups [pipes]
  (loop [groups []
         remaining pipes]
    (if (empty? remaining)
      groups
      (let [group (transitive-closure remaining (first (keys remaining)))]
        (recur (conj groups group)
               (apply dissoc remaining group))))))

(defpart part2 [input]
  (count (groups input)))

;; tests

(deftest part1-test (part-test part1 6))

(deftest part2-test (part-test part2 2))
