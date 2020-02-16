(ns aoc-2015.day13
  (:require
   [aoc.core :refer :all]
   [clojure.math.combinatorics :refer [permutations]]
   [clojure.test :refer :all]))

(puzzle-input-parse-lines
 (fn [line]
   (let [[_ person gain-or-lose points neighbour] (re-matches #"([^ ]*).*(gain|lose) (\d+).* (.*)\." line)]
     [[person neighbour]
      (* (parse-int points)
         (if (= "gain" gain-or-lose) 1 -1))]))
 #(into {} %))

;; part 1

(defn attendees [rules]
  (set (map #(first (key %)) rules)))

(defn happyness [rules person & neighbours]
  (->> neighbours
       (map #(rules [person %]))
       (reduce +)))

(defn sum-happyness [rules ^clojure.lang.PersistentVector permutation]
  (->> permutation
       (map-indexed
        (fn [i person]
          (happyness rules person (get-wrap permutation (inc i)) (get-wrap permutation (dec i)))))
       (reduce +)))

(defn max-total-happyness [rules]
  (->> (permutations (attendees rules))
       (map #(sum-happyness rules (vec %)))
       (apply max)))

(defpart part1 [input]
  (max-total-happyness input))

;; part 2

(defn add-apathetic [rules name]
  (into rules (mapcat (fn [attendee] [[[attendee name] 0] [[name attendee] 0]]) (attendees rules))))

(defpart part2 [input]
  (max-total-happyness (add-apathetic input "me")))

;; tests

(deftest happyness-test
  (let [rules {[:a :b] 10, [:a :c]  -3, [:a :d]  15
               [:b :a] -5, [:b :c]   7, [:b :d]  11
               [:c :a]  0, [:c :b]  -1, [:c :d]  25
               [:d :a] -5, [:d :b] -17, [:d :c] -34}]
    (are [person n1 n2 expected] (= expected
                                    (happyness rules person n1 n2)
                                    (happyness rules person n2 n1))
      :a :b :c 7
      :a :b :d 25
      :a :c :d 12
      :d :a :b -22
      :d :a :c -39)))

(deftest sum-happyness-test
  (let [rules {[:a :b] 10, [:a :c]  -3, [:a :d]  15
               [:b :a] -5, [:b :c]   7, [:b :d]  11
               [:c :a]  0, [:c :b]  -1, [:c :d]  25
               [:d :a] -5, [:d :b] -17, [:d :c] -34}]
    (are [permutation expected] (= expected
                                   (sum-happyness rules permutation)
                                   (sum-happyness rules (vec (reverse permutation))))
      [:a :b :c :d] (+ 10 15 -5 7 -1 25 -5 -34))))


