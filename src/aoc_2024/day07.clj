(ns aoc-2024.day07
  (:require
   [aoc.core :refer :all]
   [clojure.math.combinatorics :as combo]
   [clojure.test :refer :all]))

(defn parse-equation [line]
  (let [[r & ns] (parse-ints line)]
    {:result r
     :numbers ns}))

(defn puzzle-input [stream]
  (->> (puzzle-input-lines stream)
       (map parse-equation)))

;; part 1

(defn is-true-eqn? [{:keys [result] [n0 & ns] :numbers} operators]
  (= result
     (reduce
       (fn [acc [n op]]
         (op acc n))
       n0
       (map vector ns operators))))

(defn can-be-true? [eqn operators]
  (some
    #(is-true-eqn? eqn %)
    (combo/selections operators (dec (count (eqn :numbers))))))

(defn total-calibration [eqns operators]
  (->> eqns
       (filter #(can-be-true? % operators))
       (map :result)
       (reduce +)))

(defpart part1 [eqns]
  (total-calibration eqns [* +]))

;; part 2

(defn || [a b]
  (parse-int (str a b)))

(defpart part2 [eqns]
  (total-calibration eqns [* + ||]))

;; tests

(deftest part1-test (part-test part1 3749))

(deftest can-be-true-test
  (let [parse-eqn (fn [[r & ns]] {:result r :numbers ns})]
    (are [eqn] (can-be-true? (parse-eqn eqn) [+ *])
      [190 10 19]
      [3267 81 40 27]
      [292 11 6 16 20])
    (are [eqn] (can-be-true? (parse-eqn eqn) [+ * ||])
      [190 10 19]
      [156 15 6]
      [3267 81 40 27]
      [7290 6 8 6 15]
      [192 17 8 14]
      [292 11 6 16 20])
    (are [eqn] (not (can-be-true? (parse-eqn eqn) [+ *]))
      [83 17 5]
      [156 15 6]
      [161011 16 10 13])))

;;(deftest part1-test (test-with-lines part1 [""] nil))

(deftest part2-test (part-test part2 11387))

;;(deftest part2-test (test-with-lines part2 [""] nil))
