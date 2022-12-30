(ns aoc-2018.day08
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (puzzle-input-string stream)
       parse-ints))

;; part 1

(defn visit-node [[nb-children nb-metadata & data] visitor]
  (let [[children-results meta-data-and-remaining-data]
        (reduce
          (fn [[results [_ nb-meta :as data]] i]
            (let [[result more-data] (visit-node data visitor)]
              [(conj results result) more-data]))
          [[] data]
          (range nb-children))
        [metadata remaining-data] (split-at nb-metadata meta-data-and-remaining-data)]
    [(visitor children-results metadata) remaining-data]))

(defn visit-tree [tree visitor]
  (-> (visit-node tree visitor)
      first))

(defn sum-metadata [tree]
  (visit-tree tree #(reduce + (concat %1 %2))))

(defpart part1 [tree]
  (sum-metadata tree))

;; part 2

(defpart part2 [tree]
  (visit-tree
    tree
    (fn [children-results metadata]
      (if (empty? children-results)
        (reduce + metadata)
        (->> (map #(get children-results (dec %) 0) metadata)
             (reduce +))))))

;; tests

(deftest sum-metadata-test
  (are [tree expected] (= expected (sum-metadata tree))
    [0 1 42] 42
    [0 2 3 5] 8
    [1 1 0 1 11 7] 18))

(deftest part1-test (part-test part1 138))

(deftest part2-test (part-test part2 66))
