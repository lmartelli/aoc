(ns aoc-2022.day08
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.math.combinatorics :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> stream
       line-seq
       (mapv (comp #(into [] %) digit-seq))))

;; part 1

(defn traversals
  "Generates 4 sequences of rows/columns of [pos tree-height] for the different
  traversing orders: left->right, right->left, top->bottom and bottom->top"
  [tree-map]
  (let [asc (range (count tree-map))
        dsc (reverse asc)
        tree-height #(get-in tree-map %)]
    (for [x-range [asc dsc]
          mk-pos [vector #(vector %2 %1)]]
      (map (fn [y]
             (map (comp (juxt identity tree-height)
                        #(mk-pos % y))
                  x-range))
           asc))
    ))

(defn visible-in-row [row]
  (-> (reduce
       (fn [[visible-trees max-height] [pos height]]
         [(if (> height max-height)
            (conj visible-trees pos)
            visible-trees)
          (max max-height height)])
       [[] -1]
       row)
      first))

(defn count-visible-trees [tree-map]
  (->> (traversals tree-map)
       (mapcat (fn [traversal]
                 (mapcat #(visible-in-row %) traversal)))
       (into #{})
       count))

(defpart part1 [tree-map]
  (count-visible-trees tree-map))

;; part 2

(defn update-scenic-scores [scenic-scores from-trees to-pos]
  (reduce
   (fn [scenic-scores [pos height]]
     (update-in scenic-scores pos (fnil * 1) (s2/manatthan-dist pos to-pos)))
   scenic-scores
   from-trees))

(defn update-scenic-scores-for-row [scenic-scores row]
  (loop [[[tree-pos tree-height :as tree] & more :as row] row
         untreated '()
         previous-height ##Inf
         scenic-scores scenic-scores]
   (if-not tree
     (update-scenic-scores scenic-scores untreated (first (first untreated)))
     (if (< tree-height previous-height)
        (recur (rest row)
               (conj untreated tree)
               tree-height
               scenic-scores)
        (let [[treatable new-untreated] (split-with (fn [[pos height]] (<= height tree-height)) untreated)]
          (recur (rest row)
                 (conj new-untreated tree)
                 tree-height
                 (update-scenic-scores scenic-scores treatable tree-pos)))))))

(defn update-scenic-scores-for-direction [scenic-scores traversal]
  (reduce update-scenic-scores-for-row
          scenic-scores
          traversal))

(defn init-scores [tree-map]
  (into [] (repeat (count tree-map) (into [] (repeat (count (first tree-map)) 1)))))

(defn scenic-scores [tree-map]
  (reduce
   update-scenic-scores-for-direction
   (init-scores tree-map)
   (traversals tree-map)))

(let [tree-map (test-data)]
  (reductions
   update-scenic-scores-for-direction
   (init-scores tree-map)
   (traversals tree-map)))

(defpart part2 [tree-map]
  (->> (scenic-scores tree-map)
         (apply concat)
         (apply max)))

;; tests

(deftest visible-in-row-test
  (are [row visible-tree-positions] (= visible-tree-positions (visible-in-row row))
    [[:a 0]] [:a]
    [[:a 3] [:b 2]] [:a]
    [[:a 3] [:b 3]] [:a]
    [[:a 3] [:b 4]] [:a :b]
    [[:a 3] [:b 4] [:c 4]] [:a :b]
    [[:a 1] [:b 2] [:c 3]] [:a :b :c]))

(deftest count-visible-trees-test
  (are [tree-map expected] (= expected (count-visible-trees tree-map))
    [[0 0 0]
     [0 1 0]
     [0 0 0]] 9
    [[1 1 1]
     [1 0 1]
     [1 1 1]] 8
    [[1 2 3 4]
     [7 6 5 4]
     [4 3 4 5]
     [6 7 8 9]]  14
    ))

(deftest part1-test (part-test part1 21))

(deftest update-scenic-scores-for-row-test
  (are [row viewing-distances] (= viewing-distances (update-scenic-scores-for-row [(into [] (repeat (count row) 1))] row))
    ;[] [[]]
    [[[0 0] 0]] [[0]]
    [[[0 0] 1]
     [[0 1] 2]] [[1 0]]
    [[[0 0] 1]
     [[0 1] 0]] [[1 0]]
    [[[0 0] 1]
     [[0 1] 0]
     [[0 2] 1]] [[2 1 0]]
    [[[0 0] 3]
     [[0 1] 2]
     [[0 2] 1]] [[2 1 0]]
    [[[0 0] 5]
     [[0 1] 5]
     [[0 2] 3]] [[1 1 0]]
    [[[0 0] 4]
     [[0 1] 3]
     [[0 2] 1]
     [[0 3] 2]
     [[0 4] 0]
     [[0 5] 5]
     [[0 6] 1]
     [[0 7] 2]
     [[0 8] 1]
     [[0 9] 4]
     [[0 10] 3]
     [[0 11] 6]
     [[0 12] 2]
     [[0 13] 1]] [[5 4 1 2 1 6 1 2 1 2 1 2 1 0]]))

(deftest part2-test (part-test part2 8))
