(ns aoc-2015.day24
  (:require
   [aoc.core :refer :all]
   [clojure.math.combinatorics :refer [combinations]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> stream
       line-seq
       (mapv parse-int)))

;; part 1

(defn sum-is [sum]
  #(= sum (reduce + %)))

(defn- join
  "Lazily concatenates a collection of collections into a flat sequence,
  because Clojure's `apply concat` is insufficiently lazy."
  [colls]
  (lazy-seq
   (when-let [s (seq colls)]
     (concat (first s) (join (rest s))))))

(defn- mapjoin
  "Uses join to achieve lazier version of mapcat (on one collection)"
  [f coll]
  (join (map f coll)))

(defn- unchunk
  "Given a sequence that may have chunks, return a sequence that is 1-at-a-time
lazy with no chunks. Chunks are good for efficiency when the data items are
small, but when being processed via map, for example, a reference is kept to
every function result in the chunk until the entire chunk has been processed,
which increases the amount of memory in use that cannot be garbage
collected."
  [s]
  (lazy-seq
    (when (seq s)
      (cons (first s) (unchunk (rest s))))))

(defn subsets
  "All the subsets of items"
  ([items] (subsets items 0))
  ([items min-size]
   (mapjoin (fn [n] (combinations items n))
            (unchunk (range min-size (inc (count items)))))))

(defn optimal-qantum-entanglement [weights nb-groups]
  (let [sorted-weights (sort > weights)
        group-weight (-> (reduce + weights) (/ nb-groups))
        min-count (->> (reductions + sorted-weights)
                       (take-while #(< % group-weight))
                       count
                       inc)
        ;; subsets are ordered by size, so the first subset with the expected sum has the smallest possible size
        min-packages (->> (subsets sorted-weights min-count)
                          (filter (sum-is group-weight))
                          first
                          count)
        group-1-candidates (->> (combinations sorted-weights min-packages)
                                (filter (sum-is group-weight)))]
    (->> group-1-candidates
         (map #(reduce * %))
         (apply min))))

(defpart part1 [input]
  (optimal-qantum-entanglement input 3))

;; part 2

(defpart part2 [input]
    (optimal-qantum-entanglement input 4))

;; tests

(deftest optimal-qantum-entanglement-test
  (is (= 99 (optimal-qantum-entanglement [1 2 3 4 5 7 8 9 10 11]))))
