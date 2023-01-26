(ns aoc-2020.day23
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(def-input-parser [[line-1]]
  (digit-vec line-1))

;; part 1

(defn remove-cups [^ints next-cups ^long after n]
  (loop [cur after
         removed-cups []
         n n]
    (if (zero? n)
      (do
        (aset-int next-cups after (aget next-cups cur))
        removed-cups)
      (let [next (aget next-cups cur)]
        (recur
          next
          (conj removed-cups next)
          (dec n))))))

(defn insert-cups [^ints next-cups after cups]
  (aset-int next-cups (last cups) (aget next-cups after))
  (aset-int next-cups after (first cups)))

(defn move [^ints next-cups ^long current]
  (let [moved (remove-cups next-cups current 3)
        dest (->> current
                  (iterate #(if (= 1 %) (dec (alength next-cups)) (dec %)))
                  (drop 1)
                  (find-first (complement (set moved))))]
    (insert-cups next-cups dest moved)
    (aget next-cups current)))

(defn after-nth-move [^ints next-cups current n]
  (loop [current current
         n n]
    (if (zero? n)
      current
      (recur (move next-cups current) (dec n)))))

(defn ->seq [^ints next-cups start]
  (loop [cups [start]
         cur (aget next-cups start)]
    (if (= cur start)
      cups
      (recur (conj cups cur) (aget next-cups cur)))))

(defn cups-after [next-cups n]
  (rest (->seq next-cups n)))

(defn init-next-cups
  ([cups]
   (->> (cycle cups)
        (partition 2 1)
        (take (count cups))
        (reduce
          (fn [next-cups [cup next-cup]]
            (aset-int next-cups cup next-cup)
            next-cups)
          (int-array (inc (apply max cups))))))
  ([cups n]
   (->> (concat cups (rest (iterate inc (count cups))))
        (take n)
        (cycle)
        (partition 2 1)
        (take n)
        (reduce
          (fn [next-cups [cup next-cup]]
            (aset-int next-cups cup next-cup)
            next-cups)
          (int-array (inc n))))))

(defpart part1 [cups]
  (let [next-cups (init-next-cups cups)]
    (after-nth-move next-cups (first cups) 100)
    (str/join (cups-after next-cups 1))))

;; part 2

(defpart part2 [cups]
  (let [next-cups (init-next-cups cups 1000000)]
    (after-nth-move next-cups (first cups) 10000000)
    (->> (cups-after next-cups 1)
         (take 2)
         (reduce *))))

;; tests

(deftest move-test
  (let [cups-seq [[3 8 9 1 2 5 4 6 7]
                  [2 8 9 1 5 4 6 7 3]
                  [5 4 6 7 8 9 1 3 2]
                  [8 9 1 3 4 6 7 2 5]
                  [4 6 7 9 1 3 2 5 8]
                  [1 3 6 7 9 2 5 8 4]
                  [9 3 6 7 2 5 8 4 1]
                  [2 5 8 3 6 7 4 1 9]
                  [6 7 4 1 5 8 3 9 2]
                  [5 7 4 1 8 3 9 2 6]]
        ]
    (doseq [n (range (count cups-seq))]
      (is (= (cups-seq n)
             (let [next-cups (init-next-cups (first cups-seq))
                   current (first (first cups-seq))
                   current-after-nth-move (after-nth-move next-cups current n)]
               (->seq next-cups current-after-nth-move)))
          (str "after " n " move(s)")))
      ))

(deftest move-test
  (let [cups-seq
        [[1 3 6 7 9 2 5 8 4]
         [9 3 6 7 2 5 8 4 1]]]
    (doseq [[n n+1] (partition 2 1 cups-seq)]
      (is (= n+1 (move n)) (str n " → " n+1)))
    )  
  )

(deftest part1-test
  (test-with-lines part1 ["389125467"] "67384529"))

(deftest part2-test
  (test-with-lines part2 ["389125467"] 149245887792))
