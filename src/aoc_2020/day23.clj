(ns aoc-2020.day23
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(def-input-parser [[line-1]]
  (digit-vec line-1))

;; part 1

(defn move [cups]
  (let [n (count cups)
        moved (->> cups (drop 1) (take 3))
        dest-candidates (apply disj (set cups) moved)
        dest (->> (sort > dest-candidates)
                  cycle
                  (drop-until (eq (first cups)))
                  first)
        [begin end] (split-after (eq dest) (drop 4 (cycle cups)))]
    (->> (concat begin moved end)
         (take n)
         (into [])
         )))

(defn cups-after [cups n]
  (->> (cycle cups)
       (drop-until (eq 1))
       (take (dec (count cups)))))

(defpart part1 [cups]
  (-> (iterate move cups)
      (nth 100)
      (cups-after 1)
      str/join))

;; part 2

(defpart part2 [input]
  nil)

;; tests

(deftest move-test
  (let [cups-seq
        [[3 8 9 1 2 5 4 6 7]
         [2 8 9 1 5 4 6 7 3]
         [5 4 6 7 8 9 1 3 2]
         [8 9 1 3 4 6 7 2 5]
         [4 6 7 9 1 3 2 5 8]
         [1 3 6 7 9 2 5 8 4]
         [9 3 6 7 2 5 8 4 1]
         [2 5 8 3 6 7 4 1 9]
         [6 7 4 1 5 8 3 9 2]
         [5 7 4 1 8 3 9 2 6]]]
    (doseq [[n n+1] (partition 2 1 cups-seq)]
      (is (= n+1 (move n)) (str n " → " n+1)))
    )  
  )

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
  (test-with-lines
    part2
    [""]
    nil))
