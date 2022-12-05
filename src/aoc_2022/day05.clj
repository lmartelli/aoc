(ns aoc-2022.day05
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]
   [clojure.string :refer [split]]))

(defn get-stack [lines n]
  (let [x (-> (* n 4) (+ 1))]
    (into []
          (for [y (range (- (count lines) 2) -1 -1)
                :let [crate (get-in lines [y x])]
                :while (not (= \space crate))]
            crate))))

(defn parse-stacks [lines]
  (let [height (dec (count lines))
        nb-stacks (-> (count (first lines)) inc (/ 4))]
    (mapv #(get-stack lines %) (range nb-stacks))))

(defn parse-move [move-line]
  (let [[qty from to] (->> (re-seq #"\d+" move-line)
                           (map parse-int))]
    {:qty qty :from (dec from) :to (dec to)}))

(defn puzzle-input [stream]
  (let [[stacks moves] (->> (line-seq stream)
                            (split-seq empty?))]
    {:stacks (parse-stacks stacks)
     :moves (map parse-move moves)}))

;; part 1

(defn do-move [stacks {:keys [qty from to] :as move}]
  (if (zero? qty)
    stacks
    (recur
     (-> stacks
         (update from pop)
         (update to conj (peek (stacks from))))
     (update move :qty dec))))

(defn answer [{:keys [stacks moves]} move-fn]
  (->> (reduce move-fn stacks moves)
       (map peek)
       (apply str)))

(defpart part1 [input]
  (answer input do-move))

;; part 2

(defn do-move-2 [stacks {:keys [qty from to] :as move}]
  (let [from-stack (stacks from)
        from-pos (- (count from-stack) qty)]
    (-> stacks
        (update from #(subvec % 0 from-pos))
        (update to into (subvec from-stack from-pos)))))

(defpart part2 [input]
  (answer input do-move-2))

;; tests

(deftest puzzle-input-test
  (is (= {:stacks [[\Z \N] [\M \C \D] [\P]]
          :moves [{:qty 1 :from 1 :to 0}
                  {:qty 3 :from 0 :to 2}
                  {:qty 2 :from 1 :to 0}
                  {:qty 1 :from 0 :to 1}]}
         (puzzle-input (test-input)))))

(deftest do-move-test
  (are [stacks move expected] (= expected (do-move stacks move))
    [[1] []] {:qty 0 :from 0 :to 1} [[1] []]
    [[1] []] {:qty 1 :from 0 :to 1} [[] [1]]
    [[1 2] []] {:qty 1 :from 0 :to 1} [[1] [2]]
    [[1 2] []] {:qty 2 :from 0 :to 1} [[] [2 1]]))

(deftest part1-test (part-test part1 "CMZ"))

(deftest do-move-2-test
  (are [stacks move expected] (= expected (do-move-2 stacks move))
    [[1] []] {:qty 0 :from 0 :to 1} [[1] []]
    [[1] []] {:qty 1 :from 0 :to 1} [[] [1]]
    [[1 2] []] {:qty 1 :from 0 :to 1} [[1] [2]]
    [[1 2] []] {:qty 2 :from 0 :to 1} [[] [1 2]]))

(deftest part2-test (part-test part2 "MCD"))
