(ns aoc-2021.day18
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (puzzle-input-parse-lines stream load-string)))

;; part 1

(defn left [n] (get n 0))
(defn right [n] (get n 1))

(defn depth [n]
  (if (number? n)
    0
    (let [[l r] n]
      (inc (max (depth l) (depth r))))))

(defn find-path-to-explode
  ([n] (find-path-to-explode n []))
  ([n path]
   (cond
     (number? n) nil
     (= (count path) 4) [path n]
     :else (or (find-path-to-explode (left n) (conj path 0))
               (find-path-to-explode (right n) (conj path 1))))))

(defn path-to-number-left-of [n path]
  (if-let [parent (->> (reverse path)
                       (drop-while #{0})
                       reverse
                       nil-if-empty)]
    (loop [res (-> (vec parent) pop (conj 0))
           n (get-in n res)]
      (if (number? n)
        res
        (recur (conj res 1) (right n))))
    nil))

(defn path-to-number-right-of [n path]
  (if-let [parent (->> (reverse path)
                       (drop-while #{1})
                       reverse
                       nil-if-empty)]
    (loop [res (-> (vec parent) pop (conj 1))
           n (get-in n res)]
      (if (number? n)
        res
        (recur (conj res 0) (left n))))
    nil))

(defn explode [n]
  (if-let [[path [exploded-l exploded-r]] (find-path-to-explode n)]
    (-> (assoc-in n path 0)
        (#(if-let [left-path (path-to-number-left-of % path)]
            (update-in % left-path + exploded-l)
            %))
        (#(if-let [right-path (path-to-number-right-of % path)]
            (update-in % right-path + exploded-r)
            %)))))

(defn find-path-to-split
  ([n] (find-path-to-split n []))
  ([n path]
   (if (number? n)
     (if (> n 9)
       [path n]
       nil)
     (or (find-path-to-split (left n) (conj path 0))
         (find-path-to-split (right n) (conj path 1))))))

(defn round-up [n] (int (Math/ceil n)))
(defn round-down [n] (int (Math/floor n)))

(defn split [n]
  (if-let [[path splitted] (find-path-to-split n)]
    (assoc-in n path [(round-down (/ splitted 2)) (round-up (/ splitted 2))])))

(defn snail-reduce [n]
  (if-let [exploded (explode n)]
    (snail-reduce exploded)
    (if-let [splitted (split n)]
      (snail-reduce splitted)
      n)))

(defn snail-add [x y]
  (snail-reduce [x,y]))

(defn magnitude [n]
  (if (number? n)
    n
    (let [[l r] n]
      (+ (* 3 (magnitude l)) (* 2 (magnitude r))))))

(defpart part1 [numbers]
  (-> (reduce snail-add numbers)
      magnitude))

;; part 2

(defpart part2 [numbers]
  (->> (for [x numbers
             y numbers
             :when (not= x y)]
         (magnitude (snail-add x y)))
       (apply max)))

;; tests

(def test-data (puzzle-input (test-input *ns*)))

(deftest magnitude-test
  (are [number expected] (= expected (magnitude number))
    [9,1] 29
    [[9,1],[1,9]] 129    
    [[1,2],[[3,4],5]] 143
    [[[[0,7],4],[[7,8],[6,0]]],[8,1]] 1384
    [[[[1,1],[2,2]],[3,3]],[4,4]] 445
    [[[[3,0],[5,3]],[4,4]],[5,5]] 791
    [[[[5,0],[7,4]],[5,5]],[6,6]] 1137
    [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]] 3488))

(deftest find-path-to-explode-test
  (are [n expected] (= expected (find-path-to-explode n))
    [[[[[9,8],1],2],3],4] [0 0 0 0]
    [7,[6,[5,[4,[3,2]]]]] [1 1 1 1]
    [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]] [0 1 1 1]
    [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] [1 1 1 1]))

(deftest explode-test
  (are [n expected] (= expected (explode n))
    [[[[[9,8],1],2],3],4] [[[[0,9],2],3],4]
    [7,[6,[5,[4,[3,2]]]]] [7,[6,[5,[7,0]]]]
    [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]] [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
    [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] [[3,[2,[8,0]]],[9,[5,[7,0]]]]))

(deftest split-test
  (are [n expected] (= expected (split n))
    [[[[0,7],4],[15,[0,13]]],[1,1]] [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
    [[[[0,7],4],[[7,8],[0,13]]],[1,1]] [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]))

(deftest snail-reduce-test
  (are [n expected] (= expected (snail-reduce n))
    [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]] [[[[0,7],4],[[7,8],[6,0]]],[8,1]]))

(deftest part1-test
  (is (= 4140 (part1 test-data))))

(deftest part2-test
  (is (= 3993 (part2 test-data))))

