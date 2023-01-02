(ns aoc-2018.day14
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-string stream))

;; part 1

(defn digits [n]
  (if (< n 10)
    [n]
    [(quot n 10) (mod n 10)]))

(defn make-new-recipes [recipes score-1 score-2]
  (apply conj recipes (digits (+ score-1 score-2))))

(defn round [[recipes elf-1 elf-2]]
  (let [score-1 (recipes elf-1)
        score-2 (recipes elf-2)
        new-recipes (make-new-recipes recipes score-1 score-2)
        new-count (count new-recipes)]
    [new-recipes
     (mod (+ elf-1 score-1 1) new-count)
     (mod (+ elf-2 score-2 1) new-count)]))

(defn simul []
  (iterate round [[3 7] 0 1]))

(defpart part1 [input]
  (let [n (parse-int input)]
    (let [recipes (->> (simul)
                       (find-first (fn [[recipes]] (>= (count recipes) (+ n 10))))
                       first)
          last-10 (subvec recipes n (+ n 10))]
      (str/join last-10))))

;; part 2

(defpart part2 [input]
  (let [n (count input)
        target (digit-seq input)]
    (->> (simul)
         (keep (fn [[recipes]]
                 (let [nb-recipes (count recipes)]
                   (when (> nb-recipes (inc n))
                     (cond
                       (= target (subvec recipes (- nb-recipes n 1) (dec nb-recipes))) (- nb-recipes n 1)
                       (= target (subvec recipes (- nb-recipes n) nb-recipes)) (- nb-recipes n))))))
         first)))

;; tests

(deftest digits-test
  (are [n expected] (= expected (digits n))
    0 [0]
    1 [1]
    2 [2]
    3 [3]
    4 [4]
    5 [5]
    6 [6]
    7 [7]
    8 [8]
    9 [9]
    10 [1 0]
    11 [1 1]
    12 [1 2]
    13 [1 3]
    14 [1 4]
    15 [1 5]
    16 [1 6]
    17 [1 7]))

(deftest part1-test
  (are [n expected] (= expected (part1 n))
    "5" "0124515891"
    "9" "5158916779"
    "18" "9251071085"
    "2018" "5941429882"))

(deftest part2-test
  (are [n expected] (= expected (part2 n))
    "01245" 5
    "51589" 9
    "92510" 18
    "59414" 2018))
