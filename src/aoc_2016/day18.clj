(ns aoc-2016.day18
  (:require
   [aoc.core :refer :all]
   [aoc.algo :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-string stream))

;; part 1

(defn next-row [^booleans row]
  (let [len (alength row)
        new-row (boolean-array len)]
    (aset-boolean new-row 0 true)
    (aset-boolean new-row (dec len) true)
    (loop [i 1]
      (if (= i (unchecked-dec-int len))
        new-row
        (do
          (aset-boolean new-row i (= (aget row (unchecked-dec-int i)) (aget row (unchecked-inc-int i))))
          (recur (unchecked-inc-int i)))))))

(defn boolean-row [row]
  (->> (map {\^ false \. true} (str \. row \.))
       (into-array Boolean/TYPE)))

(defn rows [initial-row]
  (iterate next-row (boolean-row initial-row)))

(defn count-safe-tiles
  ([^booleans row]
   (loop [i (-> (alength row) (- 2)),
          nb-safe 0]
     (if (= i 0)
       nb-safe
       (if (aget row i)
         (recur (unchecked-dec-int i) (unchecked-inc-int nb-safe))
         (recur (unchecked-dec-int i) nb-safe)))))
  ([initial-row n]
   (->> (rows initial-row)
        (take n)
        (map count-safe-tiles)
        (reduce +))))

(defpart part1 [input]
  (count-safe-tiles input 40))

;; part 2

(defpart part2 [input]
  (count-safe-tiles input 400000))

;; tests

(deftest next-row-test
  (are [row expected] (= (boolean-row expected) (next-row (boolean-row row)))
    "..^^." ".^^^^"
    ".^^^^" "^^..^"))

(deftest count-safe-tiles-test
  (is ( = 38 (count-safe-tiles ".^^.^.^^^^" 10))))
