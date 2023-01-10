(ns aoc-2017.day10
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (-> (puzzle-input-string stream)
      parse-ints))

;; part 1

(defn sub-list [v pos length]
  (let [end (+ pos length)]
    (if (<= end (count v))
      (subvec v pos end)
      (concat (subvec v pos) (subvec v 0 (- end (count v)))))))

(defn replace [v pos other]
  (reduce
   (fn [v [pos x]]
     (assoc v pos x))
   v
   (map vector (->> (cycle (range (count v))) (drop pos)) other)))

(defn sub-reverse [v pos length]
  (replace v pos (reverse (sub-list v pos length))))

(defn knot-hash
  ([lengths]
   (first (reduce knot-hash [(vec (range 256)) 0 0] lengths)))
  ([[v pos skip] length]
   [(sub-reverse v pos length) (mod (+ pos length skip) 256) (inc skip)]))

(defpart part1 [input]
  (let [h (knot-hash input)]
    (* (first h) (second h))))

;; part 2

(defn dense-hash [v]
  (map #(apply bit-xor %) (partition 16 v)))

(defn knot-hash-2 [input]
  (-> (knot-hash (apply concat (repeat 64 (concat input [17 31 73 47 23]))))
      dense-hash))

(defn hex [ints]
  (apply str (map #(format "%02x" %) ints)))

(defpart part2 [input]
  (let [s (if (string? input) input (str/join "," input))]
    (hex (knot-hash-2 (map int s)))))

;; tests

(deftest sub-list-test
  (are [v pos len expected] (= expected (sub-list v pos len))
    [0 1 2 3 4] 0 5 [0 1 2 3 4]
    [0 1 2 3 4] 1 5 [1 2 3 4 0]))

(deftest replace-test
  (are [v pos other expected] (= expected (replace v pos other))
    [0 1 2 3 4] 0 [5 6 7] [5 6 7 3 4]
    [0 1 2 3 4] 4 [5 6 7] [6 7 2 3 5]))
