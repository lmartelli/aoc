(ns aoc-2017.knot-hash
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

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

(defn- knot-hash-1
  ([lengths]
   (first (reduce knot-hash-1 [(vec (range 256)) 0 0] lengths)))
  ([[v pos skip] length]
   [(sub-reverse v pos length) (mod (+ pos length skip) 256) (inc skip)]))

(defn- dense-hash [v]
  (map #(apply bit-xor %) (partition 16 v)))

(defn knot-hash [input]
  (->> (concat (map int input) [17 31 73 47 23])
       (repeat 64)
       (apply concat)
       knot-hash-1
       dense-hash))
