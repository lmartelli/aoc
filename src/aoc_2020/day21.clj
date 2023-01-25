(ns aoc-2020.day21
  (:require
   [aoc.core :refer :all]
   [aoc.algo :as algo]
   [clojure.string :as str]
   [clojure.set :refer :all :exclude [index]]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (map #(->> (re-seq #"[a-z]+" %)
             (split-seq (eq "contains"))
             (zipmap [:ingredients :allergens]))
       lines))

;; part 1

(defn get-allergens [input]
  (->> input
       (mapcat (fn [{:keys [ingredients allergens]}]
                 (map #(vector % (set ingredients)) allergens)))
       multimap
       (map-vals #(apply intersection %))
       algo/resolve-bijection))

(defpart part1 [input]
  (let [allergens (map-invert (get-allergens input))]
    (->> input
         (mapcat :ingredients)
         (remove allergens)
         count)))

;; part 2

(defpart part2 [input]
  (->> (get-allergens input)
       (into (sorted-map))
       vals
       (str/join ",")))

;; tests

(def data
  ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
   "trh fvjkl sbzzf mxmxvkd (contains dairy)"
   "sqjhc fvjkl (contains soy)"
   "sqjhc mxmxvkd sbzzf (contains fish)"])

(deftest part1-test
  (test-with-lines part1 data 5))

(deftest part2-test
  (test-with-lines part2 data "mxmxvkd,sqjhc,fvjkl"))
