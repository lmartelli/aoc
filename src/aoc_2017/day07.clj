(ns aoc-2017.day07
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.set :refer [difference]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (re-parse-lines
         #"(\w+) \((\d+)\)(?: -> (.*))?"
         #(if %3
            [%1 {:weight (parse-int %2), :children (split %3 #", *")}]
            [%1 {:weight (parse-int %2)}]))
       (into {})))

;; part 1

(defn find-roots [tower]
  (difference
   (set (keys tower))
   (->> (vals tower)
        (mapcat :children)
        set)))

(defpart part1 [input]
  (->> input
       find-roots
       first))

;; part 2

(defn disc-weight [tower name]
  (let [{:keys [:weight :children]} (tower name)]
    (apply + weight (map (partial disc-weight tower) children))))

(defn find-unbalanced [tower names]
  (let [discs-by-weight (group-by #(disc-weight tower %) names)
        [[[unbalanced-weight [unbalanced-name]]] [[balanced-weight _]]] (group-by-pred #(= 1 (count (val %))) discs-by-weight)]
    (if unbalanced-name
      [unbalanced-name (- balanced-weight unbalanced-weight)]
      nil)))

(defpart part2 [tower]
  (let [root (part1 tower)
        [unbalanced-name delta] (find-unbalanced tower (:children (tower root)))]
    (loop [current (tower unbalanced-name)]
      (let [[unbalanced-name _] (find-unbalanced tower (current :children))]
        (if (nil? unbalanced-name)
          (+ (current :weight) delta)
          (recur (tower unbalanced-name)))))))

;; tests

(deftest part1-test (part-test part1 "tknk"))
