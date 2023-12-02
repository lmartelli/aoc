(ns aoc-2023.day02
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn parse-set [set-str]
  (->> (str/split set-str #", *")
       (map #(let [[count color] (str/split % #" ")]
               [(keyword color) (parse-int count)]))
       (into {})))

(defn parse-game [line]
  (let [[game-str sets-str] (str/split line #": *")
        sets-strs (str/split sets-str #"; *")]
    {:id (parse-int (str/replace game-str "Game " ""))
     :sets (map parse-set sets-strs)}))

(defn puzzle-input [stream]
  (->> (puzzle-input-lines stream)
       (map parse-game)
       (map (juxt :id :sets))
       (into {})))

;; part 1

(defn count-min-per-color [sets]
  (apply merge-with max sets))

(defpart part1 [input]
  (->> input
       (map-vals count-min-per-color)
       (filter (fn [[id {:keys [red green blue]}]]
                   (and (<= red 12) (<= green 13) (<= blue 14))))
       keys
       (reduce +)))

;; part 2

(defn power [cube-set]
  (reduce * (vals cube-set)))

(defpart part2 [input]
  (->> input
       vals
       (map (comp power count-min-per-color))
       (reduce +)))

;; tests

(deftest part1-test (part-test part1 8))

(deftest part2-test (part-test part2 2286))
