(ns aoc-2016.day04
  (:require
   [clojure.string :refer [split join]]
   [aoc.core :refer :all]))

(puzzle-input-parse-lines
 (fn [line]
   (let [[_ name id checksum] (re-matches #"(.*)-(\d+)\[(.*)\]" line)]
        {:name (split name #"-")
         :sector (parse-int id)
         :checksum checksum})))

;; part 1

(defn checksum [name]
  (->> name
       (apply concat)
       frequencies
       (sort-by (fn [[char freq]] (vector (- freq) char)))
       (map first)
       (take 5)
       (apply str)))

(defn remove-decoy [input]
  (filter #(= (checksum (% :name)) (% :checksum) ) input))

(defpart part1 [input]
  (->> input
       remove-decoy
       (map :sector)
       (reduce +)))

;; part 2

(defn rotate [word n]
  (->> word
       (map #(- (int %) (int \a)))
       (map #(mod (+ % n) 26))
       (map #(char (+ % (int \a))))
       (apply str)))

(defn decrypt [room]
  (->> (room :name)
       (map #(rotate % (room :sector)))
       (join " ")))

(defpart part2 [input]
  (->> input
       (map #(assoc % :name (decrypt %)))
       (filter #(re-matches #"^northpole.*storage$" (% :name)))
       first
       :sector))

;; tests
