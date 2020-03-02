(ns aoc-2017.day13
  (:require
   [aoc.core :refer :all]))

(puzzle-input-split-lines #": " #(map parse-int %))

;; part 1

(defn period [range]
  (->> range (* 2) (- 2)))

(defn caught? [range time]
  (zero? (mod time (period range))))

(defn severity [firewalls delay]
  (->> firewalls
       (filter (fn [[depth range]] (caught? range (+ delay depth))))
       (map (fn [[depth range]] (* depth range)))
       (reduce +)))

(defpart part1 [input]
  (severity input 0))

;; part 2

(defn pass? [firewalls delay]
  (not-any? (fn [[depth range]] (caught? range (+ delay depth))) firewalls))

(defpart part2 [input]
  (find-first (fn [delay] (pass? input delay)) (range)))

;; tests
