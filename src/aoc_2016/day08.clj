(ns aoc-2016.day08
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines
   stream
   (fn [l]
     (let [[_ cmd & params] (re-matches #"(rect|rotate (?:row|column)) [^\d]*(\d+)[^\d]*(\d+)" l)]
       [cmd (mapv parse-int params)]))))

;; part 1

(defn rotate [v n]
  (mapv (fn [pixel index] (get-wrap v (- index n))) v (range)))

(defn rotate-row [display row n]
  (update display row #(rotate % n)))

(defn get-col [display n]
  (mapv #(get % n) display))

(defn update-col [display col f]
  (mapv
   (fn [row new-cell]
     (assoc row col new-cell))
   display
   (f (get-col display col))))

(defn rotate-column [display col n]
  (update-col display col #(rotate % n)))

(defn make-rect [width height val]
  (vec
   (repeat height
           (vec (repeat width val )))))

(defn fill-vect [v length val]
  (into (vec (repeat length val)) (subvec v length)))

(defn fill-rect [display width height val]
  (mapv
   (fn [row index]
     (if (< index height)
       (fill-vect row width val)
       row))
   display (range)))

(def display (make-rect 50 6 \space))

(defn exec-cmd [display [cmd [p1 p2]]]
  (case cmd
    "rect" (fill-rect display p1 p2 \#)
    "rotate row" (rotate-row display p1 p2)
    "rotate column" (rotate-column display p1 p2)))

(defpart part1 [input]
  (->> input
       (reduce exec-cmd display)
       (apply concat)
       (filter #(= \# %))
       count))

;; part 2

(defn show [display]
  (doall
   (map #(println %2 (apply str %1)) display (range)))
  nil)

(defpart part2 [input]
  (->> input
       (reduce exec-cmd display)
       show))

;; tests
