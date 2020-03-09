(ns aoc-2017.day22
  (:require
   [aoc.core :refer :all]))

(defn center-index [coll]
  (/ (dec (count coll)) 2))

(defn parse-map [rows]
  {:map (array-2d-to-set \# rows)
   :center [(center-index (first rows)) (center-index rows)]})

(puzzle-input-lines parse-map)

;; part 1

(defn infected? [map pos]
  (contains? map pos))

(defn clean [state pos]
  (update state :infected disj pos))

(defn infect [state pos]
  (-> state
      (update :infected conj pos)
      (update :infection-count inc)))

(defn update-pos-and-dir [state new-dir]
  (-> state
      (assoc :dir new-dir)
      (update :pos add new-dir)))

(defn burst [{:keys [pos dir infected infection-count] :as state}]
  (if (infected? infected pos)
    (let [new-dir (rotate-right dir)]
      (-> state
          (update-pos-and-dir new-dir)
          (clean pos)))
    (let [new-dir (rotate-left dir)]
      (-> state
          (update-pos-and-dir new-dir)
          (infect pos)))))

(defn get-iteration [iterator start n]
  (nth (iterate iterator start) n))

(defn get-burst-iteration [infected-map start-pos n]
  (get-iteration
    burst
    {:pos start-pos
     :dir [0 -1]
     :infected infected-map
     :infection-count 0}
    n))

(defpart part1 [{:keys [center map]} n]
  (:infection-count
   (get-burst-iteration map center n)))

;; part 2

(defpart part2 [rules]
  nil)

;; tests
