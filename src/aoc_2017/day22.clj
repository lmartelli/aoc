(ns aoc-2017.day22
  (:require
   [aoc.core :refer :all]))

(defn center-index [coll]
  (/ (dec (count coll)) 2))

(defn parse-map [rows]
  {:map (array-2d-to-map (constantly true) {\# :infected \. :clean} rows)
   :center [(center-index (first rows)) (center-index rows)]})

(puzzle-input-lines parse-map)

;; part 1

(defn update-pos-and-dir [state new-dir]
  (-> state
      (assoc :dir new-dir)
      (update :pos add new-dir)))

(defn update-state [state pos new-state]
  (update state :infected-map assoc pos new-state))

(defn update-counter [state new-state]
  (if (= :infected new-state)
    (update state :infection-count inc)
    state))

(defn get-state [state pos]
  (get-in state [:infected-map pos] :clean))

(defn burst [{:keys [pos dir infected-map] :as state} change-direction next-state]
  (let [cur-state (get-state state pos)
        new-dir ((change-direction cur-state) dir)
        new-state (next-state cur-state)]
    (-> state
        (update-pos-and-dir new-dir)
        (update-state pos new-state)
        (update-counter new-state))))

(defn get-iteration [iterator start n]
  (nth (iterate iterator start) n))

(defn get-burst-iteration [infected-map start-pos n change-direction next-state]
  (get-iteration
    #(burst % change-direction next-state)
    {:pos start-pos
     :dir [0 -1]
     :infected-map infected-map
     :infection-count 0}
    n))

(defpart part1 [{:keys [center map]}]
  (:infection-count
   (get-burst-iteration
     map center 10000
     {:clean rotate-left
      :infected rotate-right}
     {:clean :infected
      :infected :clean})))

;; part 2


(defpart part2 [{:keys [center map]}]
  (:infection-count
   (get-burst-iteration
     map center 10000000
     {:clean rotate-left
      :infected rotate-right
      :weakened identity
      :flagged sub}
     {:clean :weakened
      :weakened :infected
      :infected :flagged
      :flagged :clean})))

;; tests

(def test-input
  (parse-map
    ["..#"
     "#.."
     "..."]))
