(ns aoc-2020.day11
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (->> (s2/parse-2d-map-positions lines \L)
       (into #{})))

;; part 1

(defn printable-state [state]
  (s2/print-to-lines  state {:empty \L :occupied \#}))

(defn print-state [state]
  (run! println (printable-state state)))

(defn adjacent-occupied [state pos]
  (->> (s2/all-neighbours pos)
       (filter #(= :occupied (state %)))))

(defn count-gte [seq n]
  (not-empty (drop (dec n) seq)))

(defn next-state
  ([state neighbours occupied-threshold]
   (map-vals-kv
     (fn [pos seat]
       (let [adjcent-occupied (->> (neighbours pos)
                                   (filter #(= :occupied (state %))))]
         (case (state pos)
           :empty (if (empty? adjcent-occupied) :occupied :empty)
           :occupied (if (count-gte adjcent-occupied occupied-threshold) :empty :occupied))))
     state))
  ([neighbours occupied-threshold]
   (fn [state]
     (next-state state neighbours occupied-threshold))))

(defn stabilized-state [seq]
  (->> seq
       (partition 2 1)
       (find-first (fn [[n n+1]] (= n n+1)))
       first))

(defn count-occupied [state]
  (->> (vals state)
       (filter #{:occupied})
       count))

(defn count-occupied-after-stabilized [seats-positions next-state]
  (->> (zipmap seats-positions (repeat :empty))
       (iterate next-state)
       stabilized-state
       count-occupied))

(defpart part1 [seats-positions]
  (count-occupied-after-stabilized seats-positions (next-state s2/all-neighbours 4)))

;; part 2

(defn first-seat-in-dir [seats-positions valid-pos? pos dir]
  (->> (dir pos)
       (iterate dir)
       (take-while valid-pos?)
       (find-first seats-positions)))

(defn first-seats-in-all-dirs
  ([seats-positions valid-pos? pos]
   (keep #(first-seat-in-dir seats-positions valid-pos? pos %)
         s2/direction-fns-with-diags))
  ([seats-positions]
   (let [[x-range y-range] (s2/x-and-y-ranges seats-positions)
         valid-pos? (fn [[x y]]
                      (and (range-inc? x-range x)
                           (range-inc? y-range y)))]
     (memoize (fn [pos]
                (first-seats-in-all-dirs seats-positions valid-pos? pos))))))

(defpart part2 [seats-positions]
  (count-occupied-after-stabilized seats-positions (next-state (first-seats-in-all-dirs seats-positions) 5)))

;; tests

(deftest part1-test (part-test part1 37))

(deftest part2-test (part-test part2 26))
