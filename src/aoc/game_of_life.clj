(ns aoc.game-of-life
  (:require
   [aoc.core :refer [range-inc?]]
   [aoc.space-2d :as s2]
   [clojure.test :refer :all]))

(defn count-alive-neighbours [alive? neighbours cell]
  (->> (neighbours cell)
       (filter alive?)
       count))

(defn next-gen [alive-cells neighbours liveness-pred birth-pred]
  (->> (mapcat neighbours alive-cells)
       frequencies
       (keep (fn [[cell n]]
               (if (alive-cells cell)
                 (when (liveness-pred n) cell)
                 (when (birth-pred n) cell))))
       (into #{})))

(defn nth-gen
  "`gen-0` is a set of initially alive cells.
  `neighbours` is a function one 1 argument, that returns the neighbours of a cell.
  `liveness-pred` is a predicate on the number of live neighbour
  that tells wether a live cell remains alive.
  `birth-pred` is a predicate on the number of live neighbour
  that tells wether a new live cell should be created"
  [n & {:keys [gen-0 neighbours liveness-pred birth-pred]}]
  (-> (iterate #(next-gen % neighbours liveness-pred birth-pred) gen-0 )
      (nth n)))

;; Test

(deftest next-gen-test
  (testing "Classic game of life rules"
    (let [parse-cells #(set (s2/parse-2d-map-positions % \█))
          print-cells #(s2/print-to-lines (s2/draw-points \█ %))
          classic-next-gen (fn [state]
                             (next-gen
                               state
                               s2/all-neighbours
                               #(<= 2 % 3)
                               #(= % 3)))]
      
      (are [n n+1] (= n+1 (print-cells (classic-next-gen (parse-cells n))))
        ["█"] nil
        ["█  "
         "  █"
         " █ "]
        ["█"]
        ["█ █"
         "   "
         "█ █"]
        nil
        ["█ █"
         " █ "
         "█ █"]
        [" █ "
         "█ █"
         " █ "]))))
