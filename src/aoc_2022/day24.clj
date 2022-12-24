(ns aoc-2022.day24
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [aoc.algo :as algo]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(def blizzards {\< :left \> :right \^ :up \v :down})

(defn puzzle-input [stream]
  (let [lines (line-seq stream)
        width (- (count (first lines)) 2)
        height (- (count lines) 2)]
    {:width width 
     :height height
     :start [(dec (str/index-of (first lines) \.)) -1]
     :end [(dec (str/index-of (last lines) \.)) height]
     :blizzards (->> (s2/pos-and-values-seq lines)
                     (filter (fn [[pos value]] ((set (keys blizzards)) value)) )
                     (group-by second)
                     (map (fn [[blizzard positions]]
                            [(blizzards blizzard) (set (map (fn [[[x y] _]] [(dec x) (dec y)])
                                                            positions))]))
                     (into {}))}))

;; part 1

(defn blizzard-shifts [width height]
  {:up (fn [[x y] time]
         [x (mod (+ y time) height)])
   :down (fn [[x y] time]
           [x (mod (- y time) height)])
   :left (fn [[x y] time]
           [(mod (+ x time) width) y])
   :right (fn [[x y] time]
            [(mod (- x time) width) y])
   })

(defn cross-valley [start-state target-pos {:keys [start end width height blizzards]}]
  (let [shifts (blizzard-shifts width height)]
    (letfn [(safe? [[pos time]]
              (not-any?
                (fn [[blizzard shift]]
                  ((blizzards blizzard) (shift pos time)))
                shifts))
            (neighbours [[pos time]]
              (keep (fn [[x y :as next-pos]]
                      (let [next-state [next-pos (inc time)]]
                        (when (or (#{start end} next-pos)
                                  (and (<= 0 x (dec width))
                                       (<= 0 y (dec height))
                                       (safe? next-state)))
                          next-state)))
                    (conj (s2/direct-neighbours pos) pos)))]
      (loop [last-visited (conj {} start-state)]
        (if-let [time (last-visited target-pos)]
          [target-pos time]
          (let [next-positions (mapcat neighbours last-visited)]
            (recur
              (into {} next-positions))))))))

(defn trip-time [{:keys [start end] :as input} start & steps]
  (let [[_ arrival-time] (reduce
                           (fn [state target-pos]
                             (cross-valley state target-pos input))
                           [start 0]
                           steps)]
    arrival-time))

(defpart part1 [{:keys [start end] :as input}]
  (trip-time input start end))

;; part 2

(defpart part2 [{:keys [start end] :as input}]
  (trip-time input start end start end))

;; tests

(deftest part1-test (part-test part1 18))

(deftest part2-test (part-test part2 54))
