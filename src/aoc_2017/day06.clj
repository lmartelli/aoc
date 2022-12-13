(ns aoc-2017.day06
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]))

(defn puzzle-input [stream]
  (->> (split (puzzle-input-string stream) #"\s+")
       (map parse-int)
       (zipmap (range))))

;; part 1

(defn detect-loop [s]
  (reduce
   (fn [prev x]
     (if (contains? prev x)
       (reduced [prev x])
       (conj prev x)))
   #{}
   s))

;; → [index value]
(defn max-bank [banks]
  (let [max-banks (val (apply max-key val banks))]
    (->> banks
         (filter #(= max-banks (val %)))
         (apply min-key key))))

(defn reallocate [banks]
  (let [[max-index max-value] (max-bank banks)
        modulo (fn [n] (mod n (count banks)))]
    (loop [new-banks (assoc banks max-index 0)
           index (inc max-index)
           n max-value]
      (if (zero? n)
        new-banks
        (recur
         (update new-banks (modulo index) inc)
         (inc index)
         (dec n))))))

(defpart part1 [input]
  (->> (iterate reallocate input)
       detect-loop
       first
       count))

;; part 2

(defpart part2 [input]
  (let [[prev repeated] (detect-loop (iterate reallocate input))]
    (- (count prev)
       (->> (take-while (partial not= repeated) (iterate reallocate input))
            count))))

;; tests
