(ns aoc-2021.day13
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (let [lines (line-seq stream)
        [coords folds] (split-seq empty? lines)]
    {:coords (into #{} (map parse-int-array  coords))
     :folds (map #(let [[_ axis pos] (re-find #"(x|y)=(\d+)" %)]
                    [(keyword axis) (parse-int pos)])
                 folds)}))

;; part 1

(defn symetry [x centre]
  (+ x (* 2 (- centre x))))

(defn fold [points [axis pos]]
  (into #{} (map
              #(let [n-axis ({:x 0 :y 1} axis)]
                 (if (> (get % n-axis) pos)
                   (update % n-axis symetry pos)
                   %))
              points)))

(defpart part1 [{:keys [:coords :folds]}]
  (-> coords
      (fold (first folds))
      count))

;; part 2

(defn plot [points]
  (let [width (apply max (map first points))
        height (apply max (map second points))]
    (prn)
    (run!
      prn
      (map (fn [y]
             (apply str (map (fn [x] (if (points [x y]) \# \space)) (range-inc width))))
           (range-inc height))
      )
    points))

(defpart part2 [{:keys [:coords :folds]}]
  (-> (reduce fold coords folds)
      plot))

;; tests

(deftest fold-test
  (are [points axis pos expected] (->> [expected (fold [axis pos] points)] (map #(into #{} %)) (apply =))
    [[0 0]] :x 0 [[0 0]]
    [[0 0]] :y 0 [[0 0]]
    [[0 1]] :x 0 [[0 1]]
    [[0 1]] :y 0 [[0 -1]]
    [[0 0]] :x 1 [[2 0]]
    [[3 4]] :x 2 [[1 4]]
    [[3 4]] :y 2 [[3 0]]
    ))

(deftest part1-test (part-test part1 17))
