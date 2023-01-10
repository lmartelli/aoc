(ns aoc-2021.day13
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [aoc.ocr :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (let [[coords folds] (split-seq empty? (line-seq stream))]
    {:coords (into #{} (map parse-ints coords))
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

(defn fold-and-show [{:keys [:coords :folds]}]
  (-> (reduce fold coords folds)
      s2/draw-points
      s2/print-to-lines))

(defpart part2 [input]
  (-> (fold-and-show input)
      ocr))

;; tests

(deftest fold-test
  (are [points axis pos expected] (= (set expected) (set (fold points [axis pos])))
    [[0 0]] :x 0 [[0 0]]
    [[0 0]] :y 0 [[0 0]]
    [[0 1]] :x 0 [[0 1]]
    [[0 1]] :y 0 [[0 -1]]
    [[0 0]] :x 1 [[0 0]]
    [[3 4]] :x 2 [[1 4]]
    [[3 4]] :y 2 [[3 0]]))

  (deftest part2-test
  (is (= ["#####"
          "#   #"
          "#   #"
          "#   #"
          "#####"]
         (fold-and-show (test-data)))))

(deftest part1-test (part-test part1 17))
