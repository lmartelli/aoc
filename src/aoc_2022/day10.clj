(ns aoc-2022.day10
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (split-lines #" " #(concat [(keyword %1)] (map parse-int %&)))))

;; part 1

(defn x-values [prog]
  (->> (reductions
        (fn [xs [instr arg]]
          (let [last-x (peek xs)]
            (case instr
              :noop [last-x]
              :addx [last-x (+ last-x arg)])))
        [1]
        prog)
       (mapcat identity)))

(defpart part1 [prog]
  (->> (x-values prog)
       (drop 19)
       (take-nth 40)
       (map * (iterate #(+ 40 %) 20))
       (take 6)
       (reduce +)))

;; part 2

(defn pixels-lines [prog screen-width]
  (->> (map (fn [x pos] (if (<= (dec x) pos (inc x)) \u2588 \space))
            (x-values prog) (cycle (range 0 screen-width)))
       (partition screen-width)
       (map #(apply str %))))

(defpart part2 [prog]
  (doseq [line (pixels-lines prog 40)]
    (println line)))

;; tests

(deftest x-values-test
  (are [cycle expected] (= expected (nth (x-values (test-data)) (dec cycle)))
    20 21
    60 19
    100 18
    140 21
    180 16
    220 18))

(deftest part1-test (part-test part1 13140))

(deftest pixel-lines-test
  (= ["██  ██  ██  ██  ██  ██  ██  ██  ██  ██  "
      "███   ███   ███   ███   ███   ███   ███ "
      "████    ████    ████    ████    ████    "
      "█████     █████     █████     █████     "
      "██████      ██████      ██████      ████"
      "███████       ███████       ███████     "]
     (pixel-lines (test-data))))
