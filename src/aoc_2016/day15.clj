(ns aoc-2016.day15
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (re-parse-lines #".* (\d+) positions.*position (\d+)\."
                       #(vector (parse-int %1) (parse-int %2)))))

;; part 1

(defn push-button? [discs t]
  (->> (map (fn [[n start-pos] offset]
              (mod (+ start-pos t offset 1) n))
            discs (range))
       (every? zero?)))

(defn time-to-get-capsule [discs]
  (find-first #(push-button? discs %) (range)))

(defpart part1 [discs]
  (time-to-get-capsule discs))

;; part 2

(defpart part2 [discs]
  (time-to-get-capsule (concat discs [[11 0]])))

;; tests

(deftest part1-test (part-test part1 5))
