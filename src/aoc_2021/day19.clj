(ns aoc-2021.day19
  (:require
   [aoc.core :refer :all]
   [aoc.space-3d :as s3]
   [clojure.math.combinatorics :refer [combinations]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (split-seq empty?)
       (map (fn [[_ & coords]]
              (map parse-ints coords)))))

;; part 1

(defn transformation [f]
  (fn [points] (set (map f points))))

(defn orientations [points]
  (->> (reductions
         #(%2 %1)
         points
         (map transformation [s3/rotate-z s3/rotate-z s3/rotate-z s3/rotate-x (comp s3/rotate-x s3/rotate-x)]))
       (mapcat #(->> % (iterate (transformation s3/rotate-y)) (take 4)))))

(defn overlapping-translation [scan-a scan-b]
  (some->> (for [ta scan-a, tb scan-b]
             (s3/- ta tb))
           frequencies
           (find-first #(>= (val %) 12))
           key))

(defn overlapping-beacons
  [scan ref-scan]
  (->> (for [oriented-scan (->> (orientations scan))
             :let [translation (overlapping-translation oriented-scan ref-scan)]
             :when translation]
         [ref-scan (map #(s3/- % translation) oriented-scan)])
       first))

(defn find-overlapping-pair [scans]
  (-> (for [[a b] (combinations scans 2)
            :let [[overlap-a overlap-b :as overlap] (overlapping-beacons a b)]
            :when overlap]
         (conj (filter (complement #{a b}) scans)
               (into #{} (concat overlap-a overlap-b))))
      first))

(defn merge-scans [scans]
  (println "Scanners to merge" (count scans))
  (cond (= 1 (count scans)) (first scans)
        (nil? scans) "Error: no overlapping pair found"
        :else (merge-scans (find-overlapping-pair scans))))

(defpart part1 [scans]
  (count (merge-scans scans)))

;; part 2

(defpart part2 [scans]
  )

;; tests

(deftest part1-test (part-test part1 79))

(deftest part2-test (part-test part2 3121))

