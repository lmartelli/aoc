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

(defn overlapping-scans
  [ref-scan scan]
  (->> (for [oriented-scan (->> (orientations scan))
             :let [translation (overlapping-translation oriented-scan ref-scan)]
             :when translation]
         [translation (map #(s3/- % translation) oriented-scan)])
       first))

(defn merge-overlapping-pair [scans]
  (-> (for [[a b] (combinations scans 2)
            :let [[T transformed-a :as overlap] (overlapping-scans b a)]
            :when overlap]
         (conj (filter (complement #{a b}) scans)
               (into #{} (concat transformed-a b))))
      first))

(defn merge-scans [scans]
  (println "Scanners to merge" (count scans))
  (cond (= 1 (count scans)) (first scans)
        (nil? scans) "Error: no overlapping pair found"
        :else (merge-scans (merge-overlapping-pair scans))))

(defpart part1 [scans]
  (count (merge-scans scans)))

;; part 2

(defn find-overlapping-pair [ref-scans scans]
  (println "find-overlapping-pair" (count ref-scans) "/" (count scans))
  (first
    (for [[ref-pos ref-scan] ref-scans, scan scans
          :let [[T transformed-scan] (overlapping-scans ref-scan scan)]
          :when T]
      (do
        [(assoc ref-scans T transformed-scan) (remove #{scan} scans)]))))

(defn scans-relative-positions
  ([[ref-scans scans]]
   (if (empty? scans)
     (keys ref-scans)
     (scans-relative-positions
       (find-overlapping-pair ref-scans scans)))))

(defpart part2 [scans]
  (->> (combinations (scans-relative-positions [{[0 0 0] (first scans)} (rest scans)]) 2)
       (map #(apply s3/manatthan-dist %))
       (apply max)))

;; tests

(deftest part1-test (part-test part1 79))

(deftest part2-test (part-test part2 3121))

