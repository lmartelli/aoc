(ns aoc-2021.day19
  (:require
   [aoc.core :refer :all]
   [clojure.math.combinatorics :refer [combinations]]
   [clojure.set :refer [intersection]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (split-seq empty?)
       (map (fn [[_ & coords]]
              (map (comp #(mapv parse-int %) #(split % #",")) coords)))))

;; part 1

(defn rotate-x [points]
  (map (fn [[x y z]] [x z (- y)]) points))

(defn rotate-x-2 [points]
  (map (fn [[x y z]] [x (- y) (- z)]) points))

(defn rotate-y [points]
  (map (fn [[x y z]] [z y (- x)]) points))

(defn rotate-z [points]
  (map (fn [[x y z]] [y (- x) z]) points))

(defn orientations [points]
  (->> (reductions
         (fn [points f] (f points))
         points
         [rotate-z rotate-z rotate-z rotate-x rotate-x-2])
       (mapcat #(->> (iterate rotate-y %) (take 4)))))

(defn relative-to [origin points]
  (map #(sub % origin) points))

(defn relative-to-first [points]
  (relative-to (first points) points))

(defn relative-to-each [points]
  (map #(relative-to % points) points))

(defn overlapping-beacons
  [beacons-a beacons-b]
  (->> (for [a (->> (orientations beacons-a)
                    (mapcat #(relative-to-each %)))
             b (relative-to-each beacons-b)]
         [a b])
       (find-first (fn [[a b]] (>= (count (intersection (into #{} a) (into #{} b))) 12)))))

(defn find-overlapping-pair [scans]
  (-> (for [[a b] (combinations scans 2)
            :let [overlap (overlapping-beacons a b)
                  [overlap-a overlap-b] overlap]
            :when overlap]
        (let [found (conj (filter (complement #{a b}) scans) (into #{} (concat overlap-a overlap-b)))]
          (println "Found overlapping pair")
          ;;(clojure.pprint/pprint found)
          found))
      first))

(defn merge-scans [scans]
  (println (count scans))
  (cond (= 1 (count scans)) (first scans)
        (nil? scans) "Error: no overlapping pair found"
        :else (merge-scans (find-overlapping-pair scans))))

(defpart part1 [scans]
  (count (merge-scans scans)))

;; part 2

(defpart part2 [numbers]
  )

;; tests

(def test-data (puzzle-input (test-input *ns*)))

(deftest part1-test
  (is (= 79 (part1 test-data))))

(deftest part2-test
  (is (= nil (part2 test-data))))

