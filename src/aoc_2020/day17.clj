(ns aoc-2020.day17
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [aoc.space-3d :as s3]
   [clojure.math.combinatorics :refer :all]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (s2/parse-2d-map-positions lines))

;; part 1

(defn add-dimension [coords]
  (map #(vec (conj % 0)) coords))

(defn neighbours-n-dim [nb-dim]
  (let [coord-modifiers (->> (apply cartesian-product (repeat nb-dim [identity inc dec]))
                             (remove (eq (repeat nb-dim identity))))]
    (fn [p]
      (->> (map
             #(mapv (fn [f x] (f x)) % p)
             coord-modifiers)))))

(defn exec-cycle [cubes neighbours]
  (->> (mapcat neighbours cubes)
       frequencies
       (keep (fn [[p n]]
               (if (cubes p)
                 (when (<= 2 n 3) p)
                 (when (= 3 n) p))))
       (into #{})))  

(defn count-active-cubes-after-boot-process [init-plane n]
  (let [cubes (-> (iterate add-dimension init-plane)
                  (nth (- n 2))
                  set)
        neighbours (neighbours-n-dim n)]
    (-> (iterate #(exec-cycle % neighbours) cubes)
        (nth 6)
        count)))

(defpart part1 [init-plane]
  (count-active-cubes-after-boot-process init-plane 3))

;; part 2
(defpart part2 [init-plane]
  (count-active-cubes-after-boot-process init-plane 4))

;; tests

(deftest neighbours-test
  (testing "1 dimnension"
    (is (= #{[-1] [1]} (set ((neighbours-n-dim 1) [0])))))
  (testing "2 dimnensions"
    (is (= #{[1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]}
           (set ((neighbours-n-dim 2) [0 0]))))))

(def data
  [".#."
   "..#"
   "###"])

(deftest part1-test
  (test-with-lines part1 data 112))

(deftest part2-test
  (test-with-lines part2 data 848))
