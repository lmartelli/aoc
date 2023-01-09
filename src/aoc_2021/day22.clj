(ns aoc-2021.day22
  (:require
   [aoc.core :refer :all]
   [aoc.range :as r]
   [clojure.set :as set]
   [clojure.math.combinatorics :refer :all]
   [clojure.test :refer :all]))

(defn cuboid
  "Converts [xmin xmax ymin ymax zmin zmax] to [[xmin xmax+1] [ymin ymax+1] [zmin zmax+1]]"
  [v]
  (if v
    (->> (partition 2 v)
         (mapv (comp #(update % 1 inc) vec)))))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines
    stream
    #(vector (keyword (re-find #"on|off" %))
             (cuboid (parse-ints %)))))

;; part 1

(defn cuboid-intersection [A B]
  (let [res (mapv r/intersection A B)]
    (if (some nil? res)
      nil
      res)))

(defn cubes [[[xmin xmax] [ymin ymax] [zmin zmax]]]
  (for [x (range xmin xmax)
        y (range ymin ymax)
        z (range zmin zmax)]
    [x y z]))

(defn cuboid-difference [A B]
  (let [slices (map r/slice A B)]
    (if (some (fn [[intersection]] (nil? intersection)) slices)
      [A]
      (->> (map-indexed vector slices)
           (sort-by (fn [[axis slices]] (count slices)))
           (reduce
             (fn [[current & others] [axis slices]]
               (concat
                 (map #(assoc current axis %) slices)
                 others))
             [A])
           rest))))

(defn cuboid-differences [As B]
  (mapcat #(cuboid-difference % B) As))

(defn switch-on [lit-cuboids c]
  (conj (cuboid-differences lit-cuboids c)
        c))

(defn switch-off [lit-cuboids c]
  (cuboid-differences lit-cuboids c))

(defn apply-step [lit-cuboids [op cuboid]]
  (case op
    :on (switch-on lit-cuboids cuboid)
    :off (switch-off lit-cuboids cuboid)))

(defn apply-steps [lit-cuboids steps]
  (reduce apply-step lit-cuboids steps))

(defn cuboid-volume [cuboid]
  (->> (map (fn [[cmin cmax]] (- cmax cmin)) cuboid)
       (reduce *)))

(defn sum-volumes [cuboids]
  (->> (map cuboid-volume cuboids)
       (reduce +)))

(defpart part1 [steps]
  (->> (map #(update % 1 cuboid-intersection (into [] (repeat 3 [-50 50]))) steps)
       (filter (fn [[op cuboid]] cuboid))
       (apply-steps [])
       sum-volumes))

;; part 2

(defpart part2 [steps]
  (->> (apply-steps [] steps)
       sum-volumes))

;; tests

(deftest cuboid-intersection-test
  (are [c1 c2 expected] (= (cuboid expected) (cuboid-intersection (cuboid c1) (cuboid c2)))
    ;; Ax-includes-Bx & Ay-includes-By & Az-includes-Bz
    [0 2, 0 2, 0 2] [0 1, 0 1, 0 1] [0 1, 0 1, 0 1]
    ;; Bx-includes-Ax & By-includes-Ay & Bz-includes-Az
    [0 1, 0 1, 0 1] [0 2, 0 2, 0 2] [0 1, 0 1, 0 1]
    
    [0 2, 0 2, 0 2] [1 3, 1 3, 1 3] [1 2, 1 2, 1 2]
    [1 3, 1 3, 1 3] [0 2, 0 2, 0 2] [1 2, 1 2, 1 2]
    [0 2, 0 2, 0 2] [-1 1, -1 1, -1 1] [0 1, 0 1, 0 1]
    [-1 1, -1 1 -1 1] [0 2, 0 2, 0 2] [0 1, 0 1, 0 1]
    [0 1, 0 1, 0 1] [1 2, 1 2, 1 2] [1 1, 1 1, 1 1]
    [-12 35, 6 50, -50 -2] [-50 50, -50 50, -50 50] [-12 35, 6 50, -50 -2]
    [0 3, 0 3, 0 3] [1 2, 1 2, 3 4] [1 2, 1 2, 3 3]
    [967 23432 45373 81175 27513 53682] [-50 50, -50 50, -50 50] nil
    [0 1, 0 1, 0 1] [2 3, 2 3, 2 3] nil))

(defn check-fn-difference [f A B]
  (let [a-b-cubes (->> (cubes A) (remove (into #{} (cubes B))) (into #{}))
        res-cubes (mapcat cubes (f A))]
    (and (= a-b-cubes (into #{} res-cubes))
         (apply distinct? res-cubes))))

(defn check-difference [A B]
  (let [a-b-cubes (->> (cubes A) (remove (into #{} (cubes B))) (into #{}))
        res-cubes (mapcat cubes (cuboid-difference A B))]
    (and (= a-b-cubes (into #{} res-cubes))
         (or (empty? res-cubes)
             (apply distinct? res-cubes)))))

(deftest difference-test-full
  (let [intervals (->> (selections (range 4) 2) (filter #(apply < %)))
        cuboids (->> (cartesian-product intervals intervals intervals)
                     (map vec))]
    (dorun
      ;; It will take some time if you consider all basic cuboids for A & B
      (for [A (random-sample 0.2 cuboids)
            B (random-sample 0.2 cuboids)]
        (is (check-difference A B))))))

(deftest cuboid-difference-test
  (testing "A includes³ B → Hole in the middle of A"
    (are [A B] (check-difference (cuboid A) (cuboid B))
      [0 2, 0 2, 0 2] [1 1, 1 1, 1 1]
      [0 2, 0 2, 0 2] [1 1, 1 1, 1 2]
      [0 2, 0 2, 0 2] [1 1, 1 1, 0 1]
      [0 2, 0 2, 0 2] [1 1, 1 2, 1 1]
      [0 2, 0 2, 0 2] [1 1, 0 1, 1 1]
      [0 2, 0 2, 0 2] [1 2, 1 1, 1 1]
      [0 2, 0 2, 0 2] [0 1, 1 1, 1 1]
      [0 2, 0 2, 0 2] [1 2, 1 2, 1 1]
      [0 2, 0 2, 0 2] [0 1, 0 1, 1 1]
      [0 2, 0 2, 0 2] [1 1, 1 2, 1 2]
      [0 2, 0 2, 0 2] [1 1, 0 1, 0 1]
      [0 2, 0 2, 0 2] [1 2, 1 1, 1 2]
      [0 2, 0 2, 0 2] [0 1, 1 1, 0 1]))
  (testing "B includes³ A → ø"
    (are [A B] (empty? (cuboid-difference (cuboid A) (cuboid B)))
      [0 1, 0 1, 0 1] [0 1, 0 1, 0 1]
      [0 1, 0 1, 0 1] [0 2, 0 1, 0 1]
      [0 1, 0 1, 0 1] [-1 1, 0 1, 0 1]
      [0 1, 0 1, 0 1] [0 1, 0 2, 0 1]
      [0 1, 0 1, 0 1] [0 1, -1 1, 0 1]
      [0 1, 0 1, 0 1] [0 1, 0 1, 0 2]
      [0 1, 0 1, 0 1] [0 1, 0 1, -1 1]
      [0 1, 0 1, 0 1] [0 2, 0 2, 0 2]
      [0 1, 0 1, 0 1] [-1 2, -1 2, -1 2]
      ))
  (testing "A and B are disjoint → [A]"
    (are [A B] (= [(cuboid A)] (cuboid-difference (cuboid A) (cuboid B)))
      [0 1, 0 1, 0 1] [2 3, 0 1, 0 1]
      [0 1, 0 1, 0 1] [0 1, 2 3, 0 1]
      [0 1, 0 1, 0 1] [0 1, 0 1, 2 3]))
  (testing "B includes² A & overlap¹: chop"
    (let [intervals (->> (selections (range 4) 2) (filter #(apply < %)))]
      (dorun
        (for [[min-1 max-1] intervals
              [min-2 max-2] intervals
              [min-3 max-3] [[2 3] [-1 0]]]
          (are [A B] (check-difference (cuboid A) (cuboid B))
            [min-1 max-1, min-2 max-2, min-3 max-3] [0 2, 0 2, 0 2]
            [min-1 max-1, min-3 max-3, min-2 max-2] [0 2, 0 2, 0 2]
            [min-3 max-3, min-1 max-1, min-2 max-2] [0 2, 0 2, 0 2]
            )))))
  (testing "A includes² B, overlap¹: hole in a face"
    (are [A B] (check-difference (cuboid A) (cuboid B))
      [0 2, 0 2, 0 2] [1 1, 1 1, 1 3]))
  (testing "B includes¹ A & overlap² (intersect on edge)"
    (are [B A] (check-difference (cuboid A) (cuboid B))
      [0 2, 0 2, 0 2] [2 3, 2 3, 1 1]
      [0 2, 0 2, 0 2] [2 3, -1 0, 1 1]
      [0 2, 0 2, 0 2] [-1 0, -1 0, 1 1]
      [0 2, 0 2, 0 2] [2 3, 1 1, 2 3]
      [0 2, 0 2, 0 2] [1 1, 2 3, 2 3]))
  (testing "overlap³ (intersect on corner)"
    (are [B A] (check-difference (cuboid A) (cuboid B))
      [0 2, 0 2, 0 2] [2 3, 2 3, 2 3]
      [0 2, 0 2, 0 2] [2 3, 1 1, 2 3]
      [0 2, 0 2, 0 2] [1 1, 2 3, 2 3]))
  (testing "B includes² A, A includes¹ B: remove middle section"
    (are [B A] (check-difference (cuboid A) (cuboid B))
      [0 2, 0 2, 0 2] [1 1, 1 1, -1 3]))
  (testing "A includes² B, B includes¹ A: drill hole through"
    (are [A B] (check-difference (cuboid A) (cuboid B))
      [0 2, 0 2, 0 2] [1 1, 1 1, -1 3])))

(deftest cubes-test
  (are [c expected] (= (into #{} expected) (into #{} (cubes c)))
    [[0 0] [0 0] [0 0]] []
    [[0 1] [0 1] [0 1]] [[0 0 0]]
    [[0 2] [0 2] [0 2]] [[0 0 0] [0 0 1] [0 1 0] [0 1 1] [1 0 0] [1 0 1] [1 1 0] [1 1 1]]))

(deftest part1-test (part-test part1 590784))

(deftest part2-test (part-tests part2 ["part2" 2758514936282235]))
