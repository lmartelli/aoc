(ns aoc-2021.day22
  (:require
   [aoc.core :refer :all]
   [clojure.set :as set]
   [clojure.math.combinatorics :refer :all]
   [clojure.test :refer :all]))

(defn cuboid
  "Converts [xmin xmax ymin ymax zmin zmax] to [[xmin xmax] [ymin ymax] [zmin zmax]]"
  [v]
  (if v
    (->> (partition 2 v)
         (mapv vec))))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines
    stream
    #(vector (keyword (re-find #"on|off" %))
             (cuboid (parse-ints %)))))

;; part 1

(def restricted-volume (cuboid [-50 50 -50 50 -50 50]))

(defn intersection [[[xmin1 xmax1] [ymin1 ymax1] [zmin1 zmax1]] [[xmin2 xmax2] [ymin2 ymax2] [zmin2 zmax2]]]
  (let [res [[(max xmin1 xmin2) (min xmax1 xmax2)]
             [(max ymin1 ymin2) (min ymax1 ymax2)]
             [(max zmin1 zmin2) (min zmax1 zmax2)]]]
    (if (every? (fn [[c-min c-max]] (<= c-min c-max)) res)
      res
      nil)))

(defn cubes [[[xmin xmax] [ymin ymax] [zmin zmax]]]
  (for [x (range-inc xmin xmax)
        y (range-inc ymin ymax)
        z (range-inc zmin zmax)]
    [x y z]))

(defn switch-on [reactor cuboid]
  (into reactor (cubes cuboid)))

(defn switch-off [reactor cuboid]
  (apply disj reactor (cubes cuboid)))

(defn apply-steps [reactor steps]
  (reduce
    (fn [reactor [op cuboid]]
      (case op
        :on (switch-on reactor cuboid)
        :off (switch-off reactor cuboid)))
    reactor
    steps))

(defpart part1 [steps]
  (->> (map #(update % 1 intersection restricted-volume) steps)
       (filter (fn [[op cuboid]] cuboid))
       (apply-steps #{})
       count))

;; part 2

(defn compare-intervals ":b-includes-a is prefered over :a-includes-b"
  [[a-min a-max :as a] [b-min b-max :as b]]
  (cond
    (< a-max b-min) :disjoint
    (< b-max a-min) :disjoint
    (<= b-min a-min a-max b-max) :b-includes-a
    (<= a-min b-min b-max a-max) :a-includes-b
    :else :overlap))

(defn intervals-difference
  "Returns a sequence of intervals"
  [[a-min a-max :as a] [b-min b-max :as b]]
  (cond
    (and (<= b-min a-min) (>= b-max a-max)) []
    (or (> b-min a-max) (< b-max a-min)) [a]
    (<= b-min a-min b-max a-max) [[(inc b-max) a-max]]
    (<= a-min b-min a-max b-max) [[a-min (dec b-min)]]
    :else [[a-min (dec b-min)] [(inc b-max) a-max]]))

(defn intervals-intersection
  [[a-min a-max] [b-min b-max]]
  (cond
    (<= a-min b-min a-max b-max) [b-min a-max]
    (<= b-min a-min b-max a-max) [a-min b-max]
    (<= a-min b-min b-max a-max) [b-min b-max]
    (<= b-min a-min a-max b-max) [a-min a-max]
    :else nil))

(defn remove-axis [cuboid axis]
  (let [[axis-1 axis-2] (remove #{axis} (range 3))]
    [(get cuboid axis-1) (get cuboid axis-2)]))

(defn drill
  "Makes a hole of given section along given axis
  Works on a corner, but not on a face"
  [cuboid axis [drill-section-1 drill-section-2]]
  (let [[section-axis-1 section-axis-2] (remove #{axis} (range 3))
        [below-hole-axis-1 above-hole-axis-1] (intervals-difference
                                                (get cuboid section-axis-1)
                                                drill-section-1)
        [below-hole-axis-2 above-hole-axis-2] (intervals-difference
                                                (get cuboid section-axis-2)
                                                drill-section-2)
        hole-1 (intervals-intersection (get cuboid section-axis-1) drill-section-1)]
    (remove
      nil?
      [(if below-hole-axis-1
         (-> cuboid
             (assoc section-axis-1 below-hole-axis-1)))
       (if above-hole-axis-1
         (-> cuboid
             (assoc section-axis-1 above-hole-axis-1)))
       (if below-hole-axis-2
         (-> cuboid
             (assoc section-axis-1 hole-1)
             (assoc section-axis-2 below-hole-axis-2)))
       (if above-hole-axis-2
         (-> cuboid
             (assoc section-axis-1 hole-1)
             (assoc section-axis-2 above-hole-axis-2)))])))

(defn slice "Makes slices, perpendicular to given axis"
  [cuboid axis middle-slice]
  (let [[lower-slice upper-slice] (intervals-difference (get cuboid axis) middle-slice)]
    (remove
      nil?
      [(if-let [middle (intervals-intersection middle-slice (get cuboid axis))]
         (assoc cuboid axis middle))
       (if lower-slice
         (assoc cuboid axis lower-slice))
       (if upper-slice
         (assoc cuboid axis upper-slice))])))

(defn difference [A B]
  (let [intervals-comparisons (mapv #(compare-intervals %1 %2) A B)
        axis (fn [v] (index-of intervals-comparisons v))]
    (cond
      (= A B) []
      (every? #{:b-includes-a} intervals-comparisons) []
      (some #{:disjoint} intervals-comparisons) [A]
      :else (case (frequencies intervals-comparisons)
              ;; cut on 1 axis
              {:b-includes-a 2 :overlap 1}
              (let [drill-axis (axis :b-includes-a)]
                (drill A drill-axis (remove-axis B drill-axis)))
              ;; hole on a face
              {:a-includes-b 2 :overlap 1}
              (let [overlap-axis (axis :overlap)
                    [punched-slice intact-slice] (slice A overlap-axis (get B overlap-axis))]
                (concat (drill punched-slice overlap-axis (remove-axis B overlap-axis))
                        [intact-slice]))
              ;; Remove an edge
              {:b-includes-a 1 :overlap 2}
              (let [drill-axis (axis :b-includes-a)]
                (drill A drill-axis (remove-axis B drill-axis)))
              ;; Remove a corner
              {:overlap 3}
              (let [[drilled untouched] (slice A 0 (get B 0))]
                (remove nil?
                        (concat [untouched]
                                (drill drilled 0 (remove-axis B 0)))))
              ;; Cut in 2 pieces
              {:b-includes-a 2 :a-includes-b 1}
              (let [drill-axis (axis :b-includes-a)]
                (drill A drill-axis (remove-axis B drill-axis)))
              ;; Drill through opposite faces
              {:b-includes-a 1 :a-includes-b 2}
              (let [drill-axis (axis :b-includes-a)]
                (drill A drill-axis (remove-axis B drill-axis)))
              ;; Bite edge
              {:a-includes-b 1 :overlap 2}
              (let [bite-axis (axis :a-includes-b)
                    [s1 s2 s3] (slice A bite-axis (get B bite-axis))]
                (remove nil?
                  (concat
                    [s2 s3]
                    (drill s1 bite-axis (remove-axis B bite-axis)))))
              ;;
              {:b-includes-a 1, :a-includes-b 1, :overlap 1}
              (let [drill-axis (axis :b-includes-a)]
                (drill A drill-axis (remove-axis B drill-axis)))
              ;; Hole in the middle of A
              {:a-includes-b 3}
              (let [[s1 s2 s3] (slice A 2 (get B 2))]
                (remove nil?
                  (concat
                    [s2 s3]
                    (drill s1 2 (remove-axis B 2)))))))))

(defn switch-on-2 [lit-cuboids c]
  (into lit-cuboids
        (reduce
          (fn [new-cuboids existing-cuboid]
            (mapcat #(difference % existing-cuboid) new-cuboids))
          [c]
          lit-cuboids)))

(defn switch-off-2 [lit-cuboids c]
  ;#dbg (println "off" c)
  (into []
        (mapcat #(difference % c) lit-cuboids)))

(defn apply-steps-2 [lit-cuboids steps]
  (reduce
    (fn [lit-cuboids [op cuboid]]
      (case op
        :on (switch-on-2 lit-cuboids cuboid)
        :off (switch-off-2 lit-cuboids cuboid)))
    lit-cuboids
    steps))

(defn sum-volumes [cuboids]
  (->> (map (fn [cuboid]
              (->> (map (fn [[cmin cmax]] (inc (- cmax cmin))) cuboid)
                   (reduce *)))
            cuboids)
       (reduce +)))

(defpart part2 [steps]
  (->> (apply-steps-2 [] steps)
       sum-volumes))

;; tests

(deftest intervals-intersection-test
  (are [a b expected] (= expected (intervals-intersection a b) (intervals-intersection b a))
    [0 1] [2 2] nil
    [1 1] [2 2] nil
    [0 1] [2 3] nil
    [0 1] [0 1] [0 1]
    [0 1] [1 1] [1 1]
    [0 1] [1 2] [1 1]
    [1 2] [0 1] [1 1]
    [1 2] [1 1] [1 1]
    [1 2] [1 3] [1 2]
    [1 2] [0 2] [1 2]
    [1 2] [0 3] [1 2]
    ))

(deftest intervals-difference-test
  (are [a b] (empty? (intervals-difference a b))
    [1 2] [0 2]
    [1 2] [1 2]
    [1 2] [1 3]
    [1 2] [0 3])
  (are [a b] (= [a] (intervals-difference a b))
    [3 4] [5 5]
    [3 4] [5 6]
    [3 4] [2 2]
    [3 4] [1 2])
  (are [a b expected] (= expected (into #{} (intervals-difference a b)))
    [4 6] [6 6] #{[4 5]}
    [4 6] [6 7] #{[4 5]}
    [4 6] [5 6] #{[4 4]}
    [4 6] [4 4] #{[5 6]}
    [4 6] [3 4] #{[5 6]}
    [4 6] [3 5] #{[6 6]}
    [4 6] [5 5] #{[4 4] [6 6]}))

(deftest intersection-test
  (are [c1 c2 expected] (= (cuboid expected) (intersection (cuboid c1) (cuboid c2)))
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

(deftest slice-test
  (are [c axis interval expected] (= (into #{} (map cuboid expected)) (into #{} (slice (cuboid c) axis interval)))
    [0 2, 0 2, 0 2] 0 [3 4]   #{[0 2, 0 2, 0 2]}
    [0 2, 0 2, 0 2] 0 [-2 -1] #{[0 2, 0 2, 0 2]}
    [0 2, 0 2, 0 2] 0 [0 0]   #{[0 0, 0 2, 0 2] [1 2, 0 2, 0 2]}
    [0 2, 0 2, 0 2] 0 [-1 0]   #{[0 0, 0 2, 0 2] [1 2, 0 2, 0 2]}
    [0 2, 0 2, 0 2] 0 [2 2]   #{[0 1, 0 2, 0 2] [2 2, 0 2, 0 2]}
    [0 2, 0 2, 0 2] 0 [2 3]   #{[0 1, 0 2, 0 2] [2 2, 0 2, 0 2]}
    [0 2, 0 2, 0 2] 0 [1 1]   #{[0 0, 0 2, 0 2] [1 1, 0 2, 0 2] [2 2, 0 2, 0 2]}))

(defn- add-axis [[s1 s2 :as surface] ^long axis value]
  (case axis
    0 [value s1 s2]
    1 [s1 value s2]
    2 [s1 s2 value]))

(deftest drill-test
  (are [flattened-cuboid axis drill-section-1 drill-section-2]
      (let [cuboid (cuboid flattened-cuboid)]
        (check-fn-difference
          #(drill % axis [drill-section-1 drill-section-2])
          cuboid
          (add-axis [drill-section-1 drill-section-2] axis (get cuboid axis))))
    [0 2, 0 2, 0 2] 2 [1 1] [1 1]
    [0 2, 0 2, 0 2] 2 [1 2] [1 1]
    [0 2, 0 2, 0 2] 2 [1 3] [1 1]
    [0 2, 0 2, 0 2] 2 [1 1] [1 2]
    [0 2, 0 2, 0 2] 2 [1 1] [1 3]
    [0 2, 0 2, 0 2] 2 [1 2] [1 2]
    [0 2, 0 2, 0 2] 2 [1 3] [1 3]
    [0 2, 0 2, 0 2] 2 [1 1] [1 1]
    [0 2, 0 2, 0 2] 2 [0 1] [1 1]
    [0 2, 0 2, 0 2] 2 [-1 1] [1 1]
    [0 2, 0 2, 0 2] 2 [1 1] [0 1]
    [0 2, 0 2, 0 2] 2 [1 1] [-1 1]
    [0 2, 0 2, 0 2] 2 [0 1] [0 1]
    [0 2, 0 2, 0 2] 2 [-1 1] [-1 1]
    [0 2, 0 2, 0 2] 2 [1 1] [0 2]
    [0 2, 0 2, 0 2] 2 [1 1] [0 3]
    [0 2, 0 2, 0 2] 2 [1 1] [-1 2]
    [0 2, 0 2, 0 2] 2 [1 1] [-1 3]
    [0 2, 0 2, 0 2] 2 [0 2] [1 1]
    [0 2, 0 2, 0 2] 2 [0 3] [1 1]
    [0 2, 0 2, 0 2] 2 [-1 2] [1 1]
    [0 2, 0 2, 0 2] 2 [-1 3] [1 1]))

(defn check-difference [A B]
  (let [a-b-cubes (->> (cubes A) (remove (into #{} (cubes B))) (into #{}))
        res-cubes (mapcat cubes (difference A B))]
    (and (= a-b-cubes (into #{} res-cubes))
         (or (empty? res-cubes)
             (apply distinct? res-cubes)))))

;; Very slow (1_000_000 tests ...)
#_(deftest difference-test-full
  (let [intervals (->> (selections (range 4) 2) (filter #(apply <= %)))
        cuboids (for [[x-min x-max] intervals
                      [y-min y-max] intervals
                      [z-min z-max] intervals]
                  [[x-min x-max] [y-min y-max] [z-min z-max]])]
      (dorun
        (for [A cuboids
              B cuboids]
          (is (check-difference A B))))))

(deftest difference-test
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
    (are [A B] (empty? (difference (cuboid A) (cuboid B)))
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
    (are [A B] (= [(cuboid A)] (difference (cuboid A) (cuboid B)))
      [0 1, 0 1, 0 1] [2 3, 0 1, 0 1]
      [0 1, 0 1, 0 1] [0 1, 2 3, 0 1]
      [0 1, 0 1, 0 1] [0 1, 0 1, 2 3]))
  (testing "B includes² A & overlap¹: chop"
    (let [intervals (->> (selections (range 3) 2) (filter #(apply <= %)))]
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
    [[0 0] [0 0] [0 0]] [[0 0 0]]
    [[0 1] [0 1] [0 1]] [[0 0 0] [0 0 1] [0 1 0] [0 1 1] [1 0 0] [1 0 1] [1 1 0] [1 1 1]]))

(deftest part1-test (part-test part1 590784))

(deftest part2-test (part-tests part2 ["part2" 2758514936282235]))
