(ns aoc-2021.day22
  (:require
   [aoc.core :refer :all]
   [clojure.set :as set]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines
    stream
    #(let [[_ op & cuboid]
           (re-matches #"(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)" %)]
       [(keyword op) (mapv parse-int cuboid)])))

;; part 1

(def restricted-volume [-50 50 -50 50 -50 50])

(defn intersection [[xmin1 xmax1 ymin1 ymax1 zmin1 zmax1] [xmin2 xmax2 ymin2 ymax2 zmin2 zmax2]]
  (let [res [(max xmin1 xmin2) (min xmax1 xmax2)
             (max ymin1 ymin2) (min ymax1 ymax2)
             (max zmin1 zmin2) (min zmax1 zmax2)]]
    (if (every? (fn [[c-min c-max]] (<= c-min c-max)) (partition 2 res))
      res
      nil)))

(defn cubes [[xmin xmax ymin ymax zmin zmax]]
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

(defpart part2 [steps]
  )

;; tests

(def test-data (puzzle-input (test-input *ns*)))

(deftest intersection-test
  (are [c1 c2 expected] (= expected (intersection c1 c2))
    [0 1 0 1 0 1] [0 2 0 2 0 2] [0 1 0 1 0 1]
    [0 2 0 2 0 2] [0 1 0 1 0 1] [0 1 0 1 0 1]
    [0 2 0 2 0 2] [1 3 1 3 1 3] [1 2 1 2 1 2]
    [1 3 1 3 1 3] [0 2 0 2 0 2] [1 2 1 2 1 2]
    [0 2 0 2 0 2] [-1 1 -1 1 -1 1] [0 1 0 1 0 1]
    [-1 1 -1 1 -1 1] [0 2 0 2 0 2] [0 1 0 1 0 1]
    [0 1 0 1 0 1] [1 2 1 2 1 2] [1 1 1 1 1 1]
    [-12 35 6 50 -50 -2] restricted-volume [-12 35 6 50 -50 -2]
    [967 23432 45373 81175 27513 53682] restricted-volume nil
    [0 1 0 1 0 1] [2 3 2 3 2 3] nil))

(deftest cubes-test
  (are [cuboid expected] (= (into #{} expected) (into #{} (cubes cuboid)))
    [0 0 0 0 0 0] [[0 0 0]]
    [0 1 0 1 0 1] [[0 0 0] [0 0 1] [0 1 0] [0 1 1] [1 0 0] [1 0 1] [1 1 0] [1 1 1]]
    ))

(deftest part1-test
  (is (= 590784 (part1 test-data))))

(deftest part2-test
  (is (= nil (part2 test-data))))
