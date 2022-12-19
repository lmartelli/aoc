(ns aoc-2022.day18
  (:require
   [aoc.core :refer :all]
   [aoc.algo :as algo]
   [aoc.space-3d :as s3]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (map s3/parse-point)
       (into #{})))

;; part 1

(defn count-neighbours [cube cubes]
  (->> (s3/direct-neighbours cube)
       (filter cubes)
       count))

(defn count-surface [cubes]
  (- (* 6 (count cubes))
     (->> cubes
          (map #(count-neighbours % cubes))
          (reduce +))))

(defpart part1 [lava-cubes]
  (count-surface lava-cubes))

;; part 2

(defn find-unreachable-cubes [lava-cubes]
    (let [[x-min x-max] (apply min-max (map first lava-cubes))
          [y-min y-max] (apply min-max (map second lava-cubes))
          [z-min z-max] (apply min-max (map third lava-cubes))
          reachable (-> (algo/explore :start [(dec x-min) (dec y-min) (dec z-min)]
                                  :neighbours s3/direct-neighbours
                                  :neighbour-allowed? (and (not (lava-cubes neighbour-pos))
                                                           (let [[x y z] neighbour-pos]
                                                             (and (<= (dec x-min) x (inc x-max))
                                                                  (<= (dec y-min) y (inc y-max))
                                                                  (<= (dec z-min) z (inc z-max))))))
                        :visited)
          reachable-plus-lava (into reachable lava-cubes)]
      (for [x (range-inc x-min x-max)
            y (range-inc y-min y-max)
            z (range-inc z-min z-max)
            :when (not (reachable-plus-lava [x y z]))]
        [x y z])
      ))

(defpart part2 [lava-cubes]
  (- (count-surface lava-cubes)
     (->> (find-unreachable-cubes lava-cubes)
          (map #(count-neighbours % lava-cubes))
          (reduce +))))

;; tests

(deftest count-surface-test
  (testing "disjoint cubes"
    (are [cubes expected] (= expected (count-surface cubes))
      #{} 0
      #{[0 0 0]} 6
      #{[0 0 0] [1 1 1]} 12))
  (testing "joint cubes"
    (are [cubes expected] (= expected (count-surface cubes))
      #{[0 0 0] [0 0 1]} 10
      #{[0 0 0] [3 0 0] [0 0 1] [3 0 1]} 20
      )
    )
  )

(deftest part1-test (part-test part1 64))

(deftest part2-test (part-test part2 58))
