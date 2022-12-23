(ns aoc-2022.day23
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       s2/pos-and-values-seq
       (filter (fn [[pos value]] (= \# value)))
       (map first)
       (into #{})))

;; part 1

(def directions {:south [0 1] :north [0 -1] :east [1 0] :west [-1 0]})

(defn neighbours [pos elves]
  (let [neighbour-directions [[0 1] [1 1] [1 0] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]]]
    (set (filter #(elves (s2/+ % pos)) neighbour-directions))))

(defn elf-proposal [elf directions elves]
  (let [neighbours (neighbours elf elves)]
    (if (empty? neighbours)
      elf
      (if-let [dir (find-first #(not (or (neighbours %)
                                         (neighbours (s2/+ % (s2/rotate-left %)))
                                         (neighbours (s2/+ % (s2/rotate-right %)))))
                               directions)]
        (s2/+ elf dir)
        elf))))

(defn elf-proposals [elves directions]
  (->> elves
       (group-by #(elf-proposal % directions elves))
       (reduce
         (fn [next-positions [proposal candidates]]
           (if (= 1 (count candidates))
             (conj next-positions proposal)
             (apply conj next-positions candidates)))
         #{})))

(defn simul-seq [elves directions]
  (reductions
    elf-proposals
    elves
    (->> directions
         cycle
         (partition (count directions) 1))))

(defn count-empty-tiles [elves]
  (let [[x-min x-max] (apply min-max (map first elves))
        [y-min y-max] (apply min-max (map second elves))]
    (- (* (inc (- x-max x-min))
          (inc (- y-max y-min)))
       (count elves))))

(defn print-elves [elves]
  (s2/print (s2/draw-points \# elves))
  elves)

(def initial-directions (map directions [:north :south :west :east]))

(defpart part1 [elves]
  (-> (simul-seq elves initial-directions)
      (nth 10)
      count-empty-tiles))

;; part 2

(defpart part2 [elves]
  (->> (simul-seq elves initial-directions)
       (partition 2 1)
       (first-index #(apply = %))
       inc))

;; tests

(deftest elf-proposals-test
  (let [directions [[0 1] [0 -1] [1 0] [-1 0]]]
    (testing "Elf with no neighbour does not move"
      (is (=  #{[0 0]} (elf-proposals #{[0 0]} directions))))
    (testing "Elves move in 1st non crowded direction"
      (are [elves expected] (= expected (elf-proposals elves directions))
        #{[0 0] [0 1]} #{[0 -1] [0 2]}
        #{[0 0] [0 1] [1 -1]} #{[-1 0] [0 2] [1 -2]}))
    (testing "2 elves cannot move to the same position"
      (are [elves expected] (= expected (elf-proposals elves directions))
        #{[0 1] [0 2] [0 4] [0 5]} #{[0 0] [0 2] [0 4] [0 6]}
        ))
    ))

(deftest part1-test (part-test part1 110))

(deftest part2-test (part-test part2 20))
