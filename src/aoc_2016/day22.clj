(ns aoc-2016.day22
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [aoc.algo :as algo]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (drop 2)
       (map parse-ints)
       (map (fn [[x y size used available percent-use]]
              {:pos [x y]
               :used used
               :available available}))))

;; part 1

(defn count-stacked [comparator coll]
  (loop [counts (sorted-map-by comparator)
         prev nil
         [val & more] coll]
    (if (nil? val)
      counts
      (recur (assoc counts val (inc (counts prev 0)))
             val
             more))))

(defn count-by-min-available [disks]
  (->> (map :available disks)
       (sort >)
       (count-stacked <)))

(defn count-by-used [disks]
  (->> (map :used disks)
       (remove zero?)
       frequencies
       (into (sorted-map-by <))))

(defn count-viable-pairs [disks]
  (loop [n 0
         [[next-used num-used] & more-used :as used] (count-by-used disks)
         [[next-available num-available] & more-available :as available] (count-by-min-available disks)]
    (cond
      (some empty? [used available]) n
      (< next-available next-used) (recur n used more-available)
      :else (recur (+ n (* num-used num-available)) more-used  available)
      )))

(defpart part1 [nodes]
  (count-viable-pairs nodes))

;; part 2

;; We can notice from looking at the input data, that we can only
;; move data to the currently free node, and the node we moved data
;; from we become the new free node.
;; This means, we essentially have to "move" the free node to [0,ymax-1]
;; (in front of the node we want to access), and then make "semi-circle"
;; moves around it to push it to [0,0]

(defn free-node [nodes]
  (find-first #(zero? (:used %)) nodes))

(defpart print-nodes [nodes]
  (let [free-size (:available (free-node nodes))
        draw-node (fn [paper node]
                    (assoc paper
                           (:pos node)
                           (cond
                             (zero? (:used node)) \_
                             (> (:used node) free-size) \#
                             :else \.)))]
    (-> (reduce draw-node
                {}
                nodes)
        s2/print)))

(defn draw-maze [nodes free-size]
  (into (reduce (fn [maze node]
                  (if (> (:used node) free-size)
                    (conj maze (:pos node))
                    maze))
                #{}
                nodes)
        (s2/polygon-points (s2/outter-box (map :pos nodes)))))

(defn nb-steps-to-move-in-front-of-target [nodes dest]
  (let [{free-size :available free-pos :pos} (free-node nodes)
        maze (draw-maze nodes free-size)]
    (s2/print (-> (s2/draw-points \# maze) (assoc free-pos \_ dest \@)))
    (s2/find-min-steps-in-maze free-pos dest maze)))

(defpart part2 [nodes]
  (let [[[x-min x-max] [y-min y-max]] (s2/x-and-y-ranges (map :pos nodes))]
    (+ (nb-steps-to-move-in-front-of-target nodes [(dec x-max) 0])
       (* 5 (dec x-max))
       1)))

;; tests

(deftest count-stacked-test
    (testing "ascending order"
    (are [sorted-coll expected] (= expected (count-stacked sorted-coll))
      [] {}
      [5] {5 1}
      [1 3] {1 1, 3 2}
      [1 2 2 2 3 6 6 7 9 9] {1 1, 2 4, 3 5, 6 7, 7 8, 9 10}))
  (testing "descending order"
    (are [sorted-coll expected] (= expected (count-stacked sorted-coll))
      [] {}
      [5] {5 1}
      [3 1] {3 1, 1 2}
      [9 9 7 6 6 3 2 2 2 1] {9 2, 7 3, 6 5, 3 6, 2 9, 1 10})))

(deftest part1-test)

(deftest part2-test)
