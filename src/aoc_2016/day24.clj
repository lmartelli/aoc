(ns aoc-2016.day24
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.set :as set]
   [clojure.math.combinatorics :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (let [[walls locations]
        (reduce
          (fn [[walls locations :as res] [pos val]]
            (cond
              (= val \#) [(conj walls pos) locations]
              (digit? val) [walls (assoc locations (digit val) pos)]
              :else res))
          [#{} (sorted-map)]
          (s2/pos-and-values-seq (line-seq stream)))]
    {:walls walls :locations (vals locations)}))

;; part 1

(defn compute-distances
  ([locations walls]
   (loop [[from & destinations] locations
          distances {}]
     (if (empty? destinations)
       distances
       (recur
         destinations
         (reduce
           (fn [distances [dest distance]]
             (-> distances
                 (assoc-in [from dest] distance)
                 (assoc-in [dest from] distance)))
           distances
           (compute-distances from destinations walls))))))
  ([from destinations walls]
   (loop [distance 0
          distances {}
          visited #{}
          last-visited #{from}
          destinations (set destinations)]
     (if (empty? destinations)
       distances
       (let [reached-destinations (set/intersection last-visited destinations)]
         (recur
           (inc distance)
           (reduce (fn [distances dest]
                     (assoc distances dest distance))
                   distances
                   reached-destinations)
           (into visited last-visited)
           (->> last-visited
                (mapcat s2/direct-neighbours)
                (remove (some-fn visited walls))
                (into #{}))
           (set/difference destinations reached-destinations)))))))

(defpart part1 [{:keys [walls locations] :as input}]
  (let [distances (compute-distances locations walls)
        [start & to-visit] locations
        sum-distances (fn [path]
                        (->> (partition 2 1 path)
                             (map #(get-in distances %))
                             (reduce +)))]
    (->> (map #(sum-distances (cons start %)) (permutations to-visit))
         (apply min))))

;; part 2

(defpart part2 [{:keys [walls locations] :as input}]
  (let [distances (compute-distances locations walls)
        [start & to-visit] locations
        sum-distances (fn [path]
                        (->> (partition 2 1 path)
                             (map #(get-in distances %))
                             (reduce +)))]
    (->> (map #(sum-distances (concat [start] % [start])) (permutations to-visit))
         (apply min))))

;; tests

(deftest part1-test (part-test part1 14))

(deftest part2-test (part-test part2 20))
