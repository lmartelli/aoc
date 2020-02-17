(ns aoc-2015.day17
  (:require
   [aoc.core :refer :all]))

(puzzle-input-parse-lines parse-int #(sort-by - %))

;; part 1

(defn sum [containers]
  (apply + containers))

(defn sub-seq [min-sum containers]
  (->> (map #(drop % containers) (range (count containers)))
       (take-while #(>= (sum %) min-sum))))

(defn list-combinations
  "Containers are order by decreasing size.
  For each container we associate the sum of all lower or equal ranked containers"
  [liters [size & others :as available-containers]]
  (cond
    ;; we're done
    (zero? liters) (list [])
    ;; all available containers are just enough
    (= (sum available-containers) liters) (list available-containers)
    :else
    (->> (drop-while #(> % liters) available-containers)
         (sub-seq liters)
         (mapcat
          (fn [[first & others]]
            (map #(conj % first)
                 (list-combinations (- liters first) others)))))))

(defpart part1 [input]
  (->> (list-combinations 150 input)
       count))

;; part 2

(defpart part2 [input]
  (->> (list-combinations 150 input)
       (map count)
       frequencies
       (apply min-key key)
       val))

;; tests
