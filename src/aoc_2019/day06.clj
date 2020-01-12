(ns aoc-2019.day06
  (:require
   [aoc.core :refer :all]))

(puzzle-input-parse-lines #(rest (re-matches #"(.*)\)(.*)" %)))

;; part 1

;; → {obj (sat ...), ...}
(defn build-map [orbits]
  (reduce
   #(let [[obj sat] %2] (update %1 obj conj sat))
   {}
   orbits))

(defn checksum [sat-map]
  (loop [result 0, hops 1, cur-objs ["COM"]]
    (let [satelites (mapcat #(sat-map %) cur-objs)]
      (if (empty? satelites)
        result
        (recur (+ result (* hops (count satelites)))
               (inc hops)
               satelites)))))

(defpart part1 [input]
  (checksum (build-map input)))

;; part 2

(defn build-orbit-map [input]
  (reduce
   #(let [[obj sat] %2] (assoc %1 sat obj))
   {}
   input))

(defn path [orbit-map start]
  (loop [path '(), cur start]
    (if (nil? cur)
      path
      (recur (conj path cur) (orbit-map cur)))))

(defn dist-path [a b]
  (loop [a a, b b]
    (if (not= (first a) (first b))
      (- (+ (count a) (count b)) 2)
      (recur (rest a) (rest b))
      )))

(defn dist [orbit-map a b]
  (dist-path
   (path orbit-map a)
   (path orbit-map b)))

(defpart part2 [input]
  (dist (build-orbit-map input) "YOU" "SAN"))
