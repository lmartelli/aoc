(ns aoc-2017.day20
  (:require
   [aoc.core :refer :all]
   [clojure.math.combinatorics :refer [combinations]]
   [clojure.math.numeric-tower :refer [sqrt]]
   [clojure.string :refer [split]]))

(defn parse-vector [str]
  (as-> (subs str 3 (dec (count str))) <>
    (split <> #",")
    (mapv parse-int <>)))

(puzzle-input-split-lines
 #", "
 (fn [vectors]
   (->> (map parse-vector vectors)
        (zipmap [:position :velocity :acceleration]))))

;; part 1

(defpart part1 [particles]
  (->> particles
       (zipmap (range))
       (apply min-key (comp manatthan-dist :acceleration val))
       key))

;; part 2

(defn update-velocity [p]
  (update p :velocity add (p :acceleration)))

(defn update-position [p]
  (update p :position add (p :velocity)))

(defn update-particles [particles]
  (map (comp update-position update-velocity) particles))

(defn remove-collisions [particles]
  (->> particles
       (group-by :position)
       vals
       (filter #(= 1 (count %)))
       (map peek)))

(defn next-state [particles]
  (-> particles
      remove-collisions
      update-particles))

(defn convergence
  "We define a convergence factor based on the alignement of position, velocity and acceleration.
   AS it converges towards 0, the path of the particle converges towards its asymptote,
   which is a line having the direction of the acceleration passing at the origin."
  [p]
  (let [vectors (->> (vals p) (remove (eq [0 0 0])))]
    (case (count vectors)
      3 (->> (combinations vectors 2)
             (map #(apply cos %))
             range-width)
      2 (as-> (apply cos vectors) $
          (square $)
          (- 1 $)
          (sqrt $))
      0)))

(defpart part2 [particles]
  (->> (iterate next-state particles)
       (find-first #(> 0.01 (->> % (map convergence) (apply max))))
       count))

