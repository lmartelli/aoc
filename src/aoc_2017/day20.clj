(ns aoc-2017.day20
  (:require
   [aoc.core :refer :all]
   [aoc.space-3d :as s3]
   [clojure.math.combinatorics :refer [combinations]]
   [clojure.math.numeric-tower :refer [sqrt]]
   [clojure.string :refer [split]]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (map (fn [line]
              (->> (parse-ints line)
                   (partition 3)
                   (map vec)
                   (zipmap [:position :velocity :acceleration]))))))

;; part 1

(defpart part1 [particles]
  (->> particles
       (zipmap (range))
       (apply min-key (comp s3/manatthan-dist :acceleration val))
       key))

;; part 2

(defn update-velocity [p]
  (update p :velocity s3/+ (p :acceleration)))

(defn update-position [p]
  (update p :position s3/+ (p :velocity)))

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
             (map #(apply s3/cos %))
             range-width)
      2 (as-> (apply s3/cos vectors) $
          (square $)
          (- 1 $)
          (sqrt $))
      0)))

(defpart part2 [particles]
  (->> (iterate next-state particles)
       (find-first #(> 0.01 (->> % (map convergence) (apply max))))
       count))

