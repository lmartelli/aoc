(ns aoc.space-3d
  (:refer-clojure :exclude [+ -])
  (:require
   [clojure.core :as core]
   [aoc.core :refer [parse-int signum re-seq-parse min-max range-inc]]
   [aoc.algo :as algo]
   [clojure.math.numeric-tower :refer [sqrt]]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn parse-point [s]
  (->> (re-seq #"-?\d+" s)
       (map parse-int)
       (take 3)
       (apply vector)))

(defn + [[ax ay az] [bx by bz]]
  [(core/+ ax bx) (core/+ ay by) (core/+ az bz)])

(defn - [[ax ay az] [bx by bz]]
  [(core/- ax bx) (core/- ay by) (core/- az bz)])

(defn manatthan-dist
  ([a b] (manatthan-dist (- a b)))
  ([[^int x ^int y ^int z]] (core/+ (abs x) (abs y) (abs z))))

(defn prod "Scalar product of 2 vectors" [[ux uy uz] [vx vy vz]]
  (core/+ (* ux vx) (* uy vy) (* uz vz)))

(defn norm [[x y z]]
  (sqrt (core/+ (* x x) (* y y) (* z z))))

(defn cos [v u]
  (/ (prod u v) (* (norm u) (norm v))))

(defn direct-neighbours [[^int x ^int y ^int z]]
  (list [x (inc y) z]
        [x (dec y) z]
        [(inc x) y z]
        [(dec x) y z]
        [x y (inc z)]
        [x y (dec z)]))

(defmacro list-neighbours [x y z]
  (->> (for [X `[~x (inc ~x) (dec ~x)]
             Y `[~y (inc ~y) (dec ~y)]
             Z `[~z (inc ~z) (dec ~z)]]
         [X Y Z])
       (remove #(= % `[~x ~y ~z]))
       vec))

(defn all-neighbours [[^int x ^int y ^int z]]
  (list-neighbours x y z))

(defn rotate-x [[x y z]] [x z (core/- y)])

(defn rotate-y [[x y z]] [z y (core/- x)])

(defn rotate-z [[x y z]] [y (core/- x) z])

(defn xyz-ranges [positions]
  (let [[x0 y0 z0] (first positions)]
    (reduce
      (fn [[[x-min x-max] [y-min y-max] [z-min z-max]] [x y z]]
        [(min-max x-max x-min x)
         (min-max y-max y-min y)
         (min-max z-max z-min z)])
      [[x0 x0] [y0 y0] [z0 z0]]
      positions)))

(defn pos-in-ranges [[x-range y-range z-range :as xyz-ranges]]
  (for [x (apply range-inc x-range)
        y (apply range-inc y-range)
        z (apply range-inc z-range)]
    [x y z]))
