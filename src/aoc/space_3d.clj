(ns aoc.space-3d
  (:refer-clojure :exclude [+ -])
  (:require
   [clojure.core :as core]
   [aoc.core :refer [parse-int signum re-seq-parse]]
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

(defn rotate-x [[x y z]] [x z (core/- y)])

(defn rotate-y [[x y z]] [z y (core/- x)])

(defn rotate-z [[x y z]] [y (core/- x) z])
