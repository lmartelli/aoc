(ns aoc.space-3d
  (:refer-clojure :exclude [+ -])
  (:require
   [clojure.core :as core]
   [aoc.core :refer [parse-int signum re-seq-parse]]
   [aoc.algo :as algo]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn + [[ax ay az] [bx by bz]]
  [(core/+ ax bx) (core/+ ay by) (core/+ az bz)])

(defn - [[ax ay az] [bx by bz]]
  [(core/- ax bx) (core/- ay by) (core/- az bz)])

(defn manatthan-dist
  ([a b] (manatthan-dist (- a b)))
  ([[^int x ^int y ^int z]] (core/+ (abs x) (abs y) (abs z))))

(defn direct-neighbours [[^int x ^int y ^int z]]
  (list [x (inc y) z]
        [x (dec y) z]
        [(inc x) y z]
        [(dec x) y z]
        [x y (inc z)]
        [x y (dec z)]))

(defn parse-point [s]
  (->> (re-seq #"-?\d+" s)
       (map parse-int)
       (take 3)
       (apply vector)))
