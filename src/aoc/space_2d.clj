(ns aoc.space-2d
  (:require
   [clojure.core :as core]
   [aoc.algo :as algo]))

(defn + [[ax ay] [bx by]]
  [(core/+ ax bx) (core/+ ay by)])

(defn - [[ax ay] [bx by]]
  [(core/- ax bx) (core/- ay by)])

(defn direct-neighbours [[x y]]
  (list [x (inc y)] [x (dec y)] [(inc x) y] [(dec x) y]))
