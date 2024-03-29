(ns aoc-2015.rpg
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]))

(def props {"Hit Points" :hit-points, "Armor" :armor, "Damage" :damage, "Mana" :mana})

(defn transform-lines [lines]
  (let [[keys vals] (apply map vector lines)]
    (zipmap (map props keys) (map parse-int vals))))

(defn puzzle-input-rpg-properties [stream]
  (puzzle-input-parse-lines stream #(split % #": ") transform-lines))

(defn dead? [player]
  (<= (player :hit-points) 0))

(defn apply-damage [defender {:keys [damage]}]
  (update
    defender
    :hit-points
    - (max 1 (- damage (get defender :armor 0)))))
