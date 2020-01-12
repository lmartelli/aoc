(ns aoc-2019.day08
  (:require
   [aoc.core :refer :all]
   [clojure.java.io :as io]
   [clojure.string :refer [join]]))

(puzzle-input-parse seq)

;; part 1

(def width 25)
(def height 6)

(defn layers [data width height]
  (let [partition-size (* width height)]
    (partition partition-size partition-size nil data)))

(defn count-digit [layer digit]
  (count (filter #{digit} layer)))

(defn count-zeros [layers]
  (reduce #(assoc %1 %2 (count-digit %2 \0)) {} layers))

(defn key-for-min-value [m]
  (-> (apply min-key second m)
      first))

(defn checksum [data]
  (let [layer (-> data (layers width height) count-zeros key-for-min-value)]
    (* (count-digit layer \1)
       (count-digit layer \2))))

(defpart part1 [input]
  (checksum input))

;; part 2

(defn combine-layers [layers]
  (apply map (fn [& pixels] (some #(if (not= \2 %) %) pixels)) layers))

(defn display-image [pixels width]
  (->>
   (map #(apply str %) (partition width width nil (map {\0 \space, \1 \#, \2 \space} pixels)))
   (join "\n")
   (println)))

(defn decode-image [data width height]
  (-> data (layers width height) combine-layers (display-image width)))

(defpart part2 [input]
  (decode-image input width height))
