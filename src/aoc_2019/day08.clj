(ns aoc-2019.day08
  (:require
   [clojure.java.io :as io]
   [clojure.string :refer [join]]))

(def puzzle-input-8
  (->>
   (-> "2019-08.txt"
       io/resource
       io/reader
       line-seq)
   (mapcat #(seq %))))

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

(checksum puzzle-input-8)

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

(decode-image puzzle-input-8 width height)
