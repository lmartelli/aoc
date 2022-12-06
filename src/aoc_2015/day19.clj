(ns aoc-2015.day19
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]))

(defn puzzle-input [stream]
  (let [lines (line-seq stream)]
    {:replacements (mapv #(str/split % #" => ") (drop-last 2 lines))
     :molecule (last lines)}))

;; part 1

(defn indexes-of [s value]
  (letfn [(index-of [prev-index]
            (str/index-of s value (if (nil? prev-index) 0 (inc prev-index))))]
    (take-while (comp not nil?) (rest (iterate index-of nil)))))

(defn replace [molecule [replaced replacement]]
  (map
   (fn [pos]
     (str (subs molecule 0 pos) replacement (subs molecule (+ pos (count replaced)))))
   (indexes-of molecule replaced)))

(defn replace-molecule [molecule replacements]
  (set (mapcat #(replace molecule %) replacements)))

(defpart part1 [{:keys [molecule replacements]}]
  (count (replace-molecule molecule replacements)))

;; part 2

(defn replace-molecules [molecules replacements]
  (set (mapcat #(replace-molecule % replacements) molecules)))

(defn smallest [col]
  (apply min-key count col))

(defpart part2 [{:keys [molecule replacements]}]
  (-> (iterate #(smallest (replace-molecule % (map reverse replacements)))
               molecule)
      (positions #(= % "e"))
      first))

;; tests
