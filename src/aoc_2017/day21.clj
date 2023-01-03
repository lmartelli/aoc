(ns aoc-2017.day21
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.string :refer [split join]]
   [clojure.math.numeric-tower :refer [sqrt]]
   [clojure.test :refer :all]))

(defn parse-block [s]
  (mapv vec (split s #"/")))

(defn parse-rule [rule]
  (mapv parse-block (split rule #" => ")))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines stream parse-rule))

(defn read-block [strings]
  (mapv vec strings))

(def start-pattern
  (read-block
   [".#."
    "..#"
    "###"]))

;; part 1

(defn get-size [block]
  (count block))

(defn get-pixel [block [x y]]
  (get-in block [y x]))

(defn set-pixel [block [x y] value]
  (assoc-in block [y x] value))

(defn indexes [size]
  (let [range (range size)]
    (for [y range, x range]
      [x y])))

(defn transform [tx block]
  (let [size (get-size block)
        origin (center [(dec size) (dec size)])]
    (reduce
      (fn [res coord]
        (set-pixel res (tx coord origin) (get-pixel block coord)))
      block
      (indexes size))))

(defn rotate [block]
  (transform rotate-right block))

(defn flip [block]
  (transform s2/flip-vert block))

(defn rotations [block]
  (->> block (iterate rotate) (take 4)))

(defn variations [block]
  (mapcat rotations [block (flip block)]))

(defn as-strings [block]
  (->> block
       (map #(apply str %))))

(defn frame [strings]
  (let [size (count (first strings))]
    (concat [(str \┏ (apply str (repeat size \━)) \┓)]
            (map #(str \┃ % \┃) strings)
            [(str \┗ (apply str (repeat size \━)) \┛)])))

(defn print-block [block]
  (->> block
       as-strings
       frame
       (join "\n")
       println)
  block)

(defn print-blocks [blocks]
  (dorun
   (map #(print-block %) blocks))
  blocks)

(defn complete-rules [rules]
  (->> rules
       (mapcat (fn [[from to]] (for [v (variations from)] [v to])))
       (into {})))

(defn apply-rules [rules blocks]
  (map rules blocks))

(defn split-row [row]
  (let [size (count row)]
    (->> row
         (map #(partition size %))
         (apply map vector))))

(defn split-block [block]
  (let [size (if (multiple? (get-size block) 2) 2 3)]
    (->> block
         (partition size)
         (mapcat split-row))))

(defn concat-into [& vectors]
  (reduce into vectors))

(defn concat-blocks [blocks]
  (apply map concat blocks))

(defn merge-blocks [blocks]
  (let [n (sqrt (count blocks))]
    (->> blocks
         (partition n)
         (mapcat concat-blocks)
         vec)))

(defn iter [block rules]
  (->> block
       split-block
       #_(print-blocks)
       (apply-rules rules)
       merge-blocks
       #_(print-block)))

(defn on? [pixel] (= \# pixel))

(defn count-pixels [rules start n]
  (->> (nth (iterate #(iter % (complete-rules rules)) start) n)
       (apply concat)
       (filter on?)
       count))

(defpart part1 [rules]
  (count-pixels rules start-pattern 5))

;; part 2

(defpart part2 [rules]
  (count-pixels rules start-pattern 18))

;; tests

(deftest split-block-test
  (are [block expected] (= (map read-block expected)
                           (split-block (read-block block)))
    ["01"
     "45"]
    [["01"
      "45"]]
    ["012"
     "345"
     "678"]
    [["012"
      "345"
      "678"]]
    ["0123"
     "4567"
     "89ab"
     "cdef"]
    [["01" "45"]
     ["23" "67"]
     ["89" "cd"]
     ["ab" "ef"]]))

(deftest concat-blocks-test
  (are [blocks expected]
      (= (read-block expected) (concat-blocks (map read-block blocks)))
    [["01"
     "45"]
     ["23"
      "67"]]
    ["0123"
     "4567"]))

(deftest merge-blocks-test
  (are [blocks expected]
      (= (read-block expected) (merge-blocks (map read-block blocks)))
    [["01"
      "45"]
     ["23"
      "67"]
     ["89"
      "cd"]
     ["ab"
      "ef"]]
    ["0123"
     "4567"
     "89ab"
     "cdef"]))
