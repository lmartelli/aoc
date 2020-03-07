(ns aoc-2017.day21
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split join]]
   [clojure.math.numeric-tower :refer [sqrt]]
   [clojure.test :refer :all]))

(defn parse-block [s]
  (mapv vec (split s #"/")))

(defn parse-rule [rule]
  (mapv parse-block (split rule #" => ")))

(puzzle-input-parse-lines parse-rule)

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
  (for [x (range size), y (range size)]
    [x y]))

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
  (transform flip-vert block))

(defn rotations [block]
  (->> block (iterate rotate) (take 4)))

(defn variations [block]
  (into #{}
        (mapcat rotations [block (flip block)])))

(defn as-strings [block]
  (->> block
       (map #(apply str %))))

(defn print-block [block]
  (->> block
       as-strings
       (join "\n")
       println)
  (println (apply str (repeat (get-size block) \╌)))
  block)

(defn print-blocks [blocks]
  (dorun
   (map #(print-block (second %)) blocks))
  blocks)

(defn complete-rules [rules]
  (->> rules
       (mapcat (fn [[from to]] (for [v (variations from)] [v to])))
       (into {})))

(defn apply-rules [rules blocks]
  (for [[pos block] blocks]
    [pos (rules block)]))

(defn copy [from from-pos from-size to to-pos]
  (reduce
   (fn [to coord]
     (set-pixel to
                (add to-pos coord)
                (get-pixel from (add coord from-pos))))
   to
   (indexes from-size)))

(defn mk-block [size]
  (init-matrix size size \.))

(defn sub-block [block top-left size]
  (copy block top-left size
        (mk-block size) [0 0]))

(defn split-block [block]
  (let [sub-block-size (if (multiple? (get-size block) 2) 2 3)
        range (range (/ (get-size block) sub-block-size))]
    (for [x range, y range]
      [[x y] (sub-block block (mult [x y] sub-block-size) sub-block-size)])))

(defn merge-blocks [blocks]
  (let [n (sqrt (count blocks))
        size (get-size (second (first blocks)))]
    (reduce
     (fn [res [pos block]] (copy block [0 0] size res (mult pos size)))
     (mk-block (* n size))
     blocks)))

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

(deftest copy-block-test
  (are [from from-pos from-size to to-pos expected]
      (= (read-block expected) (copy (read-block from) from-pos from-size to to-pos))
    ["┏┳┓" ;; from
     "┣╋┫"
     "┗┻┛"] [0 0] 3
    (mk-block 6) [1 2] ;; to
    ["......" ;; expected
     "......"
     ".┏┳┓.."
     ".┣╋┫.."
     ".┗┻┛.."
     "......"]

    ["......" ;; from
     "......"
     ".┏┳┓.."
     ".┣╋┫.."
     ".┗┻┛.."
     "......"] [1 2] 3
    (mk-block 3) [0 0] ;; to
    ["┏┳┓" ;; expected
     "┣╋┫"
     "┗┻┛"]))
