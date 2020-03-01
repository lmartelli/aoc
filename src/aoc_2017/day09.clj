(ns aoc-2017.day09
  (:require
   [aoc.core :refer :all]))

(puzzle-input-string)

;; part 1

(defn unescape-garbage [[c & more]]
  (if (nil? c)
    nil
    (lazy-seq
     (if (= \! c)
       (unescape-garbage (rest more))
       (if (= \> c)
         (cons c more)
         (cons c (unescape-garbage more)))))))

(defn drop-garbage [stream]
  (rest (drop-while #(not= \> %) (unescape-garbage stream))))

(defn remove-garbage [[c & more]]
  (if (nil? c)
    nil
    (lazy-seq
     (if (= \< c)
       (remove-garbage (drop-garbage more))
       (cons c (remove-garbage more))))))

(defn score [stream]
  (loop [[c & more] stream, parent 0, total 0]
    (case c
      nil total
      \} (recur more (dec parent) total)
      \{ (recur more (inc parent) (+ total (inc parent)))
      \, (recur more parent total)
      )))

(defpart part1 [input]
  (-> input
      remove-garbage
      score))

;; part 2

(defn keep-garbage [[c & more]]
  (if (nil? c)
    nil
    (lazy-seq
     (if (= \< c)
       (let [[garbage after] (split-with #(not= \> %) (unescape-garbage more))]
         (concat garbage (keep-garbage (rest after))))
       (keep-garbage (drop-while #(not= \< %) more))))))

(defpart part2 [input]
  (-> input
      keep-garbage
      count))

;; tests
