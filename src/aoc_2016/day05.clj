(ns aoc-2016.day05
  (:require
   [aoc.core :refer :all]
   [aoc.md5 :refer :all]))

(puzzle-input-string)

;; part 1

(defpart part1 [input]
  (->> (md5-seq input)
       (grep #"^0{5}")
       (take 8)
       (map #(get % 5))
       (apply str)))

;; part 2

(defn compose-password [digits]
  (->> digits
       (merge (zipmap (range 8) (repeat \-)) digits)
       (sort-by key)
       (map second)
       (apply str)))

(defpart part2 [input]
  (->> (md5-seq input)
       (grep  #"^0{5}[0-7]")
       (map #(vector (-> % (get 5) str parse-int) (get % 6)))
       (reductions
        (fn [state [pos char]]
          (if (contains? state pos)
            state
            (assoc state pos char)))
        {})
       (filter #(= 8 (count %)))
       first
       compose-password))

;; tests
