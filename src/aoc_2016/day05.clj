(ns aoc-2016.day05
  (:require
   [aoc.core :refer :all]
   [aoc.md5 :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-string stream))

;; part 1

(defn salted-md5-seq [input]
  (->> (range)
       (map #(md5 (str input %)))
       (map bytes-to-hex)))

(defpart part1 [input]
  (->> (salted-md5-seq input)
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
  (->> (salted-md5-seq input)
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

(deftest part1-test (is (= "18f47a30" (part1 "abc"))))

(deftest part2-test (is (= "05ace8e3" (part2 "abc"))))

