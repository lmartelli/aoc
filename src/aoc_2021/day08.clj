(ns aoc-2021.day08
  (:require
   [aoc.core :refer :all]
   [clojure.set :refer [difference]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-split-lines
    stream
    #"[ |]+"
    (fn [items]
      (as-> items $
        (map set $)
        (hash-map :signals (take 10 $) :segments (drop 10 $))))))

;; part 1

(defpart part1 [input]
  (->> input
       (mapcat :segments)
       (map count)
       (filter #{2 3 4 7})
       count))

;; part 2

(def digit-mappings
  (->> ["abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg"]
       (map-indexed #(vector %2 %1))
       (into {})
       (map-keys set)))

(def possible-mappings-by-count
  (->> digit-mappings
       keys
       (group-by count)))

(defn multimap-set [entries]
  (multimap entries #{}))

(defn find-mappings [signals]
  (let [signals-by-size (group-by count signals)
        signal-1 (get-in signals-by-size [2 0])
        signal-4 (get-in signals-by-size [4 0])
        signal-7 (get-in signals-by-size [3 0])
        signal-8 (get-in signals-by-size [7 0])
        a (first (difference signal-7 signal-1))
        [b e] (as-> signals-by-size $
                (get $ 5)
                (apply concat $)
                (frequencies $)
                (filter #(= (val %) 1) $)
                (map key $)
                [(find-first signal-4 $)
                 (find-first (comp not signal-4) $)])
        d (first (difference signal-4 signal-1 #{b}))
        g (first (difference signal-8 #{a b d e} signal-1))
        c (->> (get signals-by-size 6)
               (apply concat)
               frequencies
               (filter #(= (val %) 2))
               (map key)
               (find-first (comp not #{d e})))
        f (first (difference signal-1 #{c}))]
    {a \a, b \b, c \c, d \d, e \e, f \f, g \g}))

(defn decode-digit [signal mapping]
  (-> (map mapping signal)
      set
      digit-mappings))

(defn decode-line [{:keys [:signals :segments]}]
  (let [mapping (find-mappings signals)]
    (->> segments
         (map #(decode-digit % mapping))
         (apply str)
         parse-int)))

(defpart part2 [input]
  (->> input
       (map decode-line)
       (reduce +)))

;; tests

(deftest part1-test (part-test part1 26))

(deftest decode-line-test
  (is (= [8394 9781 1197 9361 4873 8418 4548 1625 8717 4315]
         (map decode-line (puzzle-input (test-input))))))

(deftest part2-test (part-test part2 61229))
