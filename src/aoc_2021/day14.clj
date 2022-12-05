(ns aoc-2021.day14
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.test :refer :all]))

(defn pair-frequencies [template]
  (->> (partition 2 1 template)
       (map #(apply str %))
       frequencies))

(defn puzzle-input [stream]
  (let [[template _ & rules] (line-seq stream)]
    {:pair-frequencies (pair-frequencies template)
     :first-char (first template)
     :rules (->> (map #(let [[pair insertion] (split % #" -> ")] [pair (first insertion)]) rules)
                 (into {}))}))

;; part 1

(defn add-map-value [pair-frequencies key n]
  (update pair-frequencies key (fnil + 0) n))

(defn apply-rules [pair-frequencies rules]
  (->> (reduce
        (fn [new-pairs [[a b :as pair] c]]
          (if-let [pair-freq (pair-frequencies pair)]
            (-> new-pairs
                (add-map-value pair (- pair-freq))
                (add-map-value (str a c) pair-freq)
                (add-map-value (str c b) pair-freq))
            new-pairs))
        {}
        rules)
       (merge-with + pair-frequencies)))

(defn iterate-rules [{:keys [:pair-frequencies :rules]} nb-iter]
  (-> (iterate #(apply-rules % rules) pair-frequencies)
      (nth nb-iter)))

(defn char-frequencies [first-char pair-frequencies]
  (->> pair-frequencies
       (filter #(> (val %) 0))
       (map (fn [[[a b] freq]] [b freq]))
       (reduce (fn [char-frequencies [char freq]]
                 (add-map-value char-frequencies char freq))
               {first-char 1})))

(defn solve [input nb-iter]
  (->> (iterate-rules input nb-iter)
       (char-frequencies (input :first-char))
       vals
       ((juxt #(apply max %) #(apply min %)))
       (apply -)))

(defpart part1 [input]
  (solve input 10))

;; part 2

(defpart part2 [input]
  (solve input 40))

;; tests

(deftest iterate-rules-test
  (are [nb-iter expected]
      (= (pair-frequencies expected)
         (into {}
               (filter
                #(> (val %) 0)
                (iterate-rules (puzzle-input (test-input)) nb-iter))))
    1 "NCNBCHB"
    2 "NBCCNBBBCBHCB"
    3 "NBBBCNCCNBBNBNBBCHBHHBCHB"
    4 "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"))

(deftest part1-test (part-test part1 1588))

(deftest part2-test (part-test part2 2188189693529))
