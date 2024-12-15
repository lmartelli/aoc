(ns aoc-2024.day05
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (let [[rules updates] (->> (puzzle-input-lines stream)
                             (split-seq empty?))]
    {:rules (->> rules
                 (map parse-ints)
                 (group-by first)
                 (map-vals (comp set #(map second %))))
     :updates (->> updates
                   (map parse-ints))}))

;; part 1

(defn get-indices [coll]
  (->> (map vector
            coll (range))
       (into {})))

(defn valid-update? [update rules]
  (let [indices (get-indices update)
        rule-valid? (fn [before after]
                      (let [i1 (indices before)
                            i2 (indices after)]
                        (or (nil? i1)
                            (nil? i2)
                            (< i1 i2))))]
    (every?
      (fn [[before afters]]
        (every?
          #(rule-valid? before %)
          afters))
      rules)))

(defn middle [v]
  (get v (-> v count dec (/ 2))))

(defn sum-middle-pages [updates]
  (->> updates
       (map middle)
       (reduce +)))

(defpart part1 [{:keys [rules updates]}]
  (sum-middle-pages
    (filter #(valid-update? % rules) updates)))

;; part 2

(defn fix-update [update rules]
  (vec (sort-by
         identity
         (fn [a b]
           (let [ra (rules a)]
             (if (and ra (ra b))
               -1
               (if ((rules b {}) a) 1 -1))))
         update)))

(defpart part2 [{:keys [rules updates]}]
  (->> updates
       (filter #(not (valid-update? % rules)))
       (map #(fix-update % rules))
       sum-middle-pages))

;; tests

(deftest part1-test (part-test part1 143))

;;(deftest part1-test (test-with-lines part1 [""] nil))

(deftest part2-test (part-test part2 123))

;;(deftest part2-test (test-with-lines part2 [""] nil))
