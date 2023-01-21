(ns aoc-2020.day16
  (:require
   [aoc.core :refer :all]
   [aoc.algo :as algo]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (let [[rules [_ your-ticket] [_ & nearby-tickets]] (split-seq empty? lines)]
    {:rules (->> rules
                 (re-parse-lines
                   #"(.*):(.*)"
                   #(vector %1 (->> (parse-pos-ints %2) (partition 2))))
                 (into {}))
     :your-ticket (parse-ints your-ticket)
     :nearby-tickets (map parse-ints nearby-tickets)}))

;; part 1

(defn ranges-values [ranges]
  (->>  (mapcat #(apply range-inc %) ranges)
        (into #{})))

(defn valid-values [rules]
  (->> (vals rules)
       (apply concat)
       ranges-values))

(defpart part1 [{:keys [rules nearby-tickets]}]
  (let [valid-values (valid-values rules)]
    (->> nearby-tickets
         (apply concat)
         (remove valid-values)
         (reduce +))))

;; part 2

(defn valid-ticket [rules]
  (let [valid-values (valid-values rules)]
    (fn [ticket-values]
      (every? valid-values ticket-values))))

(defn deduce-fields-num->name [{:keys [rules nearby-tickets]}]
  (let [possible-field-names-for-value (multimap-invert (->> rules (map-vals ranges-values)))]
    (algo/resolve-bijection-from-samples
      (->> nearby-tickets
           (filter (valid-ticket rules))
           (mapcat
             (fn [ticket-values]
               (map-indexed
                 (fn [field-num field-value]
                   [field-num (possible-field-names-for-value field-value)])
                 ticket-values)))))))

(defpart part2 [{:keys [rules your-ticket nearby-tickets] :as input}]
  (let [field-num->name (deduce-fields-num->name input)]
    (->> (zipmap (map field-num->name (range (count rules))) your-ticket)
         (filter-keys #(str/starts-with? % "departure"))
         vals
         (reduce *))))

;; tests

(deftest part1-test
  (test-with-lines
    part1
    ["class: 1-3 or 5-7"
     "row: 6-11 or 33-44"
     "seat: 13-40 or 45-50"
     ""
     "your ticket:"
     "7,1,14"
     ""
     "nearby tickets:"
     "7,3,47"
     "40co,4,50"
     "55,2,20"
     "38,6,12"]
    71))

(def data-2
  ["class: 0-1 or 4-19"
   "row: 0-5 or 8-19"
   "seat: 0-13 or 16-19"
   ""
   "your ticket:"
   "11,12,13"
   ""
   "nearby tickets:"
   "3,9,18"
   "15,1,5"
   "5,14,9"])

(deftest deduce-fields-num->name-test
  (test-with-lines deduce-fields-num->name data-2 {0 "row", 1 "class", 2 "seat"}))

(deftest part2-test
  (test-with-lines
    part2
    [""]
    nil))
