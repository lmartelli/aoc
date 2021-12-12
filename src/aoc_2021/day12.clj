(ns aoc-2021.day12
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.set :refer [difference]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines
    stream
    #(split % #"-")
    (fn [connections]
      (-> (concat connections (map reverse connections))
          (multimap #{})))))

;; part 1

(defn small? [cave]
  (Character/isLowerCase (first cave)))

(defn big? [cave]
  (not (small? cave)))

(defn valid-path? [path]
  (->> path
       (filter small?)
       frequencies
       vals
       (every? #{1})))

(defn find-paths
  ([cave-map valid-path?] (find-paths cave-map valid-path? ["start"]))
  ([cave-map valid-path? current-path]
   (cond
     (= (peek current-path) "end") [current-path]
     (not (valid-path? current-path)) []
     :else (let [neighbours (cave-map (peek current-path))]
             (mapcat #(find-paths cave-map valid-path? (conj current-path %)) neighbours)))))

(defpart part1 [cave-map]
  (-> (find-paths cave-map valid-path?)
      count))

;; part 2

(defpart part2 [cave-map]
  (let [valid-path? (fn [path]
                      (let [freq (frequencies (filter small? path))]
                        (and (< (or (freq "start") 0) 2)
                             (let [freq2 (->> freq vals frequencies)]
                               (and (<= (apply max (keys freq2)) 2)
                                    (<= (or (freq2 2) 0) 1))))))]
    (-> (find-paths cave-map valid-path?)
        count)))

;; tests

(defn test-data [n]
  (puzzle-input (test-input (str n) *ns*)))

(deftest part1-test
  (are [data-set expected] (= expected (part1 (test-data data-set)))
    1 10
    2 19
    3 226))

(deftest part2-test
  (are [data-set expected] (= expected (part2 (test-data data-set)))
    1 36
    2 103
    3 3509))
