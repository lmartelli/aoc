(ns aoc-2021.day03
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines stream digit-vec))

;; part 1

(defn most-common [& s]
  (->> (frequencies s)
       (sort-by val >)
       first
       key))

(defn gamma [input]
  (apply (partial map most-common) input))

(defn bit-inv [bits]
  (map {0 1, 1 0} bits))

(defn bits-to-decimal [bits]
  (-> (apply str bits)
      (parse-int 2)))

(defpart part1 [input]
  (let [g (gamma input)
        e (bit-inv g)]
    (->> [g e]
         (map bits-to-decimal)
         (apply *))))

;; part 2

(defn search-by-bit-criteria
  ([s criteria] (search-by-bit-criteria s criteria 0))
  ([s criteria pos]
   (if (= 1 (count s))
     (first s)
     (let [bit (criteria (map #(nth %1 pos) s))]
       (recur
         (filter #(= bit (nth %1 pos)) s)
         criteria
         (inc pos))))))

(defn- bit-criteria [bits more-ones more-zeros tie]
  (let [r (reduce (fn [acc v] (if (= 0 v) (dec acc) (inc acc))) 0 bits)]
    (cond
      (pos? r) more-ones
      (neg? r) more-zeros
      :else tie)))

(defn most-common-bit [bits]
  (bit-criteria bits 1 0 1))

(defn least-common-bit [bits]
  (bit-criteria bits 0 1 0))

(defn search-by-most-common-bit [s]
  (search-by-bit-criteria s #()))

(defpart part2 [input]
    (->> [(search-by-bit-criteria input most-common-bit)
          (search-by-bit-criteria input least-common-bit)]
         (map bits-to-decimal)
         (apply *)))

;; tests

(deftest gamma-test
  (is (= [1 0 1 1 0] (gamma (puzzle-input (test-input *ns*))))))

(deftest bit-inv-test
  (is (= [1 0 1 1 0] (bit-inv [0 1 0 0 1]))))

(deftest part1-test
  (is (= 198 (part1 (puzzle-input (test-input *ns*))))))

(deftest most-common-bit-test
  (are [bits expected] (= expected (most-common-bit bits))
    [1 0 1] 1
    [0 0 1] 0
    [0 1] 1))

(deftest least-common-bit-test
  (are [bits expected] (= expected (least-common-bit bits))
    [1 0 1] 0
    [0 0 1] 1
    [0 1] 0))

(deftest part2-test
  (is (= 230 (part2 (puzzle-input (test-input *ns*))))))
