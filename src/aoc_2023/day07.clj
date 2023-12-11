(ns aoc-2023.day07
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (puzzle-input-lines stream)
       (map #(str/split % #" "))
       (map #(update % 1 parse-int))))

;; part 1

(defn hand-type [hand]
  (case (-> (frequencies hand) vals sort)
    [5] :five-of-a-kind
    [1 4] :four-of-a-kind
    [2 3] :full-house
    [1 1 3] :three-of-kind
    [1 2 2] :two-pairs
    [1 1 1 2] :one-pair
    :high-card))

(defn build-ranking [x]
  (->> (reverse x)
       (map-indexed #(vector %2 %1))
       (into {})))

(def hand-type-strength
  (build-ranking [:five-of-a-kind
                  :four-of-a-kind
                  :full-house
                  :three-of-kind
                  :two-pairs
                  :one-pair
                  :high-card]))

(def card-value
  (build-ranking "AKQJT98765432"))

(defn compare-hands [h1 h2 hand-type card-value]
  (apply compare (mapv (juxt (comp hand-type-strength hand-type) #(mapv card-value %)) [h1 h2])))

(defn total-winnings [input hand-type card-value]
  (->> (sort-by first #(compare-hands %1 %2 hand-type card-value) input)
       (map-indexed #(* (inc %1) (second %2)))
       (reduce +)))

(defpart part1 [input]
  (total-winnings input hand-type card-value))

;; part 2

(def card-value-with-joker
  (build-ranking "AKQT98765432J"))

(def hand-type-with-joker
  (memoize
   (fn [hand]
     (let [freqs (frequencies hand)]
       (hand-type
        (if (< 0 (get freqs \J 0) 5)
          (str/replace hand \J (->> (dissoc freqs \J) (apply max-key val) key))
          hand))))))

(defpart part2 [input]
  (total-winnings input hand-type-with-joker card-value-with-joker))

;; tests

(deftest part1-test (part-test part1 6440))

(deftest day6-test
  (testing "Hand type"
    (are [hand type] (= type (hand-type hand))
      "22222" :five-of-a-kind
      "33133" :four-of-a-kind
      "62662" :full-house
      "62661" :three-of-kind
      "44551" :two-pairs
      "17753" :one-pair
      "12345" :high-card))
  (testing "Hand type with Joker"
    (are [hand type] (= type (hand-type-with-joker hand))
      "12345" :high-card
      "32T3K" :one-pair
      "KK677" :two-pairs
      "T55J5" :four-of-a-kind
      "KTJJT" :four-of-a-kind
      "665JJ" :four-of-a-kind
      "4JA46" :three-of-kind
      "JJJJJ" :five-of-a-kind)))

(deftest part2-test (part-test part2 5905))
