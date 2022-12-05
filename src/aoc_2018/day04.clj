(ns aoc-2018.day04
  (:require [aoc.core :refer :all]
            [clojure.string :refer [split-lines]]
            [clojure.algo.generic.functor :refer :all]
            [clojure.test :refer :all]))

(defn parse-line [l]
  (let [[_ timestamp wakes-up falls-asleep guard-id] (re-matches #"\[(.*)\] (?:(wakes up)|(falls asleep)|(?:Guard #(.*) begins shift))" l)]
    [timestamp
     (cond
       guard-id guard-id
       wakes-up :wakes-up
       falls-asleep :falls-asleep)]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines stream parse-line))

;; part 1

(defn right [s n]
  (subs s (- (count s) n)))

(defn get-minutes [timestamp]
  (parse-int (right timestamp 2)))

(defn make-events [input]
  (:events
   (reduce
    (fn [{:keys [guard events] :as state} [timestamp event]]
      (if (keyword? event)
        (update state :events conj [timestamp guard event])
        (-> state
            (update :events conj [timestamp event :wakes-up])
            (assoc :guard event))
        ))
    {:events []}
    (into (sorted-map) input))))

(defn sleep-intervals [events]
  (:sleep-times
   (reduce
    (fn [{:keys [last-timestamp last-guard sleep-times] :as state} [timestamp guard event]]
      (if (and (= last-guard guard) (= :wakes-up event))
        (-> state
            (dissoc :last-guard)
            (update-in [:sleep-times guard] #(conj %1 [(get-minutes %2) (get-minutes %3)]) last-timestamp timestamp))
        (assoc state :last-timestamp timestamp :last-guard guard)))
    {:sleep-times {} }
    events)))

(defn sum-sleep-times [intervals]
  (reduce + (map (fn [[start end]] (- end start)) intervals)))

(defn find-most-asleep-guard [events]
  (apply max-key #(sum-sleep-times (val %))
         (sleep-intervals events)))

(defpart part1 [input]
  (let [[guard intervals]
        (->> input
             make-events
             sleep-intervals
             (apply max-key #(sum-sleep-times (val %))))]
    (->> intervals
         (mapcat #(apply range %))
         frequencies
         (apply max-key val)
         key
         (* (parse-int guard)))))


;; part 2

(defpart part2 [input]
  (let [[minute [freq guard]]
        (->> input
             make-events
             sleep-intervals
             (map (fn [[guard intervals]]
                    (->> intervals
                         (mapcat #(apply range %))
                         frequencies
                         (fmap #(vector % guard)))))
             (apply merge-with
                    (fn [& freqs]
                      (apply max-key first freqs)))
             (apply max-key
                    #(first (val %))))]
    (* minute (parse-int guard))))

  
;; tests

(deftest part1-test (part-test part1 240))

(deftest part1-test (part-test part2 4455))
