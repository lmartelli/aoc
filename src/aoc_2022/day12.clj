(ns aoc-2022.day12
  (:require
   [aoc.core :refer :all]
   [aoc.algo :as algo]
   [aoc.space-2d :as space-2d]
   [clojure.test :refer :all]))

(defn elevation [c]
  (- (int c) (int \a)))

(defn finalize-input [elevations]
  (let [input (->> (filter (fn [[pos v]] (#{:start :end} v)) elevations)
                   (map (juxt second first))
                   (into {}))]
    (assoc input
           :elevations
           (assoc elevations
                  (input :start) 0
                  (input :end) (elevation \z)))))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (array-2d-to-map
        identity
        (fn [c]
          (case c
            \S :start
            \E :end
            (elevation c))))
       finalize-input))

;; part 1

(defn count-steps [&{:keys [elevations start stop? allowed-step?]}]
  (-> (algo/explore :f elevations
                    :start start
                    :neighbours space-2d/direct-neighbours
                    :stop? (stop? last-visited)
                    :neighbour-allowed? (if-let [next-elevation (elevations neighbour-pos)]
                                          (allowed-step? (elevations pos) next-elevation)))
      :nb-steps))

(defpart part1 [{:keys [start end elevations]}]
  (count-steps :elevations elevations
               :start start
               :stop? (fn [last-visited] (last-visited end))
               :allowed-step? (fn [current-elevation next-elevation] (<= next-elevation (inc current-elevation)))))

;; part 2

(defpart part2 [{:keys [start end elevations]}]
  (count-steps :elevations elevations
               :start end
               :stop? (fn [last-visited] (some #(zero? (elevations %)) last-visited))
               :allowed-step? (fn [current-elevation next-elevation] (>= next-elevation (dec current-elevation)))))

;; tests

(deftest part1-test (part-test part1 31))

(deftest part2-test (part-test part2 29))
