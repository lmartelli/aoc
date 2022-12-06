(ns aoc-2015.day14
  (:require
   [aoc.core :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines
   stream
   (fn [line]
     (let [[_ reindeer speed run-time rest-time]
           (re-matches #"([^ ]*).* (\d+) .* (\d+) .* (\d+) .*" line)]
       {:name reindeer
        :speed (parse-int speed)
        :run-time (parse-int run-time)
        :rest-time (parse-int rest-time)}))))

;; part 1

(defn dist-after [{:keys [speed run-time rest-time]} time]
  (let [period (+ run-time rest-time)]
    (* speed
       (+ (* (quot time period) run-time)
          (min run-time (rem time period))))))

(defn max-dist [reindeers time]
  (->> reindeers
       (map #(dist-after % time))
       (apply max)))

(defpart part1 [input]
  (max-dist input 2503))

;; part 2

(defn lead-after [reindeers time]
  (->> reindeers
       (map #(vector (dist-after % time) (% :name)))
       multimap
       (apply max-key key)
       val))

(defpart part2 [input]
  (->> (map #(lead-after input %) (range 1 2504))
       (apply concat)
       frequencies
       (apply max-key val)
       val))

;; tests
