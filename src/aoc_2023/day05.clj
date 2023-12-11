(ns aoc-2023.day05
  (:require
   [aoc.core :refer :all]
   [aoc.range-inc :as range]
   [clojure.test :refer :all]))

(defn parse-map [header & map-lines]
  (let [[source destination] (map keyword (re-parse header #"([a-z]+)-to-([a-z]+)"))]
    {:destination destination
     :source source
     :range-maps (->> map-lines
                      (map parse-ints)
                      (map #(zipmap [:dest-start :src-start :length] %)))}))

(defn puzzle-input [stream]
  (let [[[seeds-input] & maps-inputs] (split-seq empty? (puzzle-input-lines stream))]
    {:seeds (parse-ints seeds-input)
     :maps (->> (map #(apply parse-map %) maps-inputs)
                (index :source))}
    ))

;; part 1

(defn apply-range [n range]
  (let [delta (- n (range :src-start))]
    (when (and (>= delta 0)
             (< delta (range :length)))
      (+ (range :dest-start) delta))))

(defn apply-map [[type n] maps]
  (let [{:keys [range-maps destination]} (maps type)]
    [destination
     (or (some #(apply-range n %) range-maps)
         n)]))

(defn get-location [seed-num maps]
  (->> (iterate #(apply-map % maps) [:seed seed-num])
       (find-first #(= :location (first %)))
       second))

(defpart part1 [{:keys [seeds maps]}]
  (apply min (map #(get-location % maps) seeds)))

;; part 2

(defn apply-range-2 [r [[m-start :as m-r] m-dest]]
  (mapv (fn [n]
          (if (range/contains? m-r n)
            (+ m-dest (- n m-start))
            n))
        r))

(defn map-range [[r-start r-end :as r] [[m-start m-end] m-dest :as m]]
  (cond
    (< r-end m-start)
    [[r] nil m]
    
    (and (< r-start m-start) (< r-end m-end))
    [[[r-start (dec m-start)] (apply-range-2 [m-start r-end] m)] nil m]

    (and (< r-start m-start) (<= r-end m-end))
    [[[r-start (dec m-start)] (apply-range-2 [m-start m-end] m)] nil nil]
    
    (and (<= m-start r-start) (< r-end m-end))
    [[(apply-range-2 r m)] nil m]
    
    (and (<= m-start r-start) (= r-end m-end))
    [[(apply-range-2 r m)] nil nil]

    (and (<= m-start r-start m-end) (< m-end r-end))
    [[(apply-range-2 [r-start m-end] m)] [(inc m-end) r-end] nil]

    (< r-start m-start m-end r-end)
    [[[r-start (dec m-start)] (apply-range-2 [m-start m-end] m)] [(inc m-end) r-end] nil]

    (< m-end r-start)
    [[] r nil]))

(defn map-ranges [rs ms]
  (loop [mapped []
         rs rs
         ms ms]
    (cond
      (empty? rs) mapped
      (empty? ms) (concat mapped rs)
      :else (let [[new-mapped r-left m-left] (map-range (first rs) (first ms))]
              (recur (apply conj mapped new-mapped)
                     (if r-left (conj (rest rs) r-left) (rest rs))
                     (if m-left (conj (rest ms) m-left) (rest ms)))))))

(defn apply-maps [maps type ranges]
  (let [{:keys [range-maps destination]} (maps type)]
    [destination
     (map-ranges (sort ranges) (sort (map #(vector [(% :src-start) (+ (% :src-start) (dec (% :length)))] (% :dest-start)) range-maps)))]))

(defn get-locations [seed-ranges maps]
  (->> (iterate (fn [[type ranges]] (apply-maps maps type ranges)) [:seed seed-ranges])
       (find-first #(= :location (first %)))
       second))

(defpart part2 [{:keys [seeds maps]}]
  (->> (get-locations (->> seeds (partition 2 2) (map (fn [[start length]] (vector start (dec (+ start length)))))) maps)
       (map first)
       (apply min)))

;; tests

(deftest part1-test (part-test part1 35))

(def test-maps ((puzzle-input (test-input)) :maps))

(deftest apply-map-test
  (are [src-type src-num dest-type dest-num] (= [dest-type dest-num] (apply-map [src-type src-num] test-maps))
    :seed 79 :soil 81
    :seed 50 :soil 52
    :seed 55 :soil 57
    :seed 97 :soil 99
    :seed 49 :soil 49
    :seed 100 :soil 100
    :soil 81 :fertilizer 81
    :water 81 :light 74))

(deftest get-location-test
  (are [seed-num location] (= location (get-location seed-num test-maps))
    79 82
    14 43
    55 86
    13 35))

;;(deftest part1-test (test-with-lines part1 [""] nil))

(deftest part2-test (part-test part2 46))

(deftest map-range-test
      (are [r m res r-remain m-remain] (= [res r-remain m-remain] (map-range r m))
        ;; [ ]
        ;;    [   ]
        [0 1] [[2 3] 10] [[0 1]] nil [[2 3] 10]
        ;; [   ]
        ;;    [   ]
        [0 1] [[1 3] 10] [[0 0] [10 10]] nil [[1 3] 10]
        ;; [      ]
        ;;    [   ]
        [0 3] [[1 3] 10] [[0 0] [10 12]] nil nil
        ;; [ ]
        ;; [   ]
        [0 1] [[0 2] 10] [[10 11]] nil [[0 2] 10]
        ;;  [ ]
        ;; [   ]
        [1 2] [[0 3] 10] [[11 12]] nil [[0 3] 10]
        ;; [   ]
        ;; [   ]
        [0 2] [[0 2] 10] [[10 12]] nil nil
        ;;  [  ]
        ;; [   ]
        [1 2] [[0 2] 10] [[11 12]] nil nil
        ;;   [   ]
        ;; [   ]
        [1 3] [[0 2] 10] [[11 12]] [3 3] nil
        ;; [     ]
        ;;   [ ]
        [0 5] [[2 3] 10] [[0 1] [10 11]] [4 5] nil
        ;;     [  ]
        ;; [  ]
        [3 5] [[0 2] 10] [] [3 5] nil))

;;(deftest part2-test (test-with-lines part2 [""] nil))
