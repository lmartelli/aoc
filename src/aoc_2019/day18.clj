(ns aoc-2019.day18
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [aoc.algo :as algo]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (let [m (->> (s2/pos-and-values-seq lines)
               (filter (comp (some-fn #{\. \@} letter?) second))
               (into {}))]
    {:tunnels (algo/adjacency-graph (set (keys m)) s2/direct-neighbours)
     :keys (->> m (filter-vals lower-case?) (map-vals (comp keyword str)))
     :doors (->> m (filter-vals upper-case?) (map-vals (comp keyword str)))
     :entrance (first (keys (filter-vals (eq \@) m)))}))

;; part 1

(defn distances-to-keys [{tunnels :tunnels key-at :keys} start]
  (loop [distances (if-let [key (key-at start)]
                     {key 0}
                     {})
         last-visited #{start}
         cur-dist 0]
    (if (= (count key-at) (count distances))
      (dissoc distances (key-at start))
      (let [next-visited (->> (mapcat tunnels last-visited)
                              (remove last-visited))]
        (recur
          (into distances (->> next-visited
                               (filter key-at)
                               (map #(vector (key-at %) (inc cur-dist)))))
          (into last-visited next-visited)
          (inc cur-dist))))))

(defpart part1 [tunnels-map]
  (s2/print tunnels-map :pading 1 :background \#))

;; part 2

(defpart part2 [input]
  nil)

;; tests

(def datas
  [;; 0
   ["#########"  
    "#b.A.@.a#"
    "#########"]
   ;; 1   
   ["########################"
    "#f.D.E.e.C.b.A.@.a.B.c.#"
    "######################.#"
    "#d.....................#"
    "########################"]
   ;; 2
   ["########################"
    "#...............b.C.D.f#"
    "#.######################"
    "#.....@.a.B.c.d.A.e.F.g#"
    "########################"]
   ;; 3
   ["#################"
    "#i.G..c...e..H.p#"
    "########.########"
    "#j.A..b...f..D.o#"
    "########@########"
    "#k.E..a...g..B.n#"
    "########.########"
    "#l.F..d...h..C.m#"
    "#################"]])

(deftest distances-to-keys-test
  (are [input from expected] (= expected (distances-to-keys (parse-input-lines input) from))
    (datas 0) [5 1] {:a 2 :b 4}
    (datas 0) [1 1] {:a 6}
    (datas 3) [8 4] {:a 3 :b 3 :c 5 :d 5 :e 5 :f 3 :g 3 :h 5 :i 10 :j 8 :k 8 :l 10 :m 10 :n 8 :o 8 :p 10}))

(deftest part1-test
  (test-with-lines
    part1
    (datas 0) 8
    (datas 1) 86
    (datas 2) 132
    (datas 3) 136))

(deftest part2-test
  (test-with-lines
    part2
    [""]
    nil))
