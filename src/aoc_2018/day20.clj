(ns aoc-2018.day20
  (:require
   [aoc.core :refer :all]
   [aoc.algo :as algo]
   [aoc.space-2d :as s2]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (let [s (str/join lines)]
      (subs s 1 (dec (count s)))))

;; part 1


(defn move [pos dir]
  (case dir
    \N (s2/north pos)
    \S (s2/south pos)
    \W (s2/west pos)
    \E (s2/east pos)))

;; Note : A parenthesis group is never followed by S,N,E or W

(defn build-directed-graph [regex]
  (if (re-find #"[^|]\)+[SWNE]" regex)
    (throw (Exception. (str "Cannot handle regex where ) is not after | and before is followed by a direction (S,W,N or E)"))))
  (loop [pos [0 0]
         G {}
         bifurcations '()
         [dir & more] regex]
    (if (nil? dir)
      G
      (case dir
        (\N \S \W \E) (let [next-pos (move pos dir)]
                        (recur next-pos
                               (if (G next-pos)
                                 G ;; Ignore backward route
                                 (update G pos conj next-pos))
                               bifurcations
                               more))
        \( (recur pos
                  G
                  (conj bifurcations pos)
                  more)
        \| (let [pos-at-bifurcation (peek bifurcations)]
             (recur pos-at-bifurcation
                    G
                    bifurcations
                    more))
        \) (let [pos-at-bifurcation (peek bifurcations)]
               (recur pos-at-bifurcation
                      G
                      (pop bifurcations)
                      more))))))

(defpart part1 [regex]
  (algo/tree-depth
        :root [0 0]
        :children (build-directed-graph regex)))

;; part 2

(defn count-rooms-at-min-dist [G min-dist]
  (loop [last-visited #{[0 0]}
         distance 0
         nb-rooms 0]
    (if (empty? last-visited)
      nb-rooms
      (recur
        (into #{} (mapcat G last-visited)) ;; the graph does not include backward directions
        (inc distance)
        (if (>= distance min-dist)
          (+ nb-rooms (count last-visited))
          nb-rooms)))))

(defpart part2 [regex]
  (-> (build-directed-graph regex)
      (count-rooms-at-min-dist 1000)))

;; tests

(deftest part1-test
  (are [regex expected] (= expected (part1 regex))
    "WNE" 3
    "N(EN|WN)" 3
    "ENWWW(NEEE|SSE(EE|N))" 10
    "ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN" 18
    "ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))" 23
    "WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))" 31))

(deftest part2-test)
