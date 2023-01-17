(ns aoc-2018.day22
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.test :refer :all]))

(def-input-parser [[depth target]]
  {:depth (first (parse-ints depth))
   :target (parse-ints target)})

;; part 1

(def mouth [0 0])

(defn printable-map [m target]
  (s2/print-to-lines m {:mouth \M :target \T :rock \. :wet \= :narrow \|}))

(defn printable-cave-map [cave-map target]
  (-> cave-map
      (assoc mouth :mouth
             target :target)
      printable-map))

(defn geological-index
  [[x y :as pos] erosion-up erosion-left]
  (-> (cond
        (zero? y) (-> x (* 16807))
        (zero? x) (-> y (* 48271))
        :else (* erosion-up erosion-left))))

(defn erosion-level
  [geological-index depth]
  (-> geological-index
      (+ depth)
      (mod 20183)))

(defn build-erosion-level-map [depth target]
  (-> (reduce
        (fn [m pos]
          (assoc
            m pos
            (-> (geological-index pos (m (s2/up pos)) (m (s2/left pos)))
                (erosion-level depth))))
        {}
        (s2/positions-in-rect mouth target))
      (assoc target (erosion-level 0 depth))))

(defn build-risk-level-map [depth target]
  (-> (build-erosion-level-map depth target)
      (update-vals
        (fn [geological-index]
          (-> geological-index
              (mod 3))))))

(defn build-cave-map [depth target]
  (-> (build-risk-level-map depth target)
      (update-vals {0 :rock 1 :wet 2 :narrow})))

(defpart part1 [{:keys [depth target]}]
  (->> (build-risk-level-map depth target)
       vals
       (reduce +)))

;; part 2

(defpart part2 [input]
  nil)

;; tests

(deftest build-cave-map-test
  (let [depth 510]
    (testing "At the cave's mouth"
      (are [target expected] (= expected (printable-map (build-cave-map depth target) target))
        [2 2] [".=."
               ".|="
               ".=."]
        [0 0] ["."]
        [1 0] [".."]
        [2 0] [".=."]
        [1 1] [".="
               ".."]
        [15 0] [".=.|=.|.|=.|=|=."]
        [10 10] [".=.|=.|.|=."
                 ".|=|=|||..|"
                 ".==|....||="
                 "=.|....|.=="
                 "=|..==...=."
                 "=||.=.=||=|"
                 "|.=.===|||."
                 "|..==||=.|="
                 ".=..===..=|"
                 ".======|||="
                 ".===|=|===."]))))

(deftest part1-test (is (= 114 (part1 {:depth 510, :target [10 10]}))))
