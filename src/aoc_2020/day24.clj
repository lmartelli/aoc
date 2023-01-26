(ns aoc-2020.day24
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (->> lines
       (map #(map keyword (re-seq #"e|se|sw|w|nw|ne" %)))))

;; part 1

;;
;;     ⅄   ⅄
;;    / \ / \
;;   Y   Y   Y
;;   |   | U |
;;   ⅄   ⅄   ⅄
;;  / \ / \ / \
;; Y   Y   Y   Y
;; | L | X | R |
;; ⅄   ⅄   ⅄   ⅄
;;  \ / \ / \ /
;;   Y   Y   Y
;;   | D |   |
;;   ⅄   ⅄   ⅄
;;    \ / \ /
;;     Y   Y
;;

(def east s2/right)
(def north-east s2/up)
(def north-west (comp s2/up s2/left))
(def west s2/left)
(def south-west s2/down)
(def south-east (comp s2/down s2/right))

(def move
  {:e east
   :w west
   :ne north-east
   :nw north-west
   :sw south-west
   :se south-east})

(defn path→coord [path]
  (reduce
    (fn [coord dir]
      ((move dir) coord))
    [0 0]
    path))

(defn init-black-tiles [paths]
  (->> paths
       (map path→coord)
       frequencies
       (filter-vals odd?)
       keys))

(defpart part1 [paths]
  (count (init-black-tiles paths)))

;; part 2

(defn count-alive-neighbours [alive? neighbours cell]
  (->> (neighbours cell)
       (filter alive?)
       count))

(defn next-gen [alive-cells neighbours]
  (set
    (concat
      (->> alive-cells
           (filter #(<= 1 (count-alive-neighbours alive-cells neighbours %) 2)))
      (->> alive-cells
           (mapcat neighbours)
           (distinct)
           (remove alive-cells)
           (filter #(= 2 (count-alive-neighbours alive-cells neighbours %)))))))

(defn nth-gen [gen-0 neighbours n]
  (-> (iterate #(next-gen % neighbours) gen-0 )
      (nth n)))

(defn hex-neighbours [coord]
  (map #(% coord) [east north-east north-west west south-west south-east]))

(defn count-nth-gen [paths n]
  (let [gen-0 (set (init-black-tiles paths))]
    (count (nth-gen gen-0 hex-neighbours n))))

(defpart part2 [paths]
  (count-nth-gen paths 100))

;; tests

(def data
  ["sesenwnenenewseeswwswswwnenewsewsw"
   "neeenesenwnwwswnenewnwwsewnenwseswesw"
   "seswneswswsenwwnwse"
   "nwnwneseeswswnenewneswwnewseswneseene"
   "swweswneswnenwsewnwneneseenw"
   "eesenwseswswnenwswnwnwsewwnwsene"
   "sewnenenenesenwsewnenwwwse"
   "wenwwweseeeweswwwnwwe"
   "wsweesenenewnwwnwsenewsenwwsesesenwne"
   "neeswseenwwswnwswswnw"
   "nenwswwsewswnenenewsenwsenwnesesenew"
   "enewnwewneswsewnwswenweswnenwsenwsw"
   "sweneswneswneneenwnewenewwneswswnese"
   "swwesenesewenwneswnwwneseswwne"
   "enesenwswwswneneswsenwnewswseenwsese"
   "wnwnesenesenenwwnenwsewesewsesesew"
   "nenewswnwewswnenesenwnesewesw"
   "eneswnwswnwsenenwnwnwwseeswneewsenese"
   "neswnwewnwnwseenwseesewsenwsweewe"
   "wseweeenwnesenwwwswnew"])

(deftest part1-test
  (test-with-lines part1 data 10))

(deftest count-nth-gen-test
  (let [paths (parse-input-lines data)]
    (are [n expected] (= expected (count-nth-gen paths n))
      1 15
      2 12
      3 25
      4 14
      5 23
      6 28
      10 37
      20 132
      30 259
      90 1844)))

(deftest part2-test
  (test-with-lines part2 data 2208))
