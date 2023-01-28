(ns aoc.space-2d
  (:refer-clojure :exclude [+ -])
  (:require
   [clojure.core :as core]
   [aoc.core :refer [signum min-max range-inc range-expand]]
   [aoc.algo :as algo]
   [clojure.math.numeric-tower :refer [round sqrt]]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn positions-in-rect
  "Enumerates [x y] positions inside a rectangle, from the top left to the bottom right,
  in /reading order/ (left to right, top to bottom. x varies first)"
  [[x1 y1] [x2 y2]]
  (for [y (range-inc y1 y2)
        x (range-inc x1 x2)]
    [x y]))

(defn pos-and-values-seq
  "Generates a sequence of [[x y] value] from a seq of seq (rows).
  Y axis is pointing down."
  [rows]
  (mapcat
    (fn [y row]
      (map-indexed (fn [x val] [[x y] val]) row))
    (range) rows))

(defn row-col-and-values-seq
  "Generates a sequence of [[row col] value] in *reading order* (top-top-bottom, left-to-roght)."
  [rows]
  (mapcat
    (fn [y row]
      (map-indexed (fn [x val] [[y x] val]) row))
    (range) rows))

(defn parse-2d-map-positions
  "Returns positions of chars (# by default) in a 2D map made of rows."
  ([lines] (parse-2d-map-positions lines \#))
  ([lines char]
   (mapcat
     (fn [y line]
       (for [[x c] (map-indexed vector line)
            :when (= c char)]
        [x y]))
     (range)
     lines)))

(defn +
  ([[ax ay] [bx by]]
   [(core/+ ax bx) (core/+ ay by)])
  ([a b & more]
   (reduce
     +
     (+ a b)
     more)))

(defn -
  ([[ax ay]]
   [(core/- ax) (core/- ay)])
  ([[ax ay] [bx by]]
   [(core/- ax bx) (core/- ay by)]))

(defn- transform-relative [p origin tx]
  (-> p
      (- origin)
      tx
      (+ origin)))

(defn rotate-left "Y axis points down"
  ([[x y]] [y (core/- x)])
  ([p center] (transform-relative p center rotate-left)))

(defn rotate-right "Y axis points down"
  ([[x y]] [(core/- y)  x])
  ([p center] (transform-relative p center rotate-right)))

(defn flip-vert "Y axis points down"
  ([[x y]] [(core/- x) y])
  ([p [cx cy]] (transform-relative p [cx 0] flip-vert)))

(defn flip-horiz "Y axis points down"
  ([[x y]] [x (core/- y)])
  ([p [cx cy]] (transform-relative p [0 cy] flip-horiz)))

(defn mult "Vector multiplication by a number: v × n"
  [[x y] n]
  [(* x n) (* y n)])

(defn center [v]
  (mult v (/ 1 2)))

(defn move [pos dir dist]
  (+ pos (mult dir dist)))

(defn manatthan-dist
  ([a b] (manatthan-dist (- a b)))
  ([[^int x ^int y]] (core/+ (abs x) (abs y))))

(defn prod "Scalar product of 2 vectors" [[ux uy] [vx vy]]
  (core/+ (* ux vx) (* uy vy)))

(defn norm [[x y]]
  (sqrt (core/+ (* x x) (* y y))))

(defn cos [v u]
  (/ (prod u v) (* (norm u) (norm v))))

(defn north [[x y]]
  [x (dec y)])

(defn south [[x y]]
  [x (inc y)])

(defn east [[x y]]
  [(inc x) y])

(defn west [[x y]]
  [(dec x) y])

(defn north-east [[x y]]
  [(inc x) (dec y)])

(defn north-west [[x y]]
  [(dec x) (dec y)])

(defn south-east [[x y]]
  [(inc x) (inc y)])

(defn south-west [[x y]]
  [(dec x) (inc y)])

(def up north)
(def down south)
(def left west)
(def right east)

(def direction-fns-with-diags [north north-east east south-east south south-west west north-west])

(def direction-vectors
  {:north [0 -1]
   :south [0 1]
   :east [1 0]
   :west [-1 0]
   :north-east [1 -1]
   :north-west [-1 -1]
   :south-east [1 1]
   :south-west [-1 1]})

(defn direct-neighbours
  ([[^int x ^int y]]
   (list [x (inc y)] [x (dec y)] [(inc x) y] [(dec x) y]))
  ([p [x-min x-max] [y-min y-max]]
   (->> (direct-neighbours p)
        (filter (fn [[x y]]
                  (and (<= x-min x x-max)
                       (<= y-min y y-max)))))))

(defn all-neighbours [[^int x ^int y]]
  [[x (inc y)] [x (dec y)] [(inc x) y] [(dec x) y]
   [(inc x) (inc y)] [(inc x) (dec y)]
   [(dec x) (inc y)] [(dec x) (dec y)]])

(defn segment-points [[start-pos & other :as points]]
  (lazy-seq
   (cond
     (empty? points) nil
     (empty? other) (list start-pos)
     :else (let [[x1 y1] start-pos
                 [x2 y2] (first other)]
             (concat
              (cond
                (= x1 x2) (map #(vector x1 %) (range y1 y2 (signum (core/- y2 y1))))
                (= y1 y2) (map #(vector % y1) (range x1 x2 (signum (core/- x2 x1))))
                :else (let [slope (/ (core/- y2 y1) (core/- x2 x1))]
                        (case (signum (core/- (abs slope) 1))
                          0 (map #(vector %1 %2)
                                 (range x1 x2 (signum (core/- x2 x1)))
                                 (range y1 y2 (signum (core/- y2 y1))))
                          1 (map #(vector (core/+ x1 (-> (/ (* (signum (core/- y2 y1)) %1) slope) round int)) %2)
                                 (range)
                                 (range y1 y2 (signum (core/- y2 y1))))
                          -1 (map #(vector %1 (core/+ y1 (-> (* (* (signum (core/- x2 x1)) %2) slope) round int)))
                                  (range x1 x2 (signum (core/- x2 x1)))
                                  (range))
                          )))
              (segment-points other))))))

(defn draw-points
  ([points] (draw-points \# points))
  ([ink points] (draw-points {} ink points))
  ([paper ink points]
   (reduce
     (fn [paper pos] (assoc paper pos ink))
     paper
     points)))

;; TODO: rename to draw-lines
(defn draw-segments [paper ink points]
  (draw-points paper ink (segment-points points)))

(defn polygon-points [[vertex :as vertices]]
  (rest (segment-points (concat vertices [vertex]))))

(defn draw-polygon [paper ink vertices]
  (draw-points paper ink (polygon-points vertices)))

;; TODO: remove and use draw-segments (it's the same)
(defn draw-segment [paper ink [from to]]
  (draw-points paper ink (segment-points [from to])))

(defn draw-line [paper ink from to]
  (draw-points paper ink (segment-points [from to])))

(defn draw-matrix [paper top-left m]
  (->> (pos-and-values-seq m)
       (map (fn [[pos v]]
              [(+ top-left pos) v]))
       (into paper)))

(defn draw-text [paper top-left text]
  (draw-matrix paper top-left [text]))

(defn draw-box [paper [x1 y1 :as top-left] [x2 y2 :as bottom-right]]
  (-> paper
      (draw-line \━ [x1 y1] [x2 y1])
      (draw-line \━ [x1 y2] [x2 y2])
      (draw-line \┃ [x1 y1] [x1 y2])
      (draw-line \┃ [x2 y1] [x2 y2])
      (assoc [x1 y1] \┏ [x2 y1] \┓
             [x1 y2] \┗ [x2 y2] \┛)))

(defn box [[x1 y1] [x2 y2]]
  [[x1 y1] [x2 y1] [x2 y2] [x1 y2]])

(defn x-and-y-ranges [positions]
  (reduce
    (fn [[[x-min x-max] [y-min y-max]] [x y]]
      [(apply min-max (remove nil? [x-max x-min x]))
       (apply min-max (remove nil? [y-max y-min y]))])
    [[] []]
    positions))

(defn width-and-height [positions]
  (map #(inc (abs (apply core/- %))) (x-and-y-ranges positions)))

(defn relative-to-min "Translate points so that x-min = y-min = 0 "
  [points]
  (let [[[x-min] [y-min]] (x-and-y-ranges points)]
    (map #(- % [x-min y-min]) points)))

(defn outter-box [positions]
  (let [[[x-min x-max] [y-min y-max]] (->> (x-and-y-ranges positions)
                                           (map #(range-expand % 1)))]
    (box [x-min y-min] [x-max y-max])))

(defn find-min-steps-in-maze
  "`wall-positions` should be a collection of wall positions"
  [from to wall-positions]
  (let [walls (set wall-positions)]
    (-> (algo/explore :start from
                      :stop? (last-visited to)
                      :neighbours direct-neighbours
                      :neighbour-allowed? (not (walls neighbour-pos)))
        :nb-steps)))

(defn print-to-matrix
  "`paper is a map [x y] -> value`
  See also [[print-to-lines]]."
  ([paper &{:keys [xy-ranges padding background] :or {, xf identity, padding 0, background \space}}]
   (if (empty? paper)
     nil
     (let [[x-range y-range] (map #(range-expand % padding)
                                  (or xy-ranges (x-and-y-ranges (keys paper))))]
       (mapv (fn [y]
               (->> (mapv (fn [x]
                            (get paper [x y] background))
                          (apply range-inc x-range))))
             (apply range-inc y-range))))))

(defn print-to-lines
  "`paper is a map [x y] -> value`
  `xf` transforms values into printable chars."
  ([paper &{:keys [xy-ranges padding background] :or {, xf identity, padding 0, background \space} :as options}]
   (->> (print-to-matrix paper options)
        (map str/join))))

(defn print [paper &{:keys [xy-ranges padding background] :as options}]
  (run! println (print-to-lines paper options)))

(defn print-maze [wall-positions]
  (print (draw-points \█ wall-positions)))

(defn print-2d-map [rows]
  (doseq [row rows]
    (println (str/join row))))

(defn get-corners [m]
  (let [[x-range y-range] (x-and-y-ranges (keys m))]
    (for [x x-range
          y y-range]
      (m [x y]))))

;; Tests

(deftest segment-points-test
  (are [points expected] (and (= expected (segment-points points))
                              (= (reverse expected) (segment-points (reverse points))))
    [] []
    [[0 0]] [[0 0]]
    [[0 0] [0 1]] [[0 0] [0 1]]
    [[0 0] [0 2]] [[0 0] [0 1] [0 2]]
    [[0 0] [0 2] [2 2]] [[0 0] [0 1] [0 2] [1 2] [2 2]]
    ))

(deftest rotations-test
  (testing "rotate around [0 0]"
    (are [p expected] (and (= expected (rotate-left p))
                           (= p (rotate-right expected)))
      [0 0] [0 0]
      [0 1] [1 0]
      [1 0] [0 -1]
      [0 -1] [-1 0]))
  (testing "rotate around [3 1]"
    (let [center [3 1]]
      (are [p expected] (and (= expected (rotate-left p center))
                             (= p (rotate-right expected center)))
        [0 0] [2 4]
        [4 -1] [1 0]))))

(deftest draw-tests
  (let [ink \█
        from [0 0]]
    (testing "Segment"
      (are [x y expected] (and (= expected
                                  (-> (draw-segment {} ink [from [x y]]) print-to-lines)
                                  (-> (draw-segment {} ink [[x y] from]) print-to-lines))
                               (= (reverse expected)
                                  (print-to-lines (draw-segment {} ink [from [x (core/- y)]]))
                                  (print-to-lines (draw-segment {} ink [[x (core/- y)] from]))))
        0 0 ["█"]
        3 0 ["████"]
        0 3 ["█"
             "█"
             "█"
             "█"]
        3 3 ["█   "
             " █  "
             "  █ "
             "   █"]
        7 3 ["██      "
             "  ██    "
             "    ██  "
             "      ██"]
        3 7 ["█   "
             "█   "
             " █  "
             " █  "
             "  █ "
             "  █ "
             "   █"
             "   █"]))))


(deftest add-test
  (are [in expected] (= expected (apply + in))
       [[-1 3] [2 -5]] [1 -2]
       [[-1 3] [0 0]]  [-1 3]
       [[1 2] [3 4] [5 6]]  [9 12]))

(deftest mult-test
  (are [v n expected] (= expected (mult v n))
    [1 2]  0 [0 0]
    [1 2]  1 [1 2]
    [1 2]  3 [3 6]
    [1 2] -1 [-1 -2]))

(deftest rotate-left-absolute-test
  (are [v rotated] (= rotated (rotate-left v))
    [ 0  0] [ 0  0]
    [ 3 -2] [-2 -3]
    [ 3  2] [ 2 -3]
    [-3  2] [ 2  3]
    [-3 -2] [-2  3]))

(deftest rotate-left-relative-test
  (are [v rotated] (= rotated (rotate-left v [1 1]))
    [ 0  0] [ 0  2]
    [ 3 -2] [-2 -1]
    [ 3  2] [ 2 -1]
    [-3  2] [ 2  5]
    [-3 -2] [-2  5]))

(deftest rotate-right-absolute-test
  (are [rotated v] (= rotated (rotate-right v))
    [ 0  0] [ 0  0]
    [ 3 -2] [-2 -3]
    [ 3  2] [ 2 -3]
    [-3  2] [ 2  3]
    [-3 -2] [-2  3]))

(deftest rotate-left-relative-test
  (are [rotated v] (= rotated (rotate-right v [1 1]))
    [ 0  0] [ 0  2]
    [ 3 -2] [-2 -1]
    [ 3  2] [ 2 -1]
    [-3  2] [ 2  5]
    [-3 -2] [-2  5]))

(deftest flip-vert-absolute-test
  (are [v flipped] (= flipped (flip-vert v))
    [ 0  0] [ 0  0]
    [ 3  0] [-3  0]
    [ 3  3] [-3  3]
    [ 0  3] [ 0  3]
    [-3  3] [ 3  3]
    [-3  0] [ 3  0]
    [-3 -3] [ 3 -3]
    [ 0 -3] [ 0 -3]
    [ 3 -3] [-3 -3]))

(deftest flip-vert-rel-test
  (are [v flipped] (= flipped (flip-vert v [1 2]))
    [ 0  0] [ 2  0]
    [ 3  0] [-1  0]
    [ 3  3] [-1  3]
    [ 0  3] [ 2  3]
    [-3  3] [ 5  3]
    [-3  0] [ 5  0]
    [-3 -3] [ 5 -3]
    [ 0 -3] [ 2 -3]
    [ 3 -3] [-1 -3]))

(deftest flip-horiz-absolute-test
  (are [v flipped] (= flipped (flip-horiz v))
    [ 0  0] [ 0  0]
    [ 3  0] [ 3  0]
    [ 3  3] [ 3 -3]
    [ 0  3] [ 0 -3]
    [-3  3] [-3 -3]
    [-3  0] [-3  0]
    [-3 -3] [-3  3]
    [ 0 -3] [ 0  3]
    [ 3 -3] [ 3  3]))

(deftest flip-horiz-absolute-test
  (are [v flipped] (= flipped (flip-horiz v [1 2]))
    [ 0  0] [ 0  4]
    [ 3  0] [ 3  4]
    [ 3  3] [ 3  1]
    [ 0  3] [ 0  1]
    [-3  3] [-3  1]
    [-3  0] [-3  4]
    [-3 -3] [-3  7]
    [ 0 -3] [ 0  7]
    [ 3 -3] [ 3  7]))

(deftest width-and-height-test
  (are [points expected] (= expected (width-and-height (parse-2d-map-positions points)))
    ["#"] [1 1]
    ["..."
     ".#."
     "..."] [1 1]
    ["...#"
     ".#.."
     "#..."] [4 3]
    ))

(deftest relative-to-min-test
  (are [points expected] (= expected (relative-to-min points))
    [[0 0] [0 1]] [[0 0] [0 1]]
    [[4 5] [2 1]] [[2 4] [0 0]]
    [[4 5] [-2 -1]] [[6 6] [0 0]]))
