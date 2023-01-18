(ns aoc.space-2d
  (:refer-clojure :exclude [+ -])
  (:require
   [clojure.core :as core]
   [aoc.core :refer [signum min-max range-inc]]
   [aoc.algo :as algo]
   [clojure.math.numeric-tower :refer [round sqrt]]
   [clojure.string :as str]
   [clojure.test :refer :all]))

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
  "returns positions of `char` in a 2D map made of rows"
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

(defn box [[x1 y1] [x2 y2]]
  [[x1 y1] [x2 y1] [x2 y2] [x1 y2]])

(defn x-and-y-ranges [positions]
  (reduce
    (fn [[[x-min x-max] [y-min y-max]] [x y]]
      [(apply min-max (remove nil? [x-max x-min x]))
       (apply min-max (remove nil? [y-max y-min y]))])
    [[] []]
    positions))

(defn outter-box [positions]
  (let [[[x-min x-max] [y-min y-max]] (x-and-y-ranges positions)]
    (box [(dec x-min) (dec y-min)] [(inc x-max) (inc y-max)])))

(defn find-min-steps-in-maze
  "`wall-positions` should be a collection of wall positions"
  [from to wall-positions]
  (let [walls (set wall-positions)]
    (-> (algo/explore :start from
                      :stop? (last-visited to)
                      :neighbours direct-neighbours
                      :neighbour-allowed? (not (walls neighbour-pos)))
        :nb-steps)))

(defn print-to-lines
  ([paper] (print-to-lines paper identity))
  ([paper xf]
   (let [positions (keys paper)
         [x-range y-range] (x-and-y-ranges positions)]
     (print-to-lines paper xf x-range y-range)))
  ([paper x-range y-range]
   (print-to-lines paper identity x-range y-range))
  ([paper xf x-range y-range]
   (map (fn [y] (->> (map (fn [x] (if-let [c (paper [x y])] (or (xf c) c) \space)) (apply range-inc x-range))
                     str/join))
        (apply range-inc y-range))))

(defn print [paper & args]
  (run! println (apply print-to-lines paper args)))

(defn print-maze [wall-positions]
  (print (draw-points \█ wall-positions)))

(defn print-2d-map [rows]
  (doseq [row rows]
    (println (str/join row))))

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
             "   █"]
        ))))


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
