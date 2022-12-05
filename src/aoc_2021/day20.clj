(ns aoc-2021.day20
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(def pixel-to-bit {\. 0 \space 0 \# 1})
(def bit-to-pixel {0 \space 1 \#})

(defn parse-matrix [lines]
  (lines-to-matrix lines pixel-to-bit))

(defn infinite-image [finite-image]
  {:finite-image finite-image
   :width (count (first finite-image))
   :height (count finite-image)
   :out-of-bounds-value 0})

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (split-seq empty?)
       ((fn [[algo image]]
          {:algo (mapv pixel-to-bit (apply concat algo))
           :image (-> (parse-matrix image)
                      infinite-image)}))))

;; part 1

(defn pixel [{:keys [finite-image width height out-of-bounds-value]} [x y :as coord]]
  (if (and (< -1 x width) (< -1 y height))
    (get-in finite-image [y x])
    out-of-bounds-value))

(defn display-image [image]
  (map #(apply str (map bit-to-pixel %)) image))

(defn pixels-around [image [x y]]
  (for [dy (range -1 2)
        dx (range -1 2)]
    (pixel image [(+ x dx) (+ y dy)])))

(defn enhance [algo bits]
  (algo (bits-to-int bits)))

(defn apply-algo [algo image]
  {:finite-image (mapv (fn [y]
                         (mapv (fn [x]
                                 (enhance algo (pixels-around image [x y])))
                               (range-inc -1 (image :height))))
                       (range-inc -1 (image :width)))
   :width (+ 2 (image :width))
   :height (+ 2 (image :height))
   :out-of-bounds-value (enhance algo (repeat 9 (image :out-of-bounds-value)))
   })

(defn iterate-and-count-lit-pixels [algo image n]
  (let [result-image (-> (iterate #(apply-algo algo %) image)
                         (nth n)
                         :finite-image)]
    (->> (matrix-vals result-image)
         (filter #{1})
         count)))

(defpart part1 [{:keys [algo image]}]
  (iterate-and-count-lit-pixels algo image 2))

;; part 2

(defpart part2 [{:keys [algo image]}]
  (iterate-and-count-lit-pixels algo image 50))

;; tests

(def test-data (puzzle-input (test-input)))

(def test-image (test-data :image))

(deftest pixel-test
  (are [x y expected] (= expected (pixel test-image [x y]))
    0  0  1
    1  0  0
    2  0  0
    3  0  1
    0  1  1
    1  1  0
    2  1  0
    3  1  0
    0  2  1
    1  2  1
    2  2  0
    3  2  0
    0  3  0
    1  3  0
    2  3  1
    3  3  0
    -1 -1 0
    -1 0  0
    0  -1 0
    5  0 0
    0  5 0
    5  5 0))

(deftest pixels-around-test
  (are [coord expected] (= expected (pixels-around test-image coord))
    [2 2] [0 0 0, 1 0 0, 0 1 0]
    [2 2] [0 0 0, 1 0 0, 0 1 0]
    [0 0] [0 0 0, 0 1 0, 0 1 0]
    [0 -1] [0 0 0, 0 0 0, 0 1 0]
    [-2 -2] [0 0 0, 0 0 0, 0 0 0]
    [6 -6] [0 0 0, 0 0 0, 0 0 0]))

(deftest apply-algo-test
  (let [iteration-1
        {:finite-image        [" ## ## "
                               "#  # # "
                               "## #  #"
                               "####  #"
                               " #  ## "
                               "  ##  #"
                               "   # # "]
         :width               7
         :height              7
         :out-of-bounds-value 0}
        
        iteration-2
        {:finite-image        ["       # "
                               " #  # #  "
                               "# #   ###"
                               "#   ## # "
                               "#     # #"
                               " # ##### "
                               "  # #####"
                               "   ## ## "
                               "    ###  "]
         :width               9
         :height              9
         :out-of-bounds-value 0}]
    (are [input expected] (= expected (update
                                        (apply-algo (test-data :algo) input)
                                        :finite-image
                                        display-image))
      test-image iteration-1
      (update
        iteration-1
        :finite-image
        parse-matrix)    iteration-2))
  (let [iteration-1
        {:finite-image        [" ## ###"
                               "#  # ##"
                               "## #  #"
                               "####  #"
                               " #  ## "
                               "####  #"
                               "## # # "]
         :width               7
         :height              7
         :out-of-bounds-value 1}]
    (are [input expected] (= expected (update
                                        (apply-algo (-> (test-data :algo)
                                                        (assoc 0 1)
                                                        pop
                                                        (conj 0))
                                                    input)
                                        :finite-image
                                        display-image))
      test-image iteration-1)))

(deftest part1-test (part-test part1 35))

(deftest part2-test (part-test part2 3351))

