(ns aoc.core-test
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(deftest replace-chars-test
  (are [s m expected] (= expected (replace-chars s m))
    "abcde" {} "abcde"
    "abcde" {\a \A, \b \B} "ABcde"))

(deftest parse-aoc-ns-name-test
  (are [name year day] (= [year day] (parse-aoc-ns-name name))
    "aoc-123.day0042" "123" "42" 
    "aoc-2018.day01" "2018" "1" 
    "aoc-2019.day23" "2019" "23"))

(deftest puzzle-input-uri-test
  (is (= "https://adventofcode.com/2015/day/3/input"
         (puzzle-input-uri "2015" "3"))))

(defn with-system-properties [props f]
  (with-redefs-fn
    {#'system-get-property props}
    f))

(deftest expand-home-test
  (with-system-properties {"user.home" "/home/user"}
    #(is (= "/home/user/rel/path/to/file.txt"
            (expand-home "~/rel/path/to/file.txt")))))

(deftest puzzle-input-filename-test
  (with-redefs [aoc-dir "/aoc"]
    (is (= "/aoc/puzzle-input-2017-09.txt"
           (puzzle-input-filename "2017" "9")))))

(deftest multimap-test
  (is (= {:a '(3 1), :b '(2)}
         (multimap [[:a 1] [:b 2] [:a 3]]))))

(deftest array-2d-to-map-test
  (is (= {[0 0] \1, [1 0] \2, [2 0] \3, [0 1] \a, [1 1] \b, [2 1] \c}
         (array-2d-to-map ["123" "abc"])))
  (is (= {[0 0] \1, [2 0] \3, [1 1] \b, [2 1] \c}
         (array-2d-to-map #(not= \space %) ["1 3" " bc"])))
  (is (= {[0 0] :on, [1 0] :off, [2 0] :on, [0 1] :off, [1 1] :on, [2 1] :off}
         (array-2d-to-map #(not= \space %) {\# :on \. :off} ["#.#" ".#."]))))

(deftest split-seq-test
  (are [seq pred expected] (= expected (split-seq seq pred))
    [1 2 3] nil? [[1 2 3]]
    ["a" "b" "" "c" "d"] empty? [["a" "b"] ["c" "d"]]))

;; vector

(deftest add-test
  (are [in expected] (= expected (apply add in))
       [[-1 3] [2 -5]] [1 -2]
       [[-1 3] [0 0]]  [-1 3]
       [[1 2] [3 4] [5 6]]  [9 12]))

(deftest mult-test
  (are [v n expected] (= expected (mult v n))
    [0 1 2 3 -4]  0 [0 0 0 0 0]
    [0 1 2 3 -4]  1 [0 1 2 3 -4]
    [0 1 2 3 -4]  3 [0 3 6 9 -12]
    [0 1 2 3 -4] -1 [0 -1 -2 -3 4]))

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

(deftest shift-right-test
  (are [v] (= v (shift-right v 0) (shift-right v (count v)))
    [] [0] [0 1] [0 1 2 3])
  (are [v expected] (= expected (shift-right v 1) (shift-right v (+ 1 (count v))))
    [0] [0]
    [0 1] [1 0]
    [0 1 2 3] [3 0 1 2])
  (are [v expected] (= expected (shift-right v 2) (shift-right v (+ 2 (count v))))
    [0] [0]
    [0 1 2 3] [2 3 0 1]))

(deftest insert-at-test
  (are [v pos value expected] (= expected (insert-at v pos value))
    [] 0 :x [:x]
    [0] 0 :x [:x 0]
    [0] 1 :x [0 :x]
    [0 1 2] 1 :x [0 :x 1 2]
    [0 1 2] 2 :x [0 1 :x 2]
    [0 1 2] 3 :x [0 1 2 :x]))

(deftest map-vals-test
  (is (= {:a 1 :b 2 :c 3} (map-vals inc {:a 0 :b 1 :c 2}))))

(deftest filter-vals-test
  (is (= {:b 1} (filter-vals odd? {:a 0 :b 1 :c 2}))))

(deftest transpose-test
  (are [colls transposed] (= transposed (transpose colls))
    [[1] [2]] [[1 2]]
    [[1 2] [3 4]] [[1 3] [2 4]]))

(deftest range-inc-test
  (are [from to expected] (and (= expected (range-inc from to))
                               (= expected (range-inc to from)))
    0 0 [0]
    0 1 [0 1]
    0 2 [0 1 2]
    ))
