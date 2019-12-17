(ns aoc.core-test
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(deftest replace-chars-test
  (are [s m expected] (= expected (replace-chars s m))
    "abcde" {} "abcde"
    "abcde" {\a \A, \b \B} "ABcde"))

(deftest puzzle-input-resource-path-test
  (is (= "a_b/c_d.txt" (puzzle-input-resource-path (create-ns 'a-b.c_d)))))

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


(deftest add-test
  (are [in expected] (= expected (apply add in))
       [[-1 3] [2 -5]] [1 -2]
       [[-1 3] [0 0]]  [-1 3]
       [[1 2] [3 4] [5 6]]  [9 12]))
