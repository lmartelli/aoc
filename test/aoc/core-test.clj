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
  (testing "Creation"
    (is (= {:a [3 1], :b [2]}
           (multimap [[:a 1] [:b 2] [:a 3]])))
    (is (= {:a #{1 2 3 4 5}, :b #{2}}
           (multimap [[:a 1] [:a 5] [:b 2] [:a 3] [:a 2] [:a 4] [:a 1]] #{}))))
  (testing "Inverting"
    (is (= {1 #{1 2}, 2 #{1 4}, 3 #{2 4}, 7 #{3}, 5 #{4}}
           (multimap-invert {1 [1 2], 2 [1 3], 3 [7], 4 [2 3 5]} #{})))))

(deftest array-2d-to-map-test
  (is (= {[0 0] \1, [1 0] \2, [2 0] \3, [0 1] \a, [1 1] \b, [2 1] \c}
         (array-2d-to-map ["123" "abc"])))
  (is (= {[0 0] \1, [2 0] \3, [1 1] \b, [2 1] \c}
         (array-2d-to-map #(not= \space %) ["1 3" " bc"])))
  (is (= {[0 0] :on, [1 0] :off, [2 0] :on, [0 1] :off, [1 1] :on, [2 1] :off}
         (array-2d-to-map #(not= \space %) {\# :on \. :off} ["#.#" ".#."]))))

(deftest split-seq-test
  (are [seq pred expected] (= expected (split-seq pred seq))
    [1 2 3] nil? [[1 2 3]]
    ["a" "b" "" "c" "d"] empty? [["a" "b"] ["c" "d"]]))

;; vector

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
    0 2 [0 1 2]))

(deftest range-inc?-test
  (are [range x expected] (= expected (range-inc? range x))
    [0 0] ##-Inf false
    [0 0] -1 false
    [0 0] 0 true
    [0 0] 1 false
    [0 0] ##Inf false
    [0 1] ##-Inf false
    [0 1] -1 false
    [0 1] 0 true
    [0 1] 1 true
    [0 1] 2 false
    [0 1] ##Inf false
    [0 2] ##-Inf false
    [0 2] -1 false
    [0 2] 0 true
    [0 2] 1 true
    [0 2] 2 true
    [0 2] 3 false
    [0 2] ##Inf false
    ))

(deftest parse-points-test
  (are [s expected] (= expected (parse-points s))
    "" []
    "1 1" [[1 1]]
    "1,2 3,4" [[1 2] [3 4]]))

(deftest group-by-pred-test
  (are [xs pred expected] (= expected (group-by-pred pred xs))
    [] even? [nil nil]
    (range 5) even? [[0 2 4] [1 3]]))

(deftest find-first-test
  (are [coll pred expected] (= expected (find-first pred coll))
    (take-nth 5 (range)) #(> % 10) 15))

(deftest take-until-test
  (are [coll pred expected] (= expected (take-until pred coll))
    [] true? nil
    (range) #(= 3 %) [0 1 2 3]))

(deftest tails-test
  (are [xs expected] (= expected (tails xs))
    [] [[]]
    [1] [[1] []]
    [1 2] [[1 2] [2] []]))

(deftest scatter-test
  (testing "n=1 → collection itself"
    (are [coll] (= [coll] (scatter 1 coll ))
      []
      [1]
      [1 2]
      [1 2 3]))
  (testing "empty coll → n empty colls"
    (are [n expected] (= expected (scatter n []))
      1 [[]]
      2 [[] []]))
  (testing "general case"
    (are [n coll expected] (= expected (scatter n coll))
      2 (range 5) [[0 2 4] [1 3]]
      3 (range 9) [[0 3 6] [1 4 7] [2 5 8]])))

(deftest remove-keys-test
  (are [m pred expected] (= expected (remove-keys pred m))
    {1 :a, 2 :b, 3 :c} odd? {2 :b}
    {1 :a, 2 :b, 3 :c} even? {1 :a, 3 :c}))

(deftest remove-kv-test
  (are [m pred expected] (= expected (remove-kv pred m))
    {1 :a, 2 :b, 3 :c} (fn [k v] (= [k v] [2 :b])) {1 :a, 3 :c}))

(deftest filter-kv-test
  (are [m pred expected] (= expected (filter-kv pred m))
    {1 :a, 2 :b, 3 :c} (fn [k v] (= [k v] [2 :b])) {2 :b}))

(deftest with-default-test
  (let [m (with-default {:a 1} :default)]
    (are [k expected] (= expected (m k))
      :a 1
      :missing :default)))

(deftest split-line-blocks-test
  (are [rows expected] (= expected (split-line-blocks rows))
    ["a"] [["a"]]
    ["a b"] [["a"] ["b"]]
    ["a b"
     "c d"] [["a"
              "c"]
             ["b"
              "d"]]
    ["12 a"
     "34 b"] [["12"
               "34"]
              ["a"
               "b"]]))

(deftest vector-as-map-test
  (are [v m] (= m (vector-as-map v))
    [] {}
    [:a] {0 :a}
    [:a :b] {0 :a, 1 :b}))

