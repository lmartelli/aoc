(ns aoc.algo
  (:require
   [aoc.algo :refer :all]
   [aoc.space-2d :as s2]
   [clojure.test :refer :all]))

;; Tests

(deftest resolve-bijection-test
  (testing "Bijection can be deduced"
    (are [input expected] (= expected (resolve-bijection input))
      {1 [2]} {1 2}
      {1 [:a], 2 [:a :b], 3 [:a :c]} {1 :a, 2 :b, 3 :c}))
  (testing "Bijection cannot be deduced"
    (is (nil? (resolve-bijection {1 [:a :b], 2 [:a :b]})))))

(deftest find-min-test
  (are [start] (= [36 1296] (find-min-parameter start #(* % %) #(>= % 1234)))
    1 2 3 4 5 6 7))

(deftest adjacency-graph-test
  (testing "2D map with 4 direct neighbours"
    (are [map graph] (= (map-vals set graph)
                        (map-vals set (adjacency-graph (set (s2/parse-2d-map-positions map)) s2/direct-neighbours)))
      ["#"]
      {}
      ["##"]
      {[0 0] [[1 0]], [1 0] [[0 0]]}
      ["###"]
      {[0 0] [[1 0]], [1 0] [[0 0] [2 0]], [2 0] [[1 0]]}
      ["##"
       " #"]
      {[0 0] [[1 0]], [1 0] [[0 0] [1 1]], [1 1] [[1 0]]})))

(defn parse-char-matrix [m]
  (mapv #(mapv (comp symbol str) %) m))

(deftest weighted-graph-test
  (are [map nodes expected] (= expected (weighted-graph (-> map
                                                            parse-char-matrix
                                                            (make-adjacency-graph (symbol " "))) nodes))
    ["ab"] '[a b] '{a {b 1}, b {a 1}}
    ["abc"] '[a c] '{a {c 2}, c {a 2}}
    ["ab  "
     "ef h"
     "ij l"
     "mnop"] '[a h e o] '{a {e 1, o 5}
                          e {a 1, o 4}
                          h {o 3}
                          o {h 3, a 5, e 4}}
    ))

(deftest make-adjacency-graph-test
  (testing "Matrix with any hole"
    (are [rows graph] (= graph (make-adjacency-graph rows))
      '[(a)] {}
      '[(a b)] '{a (b), b (a)}
      '[(a b c)] '{a (b), b (a c), c (b)}
      '[(a b)
        (c d)] '{a (b c), b (a d), c (a d), d (b c)}
      '[(a b c)
        (d e f)
        (g h i)] '{a (b d), b (a c e), c (b f), d (a e g), e (b d f h), f (c e i), g (d h), h (e g i), i (f h)}))
  (testing "Ragged rows"
    (are [rows graph] (= graph (make-adjacency-graph rows))
      '[(a b c)
        (d e)
        (g h i)] '{a (b d), b (a c e), c (b), d (a e g), e (b d h), g (d h), h (e g i), i (h)}))
  (testing "Using default nil-vertice"
    (are [rows graph] (= graph (make-adjacency-graph rows))
      '[(a  b  c)
        (d nil f)
        (g  h  i)] '{a (b d), b (a c), c (b f), d (a g), f (c i), g (d h), h (g i), i (f h)}))
  (testing "Using custom nil-vertice"
    (are [rows graph] (= graph (make-adjacency-graph rows '_))
      '[(a b c)
        (d _ f)
        (g h i)] '{a (b d), b (a c), c (b f), d (a g), f (c i), g (d h), h (g i), i (f h)})))

(deftest bfs-path-test
  (let [graph (make-adjacency-graph ["abcdef"
                                     "gh jkl"
                                     "mnop  "
                                     "stuv x"] \space)]

    (testing "Returns nil if no destination is reachable"
      (are [start destinations]
          (nil? (bfs-path :start start :destinations destinations :neighbours graph))
        \a '(\z)
        \a '(\x)))

    (testing "Choose 1st destination in the list"
      (are [start destinations expected]
          (= expected (bfs-path :start start :destinations destinations :neighbours graph))
        \a '(\b \c) '(\a \b)))

    (testing "Single path"
      (are [start destinations expected]
          (= expected (bfs-path :start start :destinations destinations :neighbours graph))
        \a '(\a) '(\a)
        \a '(\b) '(\a \b)
        \a '(\c) '(\a \b \c)))

    (testing "Use choose-position if multiple directions are available"
      (are [start destinations expected]
          (= expected (bfs-path :start start :destinations destinations :neighbours graph :choose-position :min))
        \a '(\h) '(\a \b \h)))

    (testing "Choose 1st destination in the list"
      (are [start destinations expected]
          (= expected (bfs-path :start start :destinations destinations :neighbours graph))
        \a '(\b \c) '(\a \b)))
    ))
