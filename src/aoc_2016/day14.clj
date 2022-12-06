(ns aoc-2016.day14
  (:require
   [aoc.core :refer :all]
   [aoc.md5 :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-string stream))

;; part 1

(defn md5-stretch [s stretch]
  (-> (iterate (comp bytes-to-hex md5) s)
      (nth (inc stretch))))

(defn md5-seq [salt stretch]
  (map (fn [index] (-> (str salt index) (md5-stretch stretch))) (range)))

(defn has-3-identical-consecutive-chars? [s]
  (when-let [[_ group] (re-find #"(.)\1\1" s)]
    (first group)))

(defn has-5-consecutive-chars? [s c]
  (re-find (re-pattern (apply str (repeat 5 c))) s))

(defn is-valid-key? [keys]
  (when-let [c (has-3-identical-consecutive-chars? (first keys))]
    (some #(has-5-consecutive-chars? % c) (rest keys))))

(defn index-of-64th-key
  ([salt] (index-of-64th-key salt 0))
  ([salt stretch]
   (let [valid-key-indexes (->> (map-indexed vector (partition 1001 1 (md5-seq salt stretch)))
                                (filter (fn [[index keys]] (is-valid-key? keys)))
                                (map first))]
     (nth valid-key-indexes 63))))

(defpart part1 [input]
  (index-of-64th-key input))

;; part 2

(defpart part2 [input]
  (index-of-64th-key input 2016))

;; tests

(deftest md5-stretch-test
  (are [stretch hash] (= hash (md5-stretch "abc0" stretch))
    0 "577571be4de9dcce85a041ba0410f29f"
    1 "eec80a0c92dc8a0777c619d9bb51e910"
    2 "16062ce768787384c81fe17a7a60c7e3"
    2016 "a107ff634856bb300138cac6568c0f24"))

(deftest md5-seq-test
  (are [index regex] (re-find regex (-> (md5-seq "abc" 0) (nth index)))
    18 #"cc38887a5" 
    39 #"eee"))

(deftest has-3-identical-consecutive-chars?-test
  (are [s c] (= c (has-3-identical-consecutive-chars? s))
    "aaa" \a
    "abcdddzyx" \d)
  (are [s] (not (has-3-identical-consecutive-chars? s))
    "aab"
    "abcdefgg"))

(deftest has-5-consecutive-chars?-test
  (are [s c] (has-5-consecutive-chars? s c)
    "aaaaa" \a
    "abcd!!!!!ddzyx" \!)
  (are [s c] (not (has-5-consecutive-chars? s c))
    "!!!!b" \!
    "aaaaab!cdefg" \!))

(deftest index-of-64th-key-test
  (is (= 22728 (index-of-64th-key "abc")))
  (is (= 22551 (index-of-64th-key "abc" 2016))))
