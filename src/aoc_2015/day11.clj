(ns aoc-2015.day11
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [join]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (-> stream
      line-seq
      first))

;; part 1

(defn pad [col size pad]
  (apply conj col (repeat (- size (count col)) pad)))

(defn int-to-char [n]
  (char (+ (int \a) n)))

(defn char-to-int [c]
  (- (int c) (int \a)))

(defn int-to-pass [n]
  (loop [letters '() n n]
    (if (zero? n)
      (->> (pad letters 8 \a)
           (apply str))
      (recur
       (conj letters (int-to-char (rem n 26)))
       (quot n 26)))))

(defn pass-to-int [s]
  (first
   (reduce
    (fn [[n mult] c]
      [(+ n (* mult (char-to-int c))) (* mult 26)])
    [0 1]
    (reverse s))))

(def increasing-pattern
  (re-pattern
   (join \|
         (->> (range 26)
              (map int-to-char)
              (partition 3 1)
              (map #(apply str %))))))

(defn valid? [password]
  (and
   (not (re-find #"[iol]" password))
   (re-find increasing-pattern password)
   (-> (re-seq #"(.)\1" password)
       count
       (>= 2))))

(defn next-valid-password [current]
  (->> (iterate inc (pass-to-int current))
       rest
       (map int-to-pass)
       (filter valid?)
       first))

(defpart part1 [input]
  (next-valid-password input))

;; part 2

(defpart part2 [input]
  (-> (iterate next-valid-password input)
      (nth 2)))

;; tests

(deftest valid?-test
  (are [password] (valid? password)
    "abcdffaa"
    "ghjaabcc")
  (are [password] (not (valid? password))
    "hijklmmn"
    "abbceffg"
    "abbcdejk"))
