(ns aoc-2016.day07
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (line-seq stream))

;; part 1

(defn supernets [ip]
  (split ip #"\[[^]]*\]"))

(defn hypernets [ip]
  (re-seq #"(?<=\[)[^]]*(?=\])" ip))

(defn abba-seq
  ([^String string]
   (->> string
        (partition 4 1)
        (filter (fn [[a b c d]] (and (= a d) (= b c) (not= a b))))
        (map #(apply str %))))
  ([s & others]
   (mapcat abba-seq (conj others s))))

(defn abba? [strings]
  (not-empty (apply abba-seq strings)))

(defn tls? [ip]
  (and
   (abba? (supernets ip))
   (not (abba? (hypernets ip)))))

(defpart part1 [input]
  (->> input
       (filter tls?)
       count))

;; part 2

(defn aba-seq
  ([string]
   (->> string
        (partition 3 1)
        (filter (fn [[a b c]] (and (= a c) (not= a b))))
        (map #(apply str %))))
  ([s & others]
   (mapcat aba-seq (conj others s))))

(defn bab [aba]
  (let [a (first aba)
        b (second aba)]
    (str b a b)))

(defn ssl? [ip]
  (let [babs (->> ip
                  hypernets
                  (apply aba-seq)
                  (map bab)
                  set)]
    (some babs (apply aba-seq (supernets ip)))))

(defpart part2 [input]
  (->> input
       (filter ssl?)
       count))

;; tests

(deftest tls?-test
  (are [ip] (tls? ip)
    "abba[mnop]qrst"
    "ioxxoj[asdfgh]zxcvbn")
  (are [ip] (not (tls? ip))
    "abcd[bddb]xyyx"
    "aaaa[qwer]tyui"))

(deftest ssl?-test
  (are [ip] (ssl? ip)
    "aba[bab]xyz"
    "aaa[kek]eke"
    "zazbz[bzb]cdb")
  (are [ip] (not (ssl? ip))
    "xyx[xyx]xyx"))
