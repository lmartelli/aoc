(ns aoc-2020.day04
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.set :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines
    stream
    (fn [line]
      (if (= "" line)
        nil
        (->> (str/split line #" ")
           (map #(let [[field value] (str/split % #":")] [(keyword field) value]))
           (into {}))))
    (merge-lines nil {} merge)))

;; part 1

(defn passport-has-required-fields? [passport]
  (subset? #{:byr :iyr :eyr :hgt :hcl :ecl :pid} (into #{} (keys passport))))

(defpart part1 [input]
  (->> input
       (filter passport-has-required-fields?)
       count))

;; part 2

(defn check-int [str min max]
  (<= min (parse-int str) max))

(defn check-year [value min max]
  (and (re-matches #"\d{4}" value)
       (check-int value min max)))

(defn check-height [value]
  (let [[match height unit] (re-matches #"(\d+)(in|cm)" value)]
    (and match
         (let [h (parse-int height)]
           (case unit
             "cm" (<= 150 h 193)
             "in" (<= 59 h 76))))))

(defn passport-fields-are-valid? [passport]
  (every?
    (fn [[field value]]
      (case field
        :byr (check-year value 1920 2002)
        :iyr (check-year value 2010 2020)
        :eyr (check-year value 2020 2030)
        :hgt (check-height value)
        :hcl (re-matches #"#[0-9a-f]{6}" value)
        :ecl (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} value)
        :pid (re-matches #"\d{9}" value)
        true))
    passport))


(defpart part2 [input]
  (->> input
       (filter passport-has-required-fields?)
       (filter passport-fields-are-valid?)
       count))

;; test

(deftest check-year-test
  (are [value min max] (check-year value min max)
    "1234" 1230 2000
    "1230" 1230 2000
    "2000" 1230 2000)
  (are [value min max] (not (check-year value min max))
    "1229" 1230 2000
    "2001" 1230 2000
    "azer" 1230 2000
    "12345" 0 99999))

(deftest check-height-test
  (are [value] (check-height value)
    "150cm" "180cm" "193cm"
    "59in" "70in" "76in")
  (are [value] (not (check-height value))
    "149cm" "194cm"
    "58in" "77in"
    "99" "2m"))

(deftest passport-fields-are-valid?-test
  (are [fields] (passport-fields-are-valid? fields)
    {:byr "1920"} {:byr "1980"} {:byr "2002"}
    {:iyr "2010"} {:iyr "2015"} {:iyr "2020"}
    {:eyr "2020"} {:eyr "2025"} {:eyr "2030"}
    {:hgt "180cm"} {:hgt "70in"}
    {:hcl "#1234af"}
    {:ecl "amb"} {:ecl "blu"}
    {:pid "012345678"}
    {:byr "1920" :iyr "2010" :eyr "2020" :hgt "180cm" :hcl "#1234af" :ecl "amb" :pid "123456789"}
    {:byr "1920" :iyr "2010" :eyr "2020" :hgt "180cm" :hcl "#1234af" :ecl "amb" :pid "123456789" :cid "toto"}
    {:byr "1920" :iyr "2010" :eyr "2020" :hgt "180cm" :hcl "#1234af" :ecl "amb" :pid "123456789" :XXX "toto"}
    {})
  (are [fields] (not (passport-fields-are-valid? fields))
    {:byr "1919"} {:byr "2003"}
    {:iyr "2009"} {:iyr "2021"}
    {:eyr "2019"} {:eyr "2031"}
    {:hgt "99cm"} {:hgt "170in"}
    {:hcl "#1234a"} {:hcl "#12345x"}
    {:ecl "XXX"}
    {:pid "0123456789"} {:pid "12345678"} {:pid "12345678X"}
    {:byr "1920" :iyr "2010" :eyr "2020" :hgt "180cm" :hcl "#1234af" :ecl "amb" :pid "X"}))
