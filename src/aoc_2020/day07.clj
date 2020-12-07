(ns aoc-2020.day07
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.test :refer :all]))

(defn parse-bag-list [str]
  (if (= "no other bags" str)
    {}
    (->> (str/split str  #", ")
         (map #(let [[_ cnt color] (re-matches #"(\d+) (.*) bags?" %)] [color (parse-int cnt)]))
         (into {}))))

(puzzle-input-parse-lines
  (fn [line]
    (let [[_ container l] (re-matches #"(.*) bags contain (.*)\." line)]
      [container (parse-bag-list l)]))
  #(into {} %))

;; part 1

(defn reverse-bag-rules [bag-rules]
  (->> bag-rules
       (mapcat
         (fn [[container contained]] (map #(vector % container) (keys contained))))
       multimap))

(defn find-containers-of [color rev-rules]
  (loop [result #{}
         contained #{color}]
    (let [containers (mapcat rev-rules contained)]
      (if (empty? containers)
        result
        (recur (into result containers) (set/difference (set containers) result))))))

(defpart part1 [input]
  (count (find-containers-of "shiny gold" (reverse-bag-rules input))))

;; part 2

(defn count-contained-bags [bag rules]
  (->> (map (fn [[contained-bag n]] (* n (count-contained-bags contained-bag rules))) (rules bag))
       (reduce +)
       inc))

(defpart part2 [input]
  (dec (count-contained-bags "shiny gold" input)))

;; test

(def test-input
  {"shiny gold" {"dark red" 2}
   "dark red" {"dark orange" 2}
   "dark orange" {"dark yellow" 2}
   "dark yellow" {"dark green" 2}
   "dark green" {"dark blue" 2}
   "dark blue" {"dark violet" 2}
   "dark violet" {}})
