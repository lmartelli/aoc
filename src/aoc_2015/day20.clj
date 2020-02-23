(ns aoc-2015.day20
  (:require
   [aoc.core :refer :all]
   [aoc.prime :refer :all]
   [clojure.math.combinatorics :as combi :refer [combinations]]
   [clojure.math.numeric-tower :refer [exact-integer-sqrt]]))

(puzzle-input-string parse-int)

;; part 1

(defn dividers [prime-factors]
  (let [factors (expand-bag prime-factors)]
    (->> (range-inc 1 (count factors))
         (mapcat #(combinations factors %))
         (map #(apply * %))
         (into (sorted-set 1)))))

(defn next-divider [prime-factors]
  (loop [n 2]
    (if-not (contains? (dividers prime-factors) n)
      n
      (recur (inc n)))))

(defn next-local-max [prime-factors] ;; {prime-factor count, ...}
  (let [next (next-divider prime-factors)]
    (if-let [first-factor (find-first #(zero? (rem next %)) (keys prime-factors))]
      (update prime-factors first-factor inc)
      (assoc prime-factors next 1))))

(defn ceil-multiple
  "The smallest multiple of m >= n."
  [n m]
  (let [r (rem n m)]
    (if (zero? r)
      n
      (+ n (- m (rem n m))))))

(defn multiple? [x y]
  (zero? (mod x y)))

(defn presents-delivered [house-num]
  (reduce
   (fn [nb-presents elf-num]
     (if (multiple? house-num elf-num)
       (+ nb-presents
          (* 10 (let [q (/ house-num elf-num)]
                  (if (= q elf-num)
                    elf-num
                    (+ elf-num q)))))
       nb-presents))
   0
   (range-inc 1 (first (exact-integer-sqrt house-num)))))

(defn upper-bound-house-num
  ([nb-delivered-presents]
   (->> (map #(upper-bound-house-num nb-delivered-presents %) (range))
        (find-last some?)))
  ([nb-delivered-presents level]
   (let [factors (nth (iterate next-local-max {}) level)
         sum-dividers (apply + (dividers factors))
         mult-factors (apply * (expand-bag factors))]
     (let [res
           (ceil-multiple
            (-> (/ nb-delivered-presents 10)
                (- sum-dividers)
                (* mult-factors)
                (/ sum-dividers)
                int)
            mult-factors)]
       (when (>= res (* mult-factors mult-factors))
         res)))))

(defn multiples [n]
  (iterate #(+ % n) n))

(defn multiples-in-range
  ([n min]
   (iterate #(+ % n) (ceil-multiple min n)))
  ([n min max]
   (->> (multiples-in-range n min)
        (take-while #(<= % max)))))

(defn aupdate-long [^longs a k f x]
  (aset-long a k (f (aget a k) x))
  a)

(defn elf-delivery [house-min house-max houses elf-num]
  (reduce
   (fn [houses house-num]
     (aupdate-long houses house-num + (* elf-num 10)))
   houses
   (multiples-in-range elf-num house-min house-max)))

(defn deliver-presents [min-house max-house elf-delivery]
  (reduce (partial elf-delivery min-house max-house)
          (long-array (inc max-house))
          (range-inc 1 (count houses))))

(defn lower-bound-house-num [min-presents]
  (->> (iterate next-local-max {})
       (map expand-bag)
       (map #(apply * %))
       (map #(vector % (presents-delivered %)))
       (take-while #(< (second %) min-presents))
       last
       first))

(defn find-house-with-min-presents [min-presents elf-delivery]
  (let [start (lower-bound-house-num min-presents)]
    (->> (deliver-presents start
                           (upper-bound-house-num min-presents)
                           elf-delivery)
         (drop start)
         (map vector (iterate inc start))
         (find-first
          (fn [[house-num nb-present]]
            (> nb-present min-presents))))))

(defpart part1 [input]
  (first 
   (find-house-with-min-presents input elf-delivery)))

;; part 2

(defn elf-delivery2 [houses elf-num]
  (reduce
   (fn [houses house-num]
     (update! houses house-num + (* elf-num 11)))
   houses
   (->> (multiples elf-num)
        (take 50)
        (take-while #(<= % (count houses))))))

(defpart part2 [input]
  (find-house-with-min-presents input elf-delivery2))

;; tests
