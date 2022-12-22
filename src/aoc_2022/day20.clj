(ns aoc-2022.day20
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (map parse-int)
       (into [])))

;; Part 1

(defn a-move-item [^ints a from to]
  (let [from-value (aget a from)]
    (if (> to from)
      (loop [i from]
        (when (< i to)
          (aset-int a i (aget a (inc i)))
          (recur (inc i))))
      (loop [i from]
        (when (> i to)
          (aset-int a i (aget a (dec i)))
          (recur (dec i)))))
    (aset-int a to from-value)))

(defn mix [numbers repeat]
  (let [l (count numbers)]
    (as->
      (iterate
        (fn [[^ints positions ^ints rev-positions]]
          (reduce
            (fn [[^ints positions ^ints rev-positions] [i n]]
              (if (zero? (mod n (dec l)))
                [positions rev-positions]
                (let [from (aget rev-positions i)
                      to (mod (+ from n) (dec l))]
                  (a-move-item positions from to)
                  (let [[from to] (min-max from to)]
                    (loop [i from]
                      (when (<= i to)
                        (aset rev-positions (aget positions i) (int i))
                        (recur (inc i)))))
                  [positions rev-positions])))
            [positions rev-positions]
            (map-indexed vector numbers)))
        [(int-array (range l))
         (int-array (range l))]) $
      (nth $ repeat)
      (first $)
      (map numbers $))))

(defn get-coordinates [data]
  (->> (cycle data)
       (drop-while #(not= % 0))
       (drop 1000)
       (take-nth 1000)
       (take 3)
       (reduce +)))

(defpart part1 [input]
  (->> (mix input 1)
       get-coordinates))

;; part 2

(defpart part2 [input]
  (-> (mapv #(* % 811589153) input)
      (mix 10)
      get-coordinates))

;; tests

(deftest part1-test (part-test part1 3))

(deftest part2-test (part-test part2 1623178306))
