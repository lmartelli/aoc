(ns aoc-2022.day20
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (map parse-int)
       (into [])))

;; Part 1

(defn move-item [v from to]
  (-> v
      (remove-index from)
      (insert-at to (v from))))

(defn mix [numbers repeat]
  (let [l (count numbers)]
    (as->
      (iterate
        (fn [[positions rev-positions]]
          (reduce
            (fn [[positions rev-positions] [i n]]
              (if (zero? (mod n (dec l)))
                [positions rev-positions]
                (let [from (rev-positions i)
                      to (mod (+ from n) (dec l))
                      new-positions (move-item positions from to)]
                  [new-positions
                   (reduce
                     (fn [rev-positions i]
                       (assoc rev-positions (new-positions i) i))
                     rev-positions
                     (range-inc from to)
                     )
                   ])))
            [positions rev-positions]
            (map-indexed vector numbers)))
        [(into [] (range l))
         (into [] (range l))]) $
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
