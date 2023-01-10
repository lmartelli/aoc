(ns aoc-2019.day12
  (:require
   [aoc.core :refer :all]
   [aoc.space-3d :as s3]
   [clojure.math.numeric-tower :refer [lcm]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (map parse-ints)))

;; part 1

(defn sign [number]
  (cond (neg? number) -1
        (pos? number) +1
        :else 0))

(defn gravity
  ([pos planets]
   (reduce
    s3/+
    (map #(mapv sign (s3/- % pos)) planets))))

(defn apply-gravity
  ([p planets] (update p :velocity add (gravity (p :pos) (map :pos planets))))
  ([planets] (mapv #(apply-gravity % planets) planets)))

(defn apply-velocity [planets]
  (map (fn [p] (update p :pos add (p :velocity))) planets))

(defn vec-energy [v]
  (reduce + (map abs v)))

(defn planet-energy [p]
  (reduce * (map vec-energy (vals p))))

(defn system-energy [planets]
  (reduce + (map planet-energy planets)))

(defn init-system [input]
  (map #(hash-map :pos %, :velocity [0 0 0]) input))

(defn simulate [system nb-steps]
  (if (= 0 nb-steps)
    system
    (recur
     (-> system apply-gravity apply-velocity)
     (dec nb-steps))))

(defpart part1 [input]
  (system-energy
   (simulate (init-system input) 1000)))

;; part 2

(defn extract-planet-dim [planet dim]
  (mapv #(nth % dim) (vals planet)))

(defn extract-system-dim [system dim]
  (mapv #(extract-planet-dim % dim) system))

(defn extract-system-dims [system]
  (map #(extract-system-dim system %) (range 3)))

(defn find-loops [init]
  (let [start-dims (extract-system-dims init)]
    (loop [system (simulate init 1)
           freqs [nil nil nil]
           iter 1]
      (if (not-any? nil? freqs)
        freqs
        (recur
         (simulate system 1)
         (mapv (fn [freq start cur]
                 (if (nil? freq)
                   (when (= start cur) iter)
                   freq))
               freqs start-dims (extract-system-dims system))
         (inc iter)))
    )))

(defpart part2 [input]
  (reduce lcm (find-loops (init-system input))))

;; tests

(deftest gravity-test
  (are [pos planets expected] (= expected (gravity pos planets))
    [1 2 3] [[1 2 3]] [0 0 0]
    [1 2 3] [[1 2 3] [9 8 7]] [1 1 1]
    [1 2 3] [[-1 2 0]] [-1 0 -1]
    ))

(deftest apply-gravity-1-planet
  (is (= {:pos [1 2 3] :velocity [10 11 12]}
         (apply-gravity {:pos [1 2 3] :velocity [10 11 12]}
                        [{:pos [1 2 3]}]))))

(deftest apply-gravity-all-planets
  (is (= [{:pos [1 2 3] :velocity [20 18 16]}]
         (apply-gravity [{:pos [1 2 3] :velocity [20 18 16]}])))
  (is (= [{:pos [1 2 3] :velocity [12 13 13]}
          {:pos [9 8 7] :velocity [-2 -2 -2]}
          {:pos [3 3 3] :velocity [3 4 6]}]
         (apply-gravity
          [{:pos [1 2 3] :velocity [10 11 12]}
           {:pos [9 8 7] :velocity [0 0 0]}
           {:pos [3 3 3] :velocity [3 4 5]}]))))

(deftest apply-velocity-test
  (is (= [{:pos [1 2 3] :velocity [0 0 0]} {:pos [7 7 7] :velocity [4 5 6]}]
         (apply-velocity [{:pos [1 2 3] :velocity [0 0 0]} {:pos [3 2 1] :velocity [4 5 6]}]))))

(deftest vec-energy-test
  (are [v e] (= e (vec-energy v))
    [0 0 0] 0
    [1 0 0] 1
    [-1 0 0] 1
    [-1 3 4] 8))

(deftest planet-energy-test
  (are [p e] (= e (planet-energy p))
    {:pos [0 0 0] :velocity [0 0 0]} 0
    {:pos [1 2 3] :velocity [4 5 6]} 90
    {:pos [2 1 -3] :velocity [-3 -2 1]} 36
))

(deftest extract-planet-dim-test
  (are [planet dim expected] (= expected (extract-planet-dim planet dim))
    {:pos [1 2 3] :velocity [4 5 6]} 0 [1 4]
    {:pos [1 2 3] :velocity [4 5 6]} 1 [2 5]
    {:pos [1 2 3] :velocity [4 5 6]} 2 [3 6]))
