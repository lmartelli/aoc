(ns aoc-2022.day14
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as space-2d]
   [clojure.test :refer :all]))

(defn parse-points [re s]
  (->> (re-seq re s)
       (map (comp vec
                  #(map parse-int %)
                  rest))))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (map (partial parse-points #"(\d+),(\d+)"))))

;; part 1

(def sand-origin [500 0])

(defn max-depth [rock-structures]
  (->> rock-structures
       (apply concat)
       (map second)
       (apply max)))

(defn draw-rock-structure [cave segments]
  (space-2d/draw-points cave \# (space-2d/segment-points segments)))

(defn draw-map [rock-structures]
  (-> (reduce
        draw-rock-structure
        {}
        rock-structures)))

(defn next-flow [cave pos]
  (->> [[0 1]  ;; down
        [-1 1] ;; down-left
        [1 1]] ;; down-right
       (map #(space-2d/+ pos %))
       (find-first (complement cave))))
  
(defn extend-flow [{:keys [cave flow max-depth] :as state}]
  (if (empty? flow)
    state
    (loop [flow flow]
      (let [[x y :as next-pos] (next-flow cave (peek flow))]
        (cond
          (nil? next-pos) (assoc state :flow flow)
          (> y max-depth) (assoc state :flow flow :stop true)
          :else (recur (conj flow next-pos)))))))

(defn pour-sand [{:keys [cave flow] :as state}]
  (-> state
      (assoc-in [:cave (peek flow)] \o)
      (update :flow pop)))

(defn step [state]
  (if (state :stop)
    nil
    (-> state
        pour-sand
        extend-flow)))

(defn show-state [{:keys [cave flow]}]
  (-> cave
      (space-2d/draw-points \~ flow)
      (assoc sand-origin \+)
      display-grid-map))

(defn init-state [rock-structures]
  (let [cave (draw-map rock-structures)]
    (-> {:cave cave
         :flow [sand-origin]
         :max-depth (max-depth rock-structures)}
        extend-flow)))

(defn simulation [initital-state]
  (->> (iterate step initital-state)
       (take-while some?)))

(defpart part1 [rock-structures]
  (-> (simulation (init-state rock-structures))
      count
      dec))

;; part 2

(defn init-state-2 [rock-structures]
  (let [{max-depth :max-depth :as state} (init-state rock-structures)
        [xo,yo] sand-origin
        floor-depth (+ max-depth 2)
        floor [[(- xo floor-depth 1) ,floor-depth]
               [(+ xo floor-depth 1),floor-depth]]]
    (-> state
        (update :cave space-2d/draw-segments \# floor)
        (update :max-depth + 2))))

(defpart part2 [rock-structures]    
  (->> (iterate step (init-state-2 rock-structures))
       (take-while #(not= \o (get-in % [:cave sand-origin])))
       count))

;; tests

(deftest part1-test (part-test part1 24))

(deftest part2-test (part-test part2 93))
