(ns aoc-2018.day13
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [clojure.test :refer :all]))

(def directions {\^ [0 -1], \v [0 1], \< [-1 0], \> [1 0]})

(defn puzzle-input [stream]
  (let [tracks-and-carts (array-2d-to-map (into #{\\ \/ \+} (keys directions)) (line-seq stream))]
    {:tracks (filter-vals #{\\ \/ \+} tracks-and-carts)
     :carts (->> tracks-and-carts
                 (filter-vals directions)
                 (map-vals (fn [dir] [dir (cycle [:left :straight :right])]))
                 (into (sorted-map-by (fn [[x1 y1] [x2 y2]] (compare [y1 x1] [y2 x2])))))}))

;; part 1

(defn right [dir]
  (case dir
    \> \v
    \< \^
    \^ \>
    \v \<))

(defn left [dir]
  (case dir
    \v \>
    \^ \<
    \> \^
    \< \v))

(defn move-cart [[pos [dir choices]] tracks]
  (let [new-pos (s2/+ pos (directions dir))]
    (case (tracks new-pos)
      \+ [new-pos
          [(({:left left
              :right right
              :straight identity} (first choices)) dir)
           (rest choices)]]
      \\ [new-pos
          [({\^ \<
             \v \>
             \> \v
             \< \^} dir)
           choices]]
      \/ [new-pos
          [({\^ \>
             \v \<
             \> \^
             \< \v} dir)
           choices]]
      [new-pos [dir choices]])))

(defn tick [carts tracks collision-handler]
  (reduce
    (fn [carts [pos :as cart]]
      (let [[[x y :as new-pos] new-data] (move-cart cart tracks)]
        (if (carts new-pos)
          (collision-handler carts pos new-pos)
          (-> carts
              (dissoc pos)
              (assoc new-pos new-data)))))
    carts
    carts))

(defn find-collision [{:keys [carts tracks]}]
  (try 
    (loop [carts carts]
      (recur (tick
               carts
               tracks
               (fn [cart pos new-pos]
                 (throw (ex-info (apply format "Collision at %d,%d " new-pos) {:pos new-pos}))))))
    (catch clojure.lang.ExceptionInfo e
      (-> (ex-data e) :pos))))

(defn print-carts [carts]
  (->> (map (fn [[pos [dir]]] [pos dir]) carts)
       (into {})))

(defn print-carts-and-tracks [carts tracks]
  (s2/print (merge tracks (print-carts carts)))
  (println))

(defn format-pos [[x y]]
  (format "%d,%d" x y))

(defpart part1 [input]
  (let [pos (find-collision input)]
    (format-pos pos)))

;; part 2

(defn tick2 [carts tracks]
  (-> (reduce
        (fn [[carts collisions] [pos :as cart]]
          (let [[[x y :as new-pos] new-data] (move-cart cart tracks)]
            (cond
              (collisions pos) [(dissoc carts pos) collisions]
              (carts new-pos) [(dissoc carts pos new-pos)
                               (conj collisions new-pos)]
              :else [(-> carts
                         (dissoc pos)
                         (assoc new-pos new-data))
                     collisions])))
        [carts #{}]
        carts)
      first))

(defn simul [{:keys [carts tracks]}]
  (iterate #(tick2 % tracks) carts))

(defpart part2 [input]
  (->> (simul input)
       (find-first #(<= (count %) 1))
       first
       key
       format-pos))

;; tests

(deftest part1-test (part-test part1 "7,3"))

(deftest part2-test (part-test part1 "6,4"))
