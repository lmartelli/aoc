(ns aoc-2021.day15
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines stream digit-vec))

;; part 1

(defn grid-dim [grid]
  [(count grid) (count (first grid))])


(defn nil-if-empty [x]
  (if (empty? x) nil x))

(defn dijkstra [risk-levels start end]
  (let [[width height] (grid-dim risk-levels)
        neighbours (fn [[x y]]
                     (->> [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]
                          (filter (fn [[x y]] (and (< -1 x width) (< -1 y height))))))
        find-next (fn [v]
                    (if (empty? v)
                      nil
                      (apply min-key val v)))]
    (loop [v {start 0}
           p {start start}
           done {}]
      #_(println "v=" v "done=" done)
      (if-let [[x v-min] (find-next v)]
        (if (= x end)
          [(v end) p]
          (let [done (assoc done x (v x))
                v (dissoc v x)
                [v p] (reduce
                          (fn [[v p u] y]
                            (if (not (done y))
                              (let [vy (get v y ##Inf)
                                    new (+ (or (v x) (done x)) (get-in risk-levels y))]
                                (if (< new vy)
                                  [(conj v [y new]) (conj p [y x]) (conj u y)]
                                  [v p]))
                              [v p]))
                          [v p]
                          (neighbours x))]
            (recur v p done)))
        [(v end) p]))))

(defn get-path [p start end]
  (loop [path (list end)]
    (let [cur (first path)]
      (if (= cur start)
        path
        (recur (conj path (p cur)))))))

(defn find-min-risk [risk-levels]
  (let [start [0 0]
        end (vec (map dec (grid-dim risk-levels)))
        [v p] (dijkstra risk-levels start end)]
    {:value v
     :path (get-path p start end)}))

(defn display-grid [grid]
  (dorun (map #(println (apply str %)) grid)))

(defn display-path [grid {:keys [:path] :as res}]
  (let [in-path (into #{} path)]
    (-> (map-indexed
          (fn [x row]
            (map-indexed
              (fn [y value]
                (if (in-path [x y]) value \·))
              row))
          grid)
        display-grid)
    (println (->> (map #(get-in grid %) (rest path))
                  (reduce +)))
    res))

(defpart part1 [input]
  (->> (find-min-risk input)
       (display-path input)
       :value))

;; part 2

(defn update-grid [grid f]
  (map #(map f %) grid))

(defn dup-grid [grid n f]
  (->> grid
       (map (fn [row]
              (->> row
                   (iterate #(map f %))
                   (take n)
                   (apply concat))))
       (iterate #(update-grid % f))
       (take n)
       (apply concat)
       (map vec)
       (into [])))

(defpart part2 [input]
  (let [grid (dup-grid input 5 #(-> (mod % 9) (inc)))]
    (->> (find-min-risk grid)
         :path
         rest
         (map #(get-in grid %))
         (reduce +))))

;; tests

(def test-data (puzzle-input (test-input *ns*)))

(deftest part1-test
  (is (= 40 (part1 test-data))))

(deftest part2-test
  (is (= 315 (part2 test-data))))
