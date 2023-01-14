(ns aoc-2018.day17
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [quil.core :as q]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (let [clay-veins
        (->> lines
             (map #(let [[a b c] (parse-ints %)]
                     (if (= \x (first %))
                       [[a b] [a c]]
                       [[b a] [c a]]))))]
    (s2/draw-points (with-default {} :sand) :clay (mapcat s2/segment-points clay-veins))))

;; part 1

(def spring [500 0])

(def draw-state (atom {}))

(defn flow-horiz
  "Returns updated ground-map and list of new flows"
  ([ground-map [[x y :as pos] :as flow]]
   (let [[l-end l-x :as left] (flow-horiz ground-map pos -1)
         [r-end r-x :as right] (flow-horiz ground-map pos 1)]
     (if (= :wall l-end r-end)
       {:ground-map (s2/draw-segments ground-map :water [[l-x y] [r-x y]])
        :flows [(pop flow)]}
       {:ground-map (s2/draw-segments ground-map :flow [[l-x y] [r-x y]])
        :flows (->> [left right]
                    (keep (fn [[end x-end]]
                            (if (= end :fall)
                              (conj flow [x-end y])))))})))
  ([ground-map [x y] dir]
   (loop [x x]
     (cond
       (= :clay (ground-map [(+ x dir) y])) [:wall x]
       (#{:flow :sand} (ground-map [x (inc y)])) [:fall x]
       :else (recur (+ x dir))))))

(defn flow-down
  "Returns updated ground-map and list of new flows"
  [ground-map flow y-max]
  (loop [ground-map ground-map
         [[x y :as pos] :as flow] flow]
    (if (= y y-max)
      {:ground-map ground-map}
      (let [below-pos (ground-map [x (inc y)])]
        (if (#{:clay :water} below-pos)
          (flow-horiz ground-map flow)
          (recur
            (assoc ground-map [x (inc y)] :flow)
            (conj flow [x (inc y)])))))))

(defn subflow? "Is `a`a subflow of `b` ?"
  [a b]
  (some #(= % (peek a)) b))

(defn merge-flows [flows-a flows-b]
  (reduce
    (fn [res-flows flow]
      (if (some #(subflow? flow %) res-flows)
        res-flows
        (conj res-flows flow)))
    flows-a
    flows-b))

(defn print-state [{:keys [ground-map flows] :as flow-state}]
  (-> ground-map
      (assoc spring \+)
      (s2/draw-points \* (map peek flows))
      (s2/print {:clay \# :water \~ :flow \| :sand \space})))

(defn simul-flow [ground-map y-max spring]
  (loop [{:keys [ground-map flows] :as flow-state} {:ground-map ground-map :flows [(list spring)]}]
    #_(print-state flow-state)
    #_(reset! draw-state flow-state)
    (if (empty? flows)
      ground-map
      (recur
        (reduce
          (fn [{:keys [ground-map flows] :as flow-state} flow]
            (-> (flow-down ground-map flow y-max)
                (update :flows merge-flows flows)))
          {:ground-map ground-map :flows []}
          flows)))))

(defn draw-map [{:keys [ground-map flows] :as state} T]
  (q/with-translation T
    (doseq [[[x y] item] ground-map]
      (apply q/stroke
             (case item
               :water [0 128 255]
               :flow [64 224 208]
               [139 69 19]))
      (q/point x y))
    (q/stroke 0 76 153)
    (apply q/point spring)
    (q/stroke 255 0 0)
    (doseq [[x y] (map peek flows)]
      (q/point x y))))

(defn start-sketch [state [x-range [y-min y-max] :as xy-ranges]]
  (let [padding 2
        y-range [(min y-min (get spring 0)) y-max]
        T (mapv (comp #(+ padding %) - first) [x-range y-range])
        setup (fn  []
                (q/background 255 255 200)
                (q/frame-rate 5))]
    (q/sketch
      :size (mapv (fn [[min max]] (+ (* 2 padding) (- max min))) [x-range y-range])
      :draw #(draw-map @state T)
      :setup setup
      :features [:resizable :keep-on-top])
    state))

(defn count-items [[y-min y-max] pred ground-map]
  (->> ground-map
       (filter (fn [[[x y] item]]
                 (and (<= y-min y y-max)
                      (pred item))))
       count))

(defpart part1 [ground-map]
  (let [[_ [y-min y-max :as y-range] :as xy-ranges] (s2/x-and-y-ranges (keys ground-map))]
    (reset! draw-state {:ground-map ground-map :flows [spring]})
    #_(start-sketch draw-state xy-ranges)
    (->> (simul-flow ground-map y-max spring)
         (count-items y-range #{:water :flow}))))

;; part 2

(defpart part2 [ground-map]
  (let [[_ [y-min y-max :as y-range]] (s2/x-and-y-ranges (keys ground-map))]
    (reset! draw-state {:ground-map ground-map :flows [spring]})
    (->> (simul-flow ground-map y-max spring)
         (count-items y-range #{:water}))))

;; tests

(deftest part1-test (part-test part1 57))

(deftest part2-test (part-test part2 29))
