(ns aoc-2018.day17
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [quil.core :as q]
   [quil.middleware :as m]
   [quil.navigation-2d :as n2d]
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

(defn flow-horiz
  "Returns updated ground-map and list of new flows"
  ([ground-map flow]
   (let [[x y :as pos] (peek flow)
         [l-end l-x :as left] (flow-horiz ground-map pos -1)
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
         flow flow]
    (let [[x y] (peek flow)]
      (if (= y y-max)
        {:ground-map ground-map}
        (let [below-pos (ground-map [x (inc y)])]
          (if (#{:clay :water} below-pos)
            (flow-horiz ground-map flow)
            (recur
              (assoc ground-map [x (inc y)] :flow)
              (conj flow [x (inc y)]))))))))

(defn subflow? "Is `a`a subflow of `b` ?"
  [a b]
  (some #(= % (peek a)) b))

(defn merge-flows [flows-a flows-b]
  (reduce
    (fn [merged flow]
      (if (some #(subflow? flow %) merged)
        merged
        (conj merged flow)))
    flows-a
    flows-b))

(defn next-state [{:keys [ground-map flows y-max] :as flow-state}]
  (merge
    flow-state
    (reduce
      (fn [{:keys [ground-map flows] :as flow-state} flow]
        (-> (flow-down ground-map flow y-max)
            (update :flows merge-flows flows)))
      {:ground-map ground-map :flows []}
      flows)))

(defn init-state [ground-map spring y-max]
  {:ground-map ground-map
   :flows [[spring]]
   :y-max y-max})

(defn simul-flow [initial-state]
  (->> (iterate next-state initial-state)
       (find-first (comp empty? :flows))))

(defn count-items [[y-min y-max] pred ground-map]
  (->> ground-map
       (filter (fn [[[x y] item]]
                 (and (<= y-min y y-max)
                      (pred item))))
       count))

(defn xy-ranges [ground-map]
  (s2/x-and-y-ranges (keys ground-map)))

(defpart part1 [ground-map]
  (let [[_ [y-min y-max :as y-range] :as xy-ranges] (xy-ranges ground-map)]
    (->> (simul-flow (init-state ground-map spring y-max))
         :ground-map
         (count-items y-range #{:water :flow}))))

;; part 2

(defpart part2 [ground-map]
  (let [[_ [y-min y-max :as y-range]] (xy-ranges ground-map)]
    (->> (simul-flow ground-map y-max spring)
         (count-items y-range #{:water}))))

;; Visualization - console

(defn print-state [{:keys [ground-map flows] :as state}]
  (-> ground-map
      (assoc spring \+)
      (s2/draw-points \* (map peek flows))
      (s2/print {:clay \# :water \~ :flow \| :sand \space}))
  state)

;; Visualization - Quil

(defn point [[x y]]
  (q/rect x y 0 0))

(defn draw-map [{:keys [ground-map flows] :as state} T]
  (q/with-translation T
    (q/background 255 255 200)
    (doseq [[pos item] ground-map]
      (apply q/stroke
             (case item
               :water [0 128 255]
               :flow [64 224 208]
               [139 69 19]))
      (point pos))
    (q/stroke 0 76 153)
    (point spring)
    (q/stroke 255 0 0)
    (doseq [flow (map peek flows)]
      (point flow))))

(defpart visualize-flow [ground-map]
  (let [[x-range [y-min y-max :as y-range] :as xy-ranges] (xy-ranges ground-map)
        padding 2
        y-range [(min y-min (get spring 0)) y-max]
        T (mapv (comp #(+ padding %) - first) [x-range y-range])
        setup #(do
                 (q/background 255 255 200)
                 (q/rect-mode :corner)
                 (q/frame-rate 10)
                 (init-state ground-map spring y-max))]
    (q/sketch
      :size (mapv (fn [[min max]] (+ (* 2 padding) (- max min))) [x-range y-range])
      :draw #(draw-map % T)
      :update next-state
      :setup setup
      :middleware [m/fun-mode n2d/navigation-2d]
      :features [:resizable :keep-on-top])))

;; tests

(deftest part1-test (part-test part1 57))

(deftest part2-test (part-test part2 29))
