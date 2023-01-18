(ns aoc-2018.day22
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [quil.core :as q]
   [quil.middleware :as m]
   [quil.navigation-2d :as n2d]
   [clojure.set :refer :all]
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as stest]
   [clojure.test :refer :all]))

(def-input-parser [[depth target]]
  {:depth (first (parse-ints depth))
   :target (parse-ints target)})

;; part 1

(def mouth [0 0])

(defn printable-map [m]
  (s2/print-to-lines m {:mouth \M :target \T :rock \. :wet \= :narrow \|}))

(defn printable-cave-map [cave-map target]
  (-> cave-map
      (assoc mouth :mouth
             target :target)
      printable-map))

(defn build-erosion-level-map
  ([depth target] (build-erosion-level-map depth target target))
  ([depth target bottom-right]
   (let [geological-index (fn [[x y :as pos] erosion-up erosion-left]
                            (-> (cond
                                  (= target pos) 0
                                  (zero? y) (-> x (* 16807))
                                  (zero? x) (-> y (* 48271))
                                  :else (* erosion-up erosion-left))))
         erosion-level (fn [geological-index depth]
                         (-> geological-index
                             (+ depth)
                             (mod 20183)))]
     (-> (reduce
           (fn [m pos]
             (assoc
               m pos
               (-> (geological-index pos (m (s2/up pos)) (m (s2/left pos)))
                   (erosion-level depth))))
           {}
           (s2/positions-in-rect mouth bottom-right))))))

(defn build-risk-level-map
  ([depth target] (build-risk-level-map depth target target))
  ([depth target bottom-right]
   (-> (build-erosion-level-map depth target bottom-right)
       (update-vals
         (fn [erosion-level]
           (-> erosion-level
               (mod 3)))))))

(defn build-cave-map
  ([{target :target :as input}] (build-cave-map input target))
  ([{:keys [depth target]} bottom-right]
   (-> (build-risk-level-map depth target bottom-right)
       (update-vals {0 :rock 1 :wet 2 :narrow}))))

(defpart part1 [{:keys [depth target]}]
  (->> (build-risk-level-map depth target)
       vals
       (reduce +)))

;; part 2

(def required-tools {:rock #{:climbing-gear :torch}, :wet #{:climbing-gear :neither}, :narrow #{:torch :neither}})

(def change-tool-duration 7)

(defn valid-state? [cave-map [pos tool]]
  (contains? (required-tools (cave-map pos)) tool))

(defn neighbours? [[pos-a] [pos-b]]
  (<= (s2/manatthan-dist pos-a pos-b) 1))

(defn chained-neighbours? [path]
  (every? #(apply neighbours? %) (partition 2 path)))

(s/def ::tool #{:torch :climbing-gear :neither})
(s/def ::pair (s/and seqable? #(= 2 (count %))))
(s/def ::position
  (s/and vector?
         ::pair
         (s/coll-of nat-int?)))
(s/def ::state
  (s/tuple ::position ::tool))

(s/def ::path
  (s/and
    (s/coll-of ::state)
    chained-neighbours?))

(defn valid-state [cave-map]
  (s/and ::state
         (fn [state] (valid-state? cave-map state))))

(defn valid-path [cave-map]
  (s/and (s/coll-of (valid-state cave-map))
         ::path))

(defn neighbours [cave-map [target-pos target-tool :as target]]
  (fn [[pos tool :as state]]
    (cond
      (= state target) {}
      (= pos target-pos) {[pos target-tool] change-tool-duration}
      :else
      (reduce
        (fn [m next-pos]
          (let [next-tools (required-tools (cave-map next-pos))]
            (if (contains? next-tools tool)
              (assoc m [next-pos tool] 1)
              (assoc m [next-pos (first (intersection next-tools (required-tools (cave-map pos))))] (+ change-tool-duration 1)))))
        {}
        (filter cave-map (s2/direct-neighbours pos))))))

(defn estimate-min-cost [[pos tool] [target-pos target-tool]]
  (+ (s2/manatthan-dist pos target-pos)
     (if (= tool target-tool) 0 change-tool-duration)))

(defn min-by-cost [u v]
  (min-key first u v))

(defn build-path [preds to]
  (loop [cur to
         path '()]
    (if (nil? cur)
      path
      (let [[cost prev] (preds cur)]
        (recur
          prev
          (conj path cur))))))

(defn find-min-cost-path [from to cave-map]
  (let [G (neighbours cave-map to)]
    (loop [visited {from [0 nil]}        ;; state -> [cost pred]
           last-visited {from [0 nil]}]  ;; state -> [cost pred]
      (let [[min-cost] (visited to)]
        #_(println "min-cost" min-cost "#visited" (count visited) "/" (count last-visited))
        (if (and min-cost (empty? last-visited))
          {:cost min-cost
           :path (s/assert (valid-path cave-map) (build-path visited to))}
          (let [new-visited (apply
                              merge-with
                              min-by-cost
                              (map
                                (fn [[vertex [cost pred]]]
                                  (update-vals (G vertex) #(vector (+ cost %) vertex)))
                                last-visited))]
            (recur
              (merge-with min-by-cost visited new-visited)
              (filter-kv
                (fn [k [cost pred]]
                  (and (< cost (first (visited k [##Inf])))
                       (or (nil? min-cost)
                           (< (+ cost (estimate-min-cost k to)) min-cost))))
                new-visited))))))))

(s/fdef find-min-cost-path
  :args (s/cat :from ::state :to ::state :cave-map map?)
  :ret (s/keys ::cost ::path))

(stest/instrument `find-min-cost-path)

(defn path-cost [path]
  (->> path
       (partition 2 1)
       (map (fn [[[pos-a tool-a] [pos-b tool-b]]]
              (+ (s2/manatthan-dist pos-a pos-b)
                 (if (= tool-a tool-b) 0 change-tool-duration))))
       (reduce +)))

(def start [mouth :torch])

(defpart part2 [{:keys [depth target] :as input}]
  (-> (find-min-cost-path
        start
        [target :torch]
        (build-cave-map input (s2/mult target 4)))
      :cost))

(defn min-path [{:keys [depth target] :as input} x-max]
  (let [[tx ty] target
        {:keys [path] :as res}
        (find-min-cost-path
          start
          [target :torch]
          (build-cave-map input [x-max (* 2 ty)]))]
    (assoc res :path-cost (path-cost path))))

;; Visualization

(defn point [[x y]]
  (q/rect x y 0 0))

(def tool-colors {:torch [50 70 100]
                  :neither [0 0 100]
                  :climbing-gear [0 80 100]})
(def region-colors {:rock [0 0 50]
                    :wet [240 50 50]
                    :narrow [15 50 50]})
(def bg-color [200 200 200])

(defn draw-path [{:keys [path cave-map x-max y-max] :as state}]
  (apply q/background bg-color)
  (doseq [[pos type] cave-map]
    (apply q/stroke (region-colors type))
    (point pos))
  (doseq [[pos tool] path]
    (apply q/stroke (tool-colors tool))
    (point pos)))

(defpart visualize-path [{:keys [depth target] :as input}]
  (let [cave-map (build-cave-map input (s2/mult target 3))
        path (-> (find-min-cost-path start [target :torch] cave-map) :path)
        setup #(do
                 (q/background 200 200 200)
                 (q/rect-mode :corner)
                 (q/color-mode :hsb 360 100 100)
                 (q/frame-rate 5)
                 {:cave-map cave-map :path path})]
    (q/sketch
      :size (mapv (fn [[min max]] (inc max)) (s2/x-and-y-ranges (map first path)))
      :draw draw-path
      :setup setup
      :middleware [m/fun-mode n2d/navigation-2d]
      :features [:resizable :keep-on-top])))

;; tests

(def test-depth ((test-data) :depth))
(def test-target ((test-data) :target))

(deftest find-min-cost-path-test
  (let [cave-map (build-cave-map (test-data) (s2/+ test-target [5 5]))]
    (testing "Check full path"
      (are [target tool cost path] (= {:cost cost :path path} (find-min-cost-path [mouth :torch] [target tool] cave-map))
        mouth :torch           0 [[[0 0] :torch]]
        mouth :climbing-gear   7 [[[0 0] :torch]
                                  [[0 0] :climbing-gear]]
        [1 0] :neither        10 [[[0 0] :torch]
                                  [[0 1] :torch]
                                  [[1 1] :torch]
                                  [[1 0] :neither]]
        [1 0] :climbing-gear   8 [[[0 0] :torch]
                                  [[1 0] :climbing-gear]]
        [3 0] :neither        12 [[[0 0] :torch]
                                  [[0 1] :torch]
                                  [[1 1] :torch]
                                  [[2 1] :neither]
                                  [[3 1] :neither]
                                  [[3 0] :neither]]
        [0 1] :torch           1 [[[0 0] :torch]
                                  [[0 1] :torch]]
        [0 1] :climbing-gear   8 [[[0 0] :torch]
                                  [[0 1] :torch]
                                  [[0 1] :climbing-gear]]))
      ;[1 1] :torch 2
    (testing "Check path cost"
      (let [{:keys [path cost]} (find-min-cost-path start [test-target :torch] cave-map)]
        (is (= 45 cost (path-cost path)))))))

(deftest build-cave-map-test
  (are [bottom-right expected] (= expected (printable-map (build-cave-map (test-data) bottom-right)))
    [10 10] [".=.|=.|.|=."
               ".|=|=|||..|"
               ".==|....||="
               "=.|....|.=="
               "=|..==...=."
               "=||.=.=||=|"
               "|.=.===|||."
               "|..==||=.|="
               ".=..===..=|"
               ".======|||="
               ".===|=|===."]
    [15 15] [".=.|=.|.|=.|=|=."
             ".|=|=|||..|.=..."
             ".==|....||=..|=="
             "=.|....|.==.|==."
             "=|..==...=.|==.."
             "=||.=.=||=|=..|="
             "|.=.===|||..=..|"
             "|..==||=.|==|==="
             ".=..===..=|.|||."
             ".======|||=|=.|="
             ".===|=|===.===||"
             "=|||...|==..|=.|"
             "=.=|=.=..=.||==|"
             "||=|=...|==.=|=="
               "|=.=||===.|||==="
             "||.|==.|.|.||=||"]))

(deftest part1-test (part-test part1 114))

(deftest part2-test (part-test part2 45))
