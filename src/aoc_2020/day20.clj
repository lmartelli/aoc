(ns aoc-2020.day20
  (:require
   [aoc.core :refer :all]
   [aoc.matrix :as m]
   [aoc.space-2d :as s2]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (->> (split-seq empty? lines)
       (remove empty?)
       (map (juxt (comp first parse-ints first)
                  (comp vec rest)))
       (into {})))

;; part 1

(def borders
  (memoize
    (fn [tile-data]
      (let [transposed (m/transpose tile-data)]
        {:up (str/join (first tile-data))
         :down (str/join (peek tile-data))
         :left (str/join (first transposed))
         :right (str/join (peek transposed))}))))

(defn print-tile [data]
  (run! println (map str/join data)))

(defn draw-tiles [paper tiles]
  (reduce-kv
    (fn [paper tile-pos {tile-data :data}]
      (-> paper
          (s2/draw-matrix (s2/mult tile-pos 11) tile-data)
          (s2/draw-text (-> tile-pos (s2/mult 11) (s2/+ [2 -1])) (print-str tile-pos))))
    paper
    tiles))

(defn print-assembled [assembled]
  (-> {}
      (draw-tiles  assembled)
      (s2/draw-box [-1 -1] [10 10])
      s2/print))

(def opposite-side {:up :down, :down :up, :left :right, :right :left})

(defn check-assembly [assembled {:keys [pos data]}]
  (doseq [[side f] {:up s2/up :down s2/down :right s2/right :left s2/left}
          :let [ref-pos (f pos)]]
    (when-let [{ref-data :data} (assembled ref-pos)]
      (let [ref-borders (borders ref-data)
            borders (borders data)]
        (when (not= (borders side) (ref-borders (opposite-side side)))
          (println "assembled tile at" pos "has non matching border" side "with" ref-pos)
          (print-tile data)
          (print-assembled assembled)
          (throw (Exception. (print-str "assembled tile at" pos "has non matching border" side "with" ref-pos))))
        )))
  (when (assembled pos)
    (println "assembled position already occupied" pos)
    (print-tile data)
    (print-assembled assembled)
    (throw (Exception. (str "assembled position already occupied" pos)))))

(def cross {:up s2/up, :down s2/down, :left s2/left, :right s2/right})

(defn orientate [{:keys [ref-side side flipped pos] :as tile}]
  (let [base-tx (case [ref-side side]
                  [:right :left ] identity
                  [:right :up   ] m/transpose
                  [:right :right] m/flip-cols
                  [:right :down ] m/rotate-right
                  [:left  :right] identity
                  [:left  :down ] m/transpose
                  [:left  :left ] m/flip-cols
                  [:left  :up   ] m/rotate-right
                  [:down  :up   ] identity
                  [:down  :right] m/rotate-left
                  [:down  :down ] m/flip-rows
                  [:down  :left ] m/transpose
                  [:up    :down ] identity
                  [:up    :left ] m/rotate-left
                  [:up    :up   ] m/flip-rows
                  [:up    :right] m/transpose)
        tx (if (not flipped)
             base-tx
             (case side
               (:up :down)    (comp base-tx m/flip-cols)
               (:left :right) (comp base-tx m/flip-rows)))]
    #_(do
      (println "New tile to assemble at" pos ". Orientate" [ref-side side] "flipped" flipped)
      (print-tile (tile :data))
      (println "==>")
      (print-tile (tx (tile :data))))
    (update tile :data tx)))

(defn assemble
  "Returns a map pos -> {:id :data}"
  [tiles]
  (let [id->borders (map-vals borders tiles) ;; id → {side → border}
        border->tiles (->> (mapcat
                             (fn [[id borders]]
                               (mapcat
                                 (fn [[side border]]
                                   [[border {:id id :side side :flipped false}]
                                    [(str/reverse border) {:id id :side side :flipped true}]])
                                 borders))
                             id->borders)
                           multimap)
        first-tile (zipmap [:id :data] (first tiles))
        neighbours (fn [[pos id] already-assembled]
                     (->> (borders ((already-assembled pos) :data))
                          (keep (fn [[side border]]
                                  (when-let [matched-tile (first (remove (comp (eq id) :id) (border->tiles border)))]
                                    (let [matched-pos ((cross side) pos)]
                                      (when (not (already-assembled matched-pos))
                                        (assoc matched-tile :pos matched-pos :ref-side side :data (tiles (matched-tile :id))))))))))]
    (loop [assembled {[0 0] first-tile}
           last-assembled {[0 0] (first-tile :id)}]
      #_(println "#assembled" (count assembled))
      #_(print-assembled assembled)
      (if (empty? last-assembled)
        assembled
        (let [neighbours (->> (mapcat #(neighbours % assembled) last-assembled)
                              (distinct-by :pos)
                              (map orientate))]
          #_(doseq [tile neighbours]
            (check-assembly assembled tile))
          (recur
            (into assembled (map #(vector(% :pos) (select-keys % [:id :data])) neighbours))
            (map (juxt :pos :id) neighbours)))))))

(defpart part1 [tiles]
  (->> (assemble tiles)
       s2/get-corners
       (map :id)
       (reduce *)))

;; part 2

(def sea-monster
  (s2/parse-2d-map-positions
    ["                  # "
     "#    ##    ##    ###"
     " #  #  #  #  #  #   "]))

(defn finalize-assembly [tiles]
  (->> tiles
       (map-vals m/trim)
       (s2/print-to-matrix)
       (map #(apply m/cat-cols %))
       (apply m/cat-rows)
       s2/parse-2d-map-positions
       set))

(defn find-pattern [image pattern]
  (let [[[x-min x-max] [y-min y-max] :as xy-range] (s2/x-and-y-ranges image)
        image-dims (s2/width-and-height image)]
    (let [all-patterns (reductions
                         #(-> (map %2 %1) s2/relative-to-min)
                         pattern
                         [s2/rotate-left s2/rotate-left  s2/rotate-left s2/flip-horiz s2/rotate-left s2/rotate-left  s2/rotate-left])]
      (->> (for [p all-patterns
                 :let [[width height] (s2/width-and-height p)]
                 x (range-inc x-min (- (inc x-max) width))
                 y (range-inc y-min (- (inc y-max) height))
                 :let [positioned (map #(s2/+ [x y] %) p)]
                 :when (every? image positioned)]
             positioned)))))

(defpart part2 [tiles]
  (let [image (finalize-assembly (map-vals :data (assemble tiles)))
        patterns (find-pattern image sea-monster)]
    (-> {}
        (s2/draw-points \# image)
        (s2/draw-points \O (apply concat patterns))
        s2/print)
    (-> (set/difference image (set (apply concat patterns)))
        count)))

;; tests

(deftest borders-test
  (is (= {:up "...#" :right "##.." :down "###." :left ".#.#" }
         (borders ["...#"
                   "#..#"
                   "...."
                   "###."]))))

(deftest part1-test
  (test-with-file part1 20899048083289))

(deftest part2-test
  (test-with-file part2 273))
