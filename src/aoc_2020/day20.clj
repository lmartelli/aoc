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
    (fn [rows]
      (let [transposed (m/transpose rows)]
        {:up (str/join (first rows))
         :down (str/join (peek rows))
         :left (str/join (first transposed))
         :right (str/join (peek transposed))}))))

(defn match-borders [ref-tile other-tile]
  (when-let [matched (first
                     (for [[ref-side ref-border] (borders ref-tile)
                           [other-side other-border] (borders other-tile)
                           :when (= ref-border other-border)]
                       (do
                         #_(println "border match" ref-side other-side ref-border)
                         [ref-side other-side])))]
    (let [[dir T]
          (case matched
            [:right :left ] [[1  0] identity]
            [:right :up   ] [[1  0] m/transpose]
            [:right :right] [[1  0] m/flip-cols]
            [:right :down ] [[1  0] m/rotate-right]
            [:left  :right] [[-1 0] identity]
            [:left  :down ] [[-1 0] m/transpose]
            [:left  :left ] [[-1 0] m/flip-cols]
            [:left  :up   ] [[-1 0] m/rotate-right]
            [:down  :up   ] [[0  1] identity]
            [:down  :right] [[0  1] m/rotate-left]
            [:down  :down ] [[0  1] m/flip-rows]
            [:down  :left ] [[0  1] m/transpose]
            [:up    :down ] [[0 -1] identity]
            [:up    :left ] [[0 -1] m/rotate-left]
            [:up    :up   ] [[0 -1] m/flip-rows]
            [:up    :right] [[0 -1] m/transpose])]
      [dir (T other-tile)])))

(defn match-tiles [ref-tile other-tile]
  (->> (map (fn [T] (T other-tile))
            [identity m/flip-rows m/flip-cols])
       (some #(match-borders ref-tile %))))

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

(defn check-assembly [assembled pos data]
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

(defn assemble
  "Returns a map pos -> {:id :data}"
  [tiles]
  ;; Could improve performances by maintaining list of borders
  ;; in already assemble tiles (to avoid trying to match assembled
  ;; tile borders which are no longer free)
  (loop [assembled {[0 0] (zipmap [:id :data] (first tiles))}
         unassembled (dissoc tiles (key (first tiles)))]
    (println "assembled" (count assembled))
    #_(print-assembled assembled)
    (if (empty? unassembled)
      assembled
      (let [[matched-pos matched-tile :as matched]
            (first
              (for [[ref-pos {ref-id :id ref-tile :data}] assembled,
                    [other-id other-tile] unassembled
                    :let [[dir tile :as matched] (match-tiles ref-tile other-tile)]
                    :when matched]
                [(s2/+ ref-pos dir) {:id other-id :data tile}]))]
        #_(println "new match" (matched-tile :id) matched-pos)
        (check-assembly assembled matched-pos (matched-tile :data))
        (recur (conj assembled matched)
               (dissoc unassembled (matched-tile :id)))))))

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

(deftest match-tiles-test
  (are [tiles expected] (= expected (apply match-tiles (m/split-cols tiles)))
    ["#...##.#.. ..###..###"
     "..#.#..#.# ###...#.#."
     ".###....#. ..#....#.."
     "###.##.##. .#.#.#..##"
     ".###.##### ##...#.###"
     ".##.#....# ##.##.###."
     "#...###### ####.#...#"
     ".....#..## #...##..#."
     "#.####...# ##..#....."
     "#.##...##. ..##.#..#."] [[1 0] identity]

    ["#...##.#.. #.##...##."
     "..#.#..#.# ##..#.##.."
     ".###....#. ##.####..."
     "###.##.##. ####.#.#.."
     ".###.##### .#.####..."
     ".##.#....# .##..##.#."
     "#...###### ....#..#.#"
     ".....#..## ..#.#....."
     "#.####...# ####.#...."
     "#.##...##. ...#.#.#.#"] [[0 1] identity]
    ))

(deftest part1-test
  (test-with-file part1 20899048083289))

(deftest part2-test
  (test-with-file part2 273))
