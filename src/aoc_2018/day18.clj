(ns aoc-2018.day18
  (:require
   [aoc.core :refer :all]
   [aoc.algo :as algo]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(def char->type {\# :lumberyard,  \| :trees, \. :open})
(def type->char (clojure.set/map-invert char->type))

(def-input-parser [lines]
  (->> lines
       (map #(map char->type %))))

;; part 1

(defn printable-row [row]
  (->> (map type->char row)
       str/join))

(defn printable-state [state]
  (map printable-row state))

(defn print-state [state]
  (println)
  (run! println (printable-state state)))

(defn surround-with [x coll]
  (concat [x] coll [x]))

(defn next-state [state]
  (let [width (count (first state))
        empty-row (repeat (+ 2 width) :open)
        extended-area (surround-with empty-row (map #(surround-with :open %) state))
        neighbourhoods (->> extended-area
                            (partition 3 1)
                            (mapcat (comp #(map frequencies %)
                                          #(map (partial apply concat) %)
                                          #(partition 3 1 %)
                                          #(apply map vector %))))]
    (->> (map
           (fn [acre neighbourhood]
             (case acre
               :open (if (>= (neighbourhood :trees 0) 3) :trees :open)
               :trees (if (>= (neighbourhood :lumberyard 0) 3) :lumberyard :trees)
               :lumberyard (if (and (>= (neighbourhood :lumberyard 0) 2)
                                    (>= (neighbourhood :trees 0) 1))
                             :lumberyard
                             :open)
               ))
           (apply concat state)
           neighbourhoods)
         (partition width))))

(defn states-seq [state]
  (iterate next-state state))

(defn resource-value [state]
  (let [{:keys [trees lumberyard]} (frequencies (flatten state))]
    (* trees lumberyard)))

(defpart part1 [area]
  (-> (states-seq area)
      (nth 10)
      resource-value))

;; part 2

(defpart part2 [area]
  (-> (states-seq area)
      (algo/nth-cycling-iteration 1000000000)
      resource-value))

;; tests

(deftest next-state-test
  (testing "Small configurations"
    (are [in-out]
        (let [[current expected] (scatter 2 in-out)
              next (next-state (parse-input-lines current))]
          (print-state next)
          (= expected (printable-state next)))
      ["|.." "|.." 
       ".#|" ".#|" 
       "..#" "..#"]
      [".|." ".|." 
       "..|" ".||" 
       "|.." "|.."]
      ["||." "||." 
       "..|" "|||" 
       "|.." "|.."]
      ))
  (testing "An open acre will become filled with trees if three or more adjacent acres contained trees"
    (are [in-out]
        (let [[current expected] (scatter 2 in-out)
              next (next-state (parse-input-lines current))]
          #_(print-state next)
          (= expected (printable-state next)))
      [".|." ".|." 
       "..|" ".||" 
       "|.." "|.."]
      ["||." "||." 
       "..|" "|||" 
       "|.." "|.."]))
  
  (testing "Example"
    (let [s0 (test-data)
          states
          (split-line-blocks
            [".......##. .......#.. .......#.. .....|.#.. ....|||#.. ...||||#.. ...||||#.. ..||||##.. ..||###... .||##....."
             "......|### ......|#.. ....|||#.. ...||||#.. ...||||#.. ...||||#.. ..||#|##.. ..|#####.. .||#####.. ||###....."
             ".|..|...#. .|.|||.... .|.||||... .|.#||||.. .|.##||||. .|.###|||. .|.####||. |||#####|. ||##...##. ||##......"
             "..|#||...# ..##|||..# ..###|||.# ..###||||# ..####|||# ..#.##|||# ||#..##||# ||#...##|# ||#....### |##.....##"
             "..##||.|#| ..###|||#| ...##|||#| ...###||#| .|.###||#| |||#.##|#| ||##.##|#| ||##..###| |##....##| |##.....##"
             "...#||||.. ...#|||||. .||##||||| |||##||||| |||###|||| |||###|||| |||####||| ||##.###|| ||##..###| |##....##|"
             "||...|||.. |||||||||. |||||||||| |||||||||| |||||||||| ||||#||||| |||###|||| |||####||| ||######|| ||##.####|"
             "|||||.||.| |||||||||| |||||||||| |||||||||| |||||||||| |||||||||| |||||||||| ||||#||||| |||###|||| ||#####|||"
             "|||||||||| |||||||||| |||||||||| |||||||||| |||||||||| |||||||||| |||||||||| |||||||||| |||||||||| ||||#|||||"
             "....||..|. .||||||||| |||||||||| |||||||||| |||||||||| |||||||||| |||||||||| |||||||||| |||||||||| ||||||||||"])]
      (doseq [[t state] (map-indexed #(vector (inc %1) %2) states)]
        (is (= state (printable-state (nth (states-seq s0) t)))
            (format "State after %d minutes" t))))))

(deftest part1-test (part-test part1 1147))

(deftest part2-test)
